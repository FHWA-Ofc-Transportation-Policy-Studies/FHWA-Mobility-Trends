import re
import numpy as np
import os
from platforms.connect import exceptions
import glob
from datetime import datetime
import math

def log_helper(message, log_df=None):
    """
    Logs output of Snowflake commands in log.txt when log parameter is set to True in various functions.

    Parameters:
    -----------
        message: str
            The text to be written to log txt.
        log_df: DataFrame, default is None.
            When not None, writes a DataFrame to log.txt.
    
    Returns:
    --------
        None
    """

    with open('log.txt', 'a') as f:
        now = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
        f.write(f'\n{now}\n')
        f.write(message)
        f.close()
    
    try:
        if not log_df.empty:
            log_df.to_csv('log.txt', sep=' ', mode='a')
    except:
        pass
    
    return

def sanitize(*args):
    if any(';' in x for x in (args)):
        raise ValueError('String arguments may not contain semi-colons.')

def orient_snowflake(ctx, warehouse, database, schema, table, append):
    """
    Checks the existence of specified Snowlfake warehouse, database, and schema in the Snowflake instance that is being used. Makes specified infrastructure if it does not exist and the user has the permissions to do so.
    
    Parameters:
    -----------
        ctx: snowflake.connector.Cursor 
            An authenticated Snowflake Cursor object.
        warehouse: str
            The name of the Snowflake warehouse.
        database: str
            The name of the Snowflake database.
        schema: str
            The name of the Snowflake schema.
        table: str
            The name of the Snowflake table.
        append: bool
            Flag used to check for Snowflake table existence or lack thereof and raise the appropriate error. 

    Returns:
    --------
        None
    """
    
    global db
    global sch
    global tbl
    
    # Check Snowflake infrastructure and user permissions
    wh = warehouse.upper()
    db = database.upper()
    sch = schema.upper()
    tbl = table.upper()

    # Sanitize
    sanitize(wh, db, sch, tbl)
        
    # Get oriented in Snowflake
    try:
        ctx.cursor().execute(f'USE WAREHOUSE {wh};')
    except:
        print(f"Warehouse {wh} does not exist - creating new warehouse...")
        try:
            ctx.cursor().execute(f'CREATE WAREHOUSE {wh};')
            print(f'Successfully created warehouse {wh}\n')
        except:
            raise exceptions.PermissionsError(w_error=True)

    try:
        ctx.cursor().execute(f'USE DATABASE {db};')
    except:
        print(f"Database {db} does not exist - creating new database...")
        try:
            ctx.cursor().execute(f'CREATE DATABASE {db};')
            print(f'Successfully created database {db}\n')
        except:
            raise exceptions.PermissionsError(d_error=True)

    try:
        ctx.cursor().execute(f'USE SCHEMA {sch};')      
    except:
        print(f"Schema {db}.{sch} does not exist - creating new schema...")
        try:
            ctx.cursor().execute(f'CREATE SCHEMA {sch};')
            print(f'Successfully created schema {db}.{sch}\n')
        except:
            raise exceptions.PermissionsError(s_error=True)     
        
    # Get all tables in the schema and operate apporpriately via if_exists
    table_names = ctx.cursor().execute("show tables").fetchall()
    table_names = [x[1] for x in table_names]
    
    if tbl in table_names and append == False:
        raise exceptions.TableExistsError(db, sch, tbl)
    elif tbl not in table_names and append == True:
        raise exceptions.TableDoesNotExistError(db, sch, tbl)
    
    return

def set_pk(ctx, primary_key, log, force=False):
    """
    Creates or checks for primary key in the provided Snowflake table.

    Parameters:
    -----------
        ctx: snowflake.connector.Cursor
            An authenticated Snowflake Cursor object.
        primary_key: st
            The column in the DataFrame to use as the primary key in the Snowflake table.
        log: bool
            When True, outputs information about the Snowflake environment and the Snowflake commands run to log.txt file.

    Returns:
    --------
        primary_key: str
            Primary key that has newly been set for the specified table.
        pk: str
            Existing primary key set for specified table.
    """
    
    if force:
        return primary_key
    
    try:
        primary_key = primary_key.upper()
    except:
        raise exceptions.PrimaryKeyError(db, sch, tbl, primary_key, None, not_found=True)
        
    # Try to create a new primary key
    try:
        ctx.cursor().execute(f'alter table {tbl} add primary key("{primary_key}");')
        message = f'Primary key successfully set to {primary_key} for table {db}.{sch}.{tbl}\n'
        print(message)
        if log:
            log_helper(message)
        return primary_key
    
    # Check if a primary key exists and if the one specified matches the one set for the table
    except:
        try:
            pk = ctx.cursor().execute(f'SHOW PRIMARY KEYS IN "{db}"."{sch}"."{tbl}";').fetchall()
            pk = ctx.cursor().execute('SELECT * FROM table(RESULT_SCAN( LAST_QUERY_ID() ));').fetch_pandas_all()
            try:
                pk = pk.column_name.iloc[0]
            except:
                raise exceptions.PrimaryKeyError(db, sch, tbl, primary_key, pk, not_found=True)
            if pk.upper() != primary_key.upper():
                raise exceptions.PrimaryKeyError(db, sch, tbl, primary_key, pk, incorrect=True)
        except:
            raise
            
    return pk

def setup_updates(df, ctx):
        """
        Used to prepare for modify_columns function by getting information on the DataFrame and Snowflake table columns for later comparison for add_columns and drop_columns arguments.

        Parameters:
        -----------
            df: DataFrame
                A pandas DataFrame.
            ctx: snowflake.connector.Cursor
                An authenticated Snowflake Cursor object.

        Returns:
        --------
            snow_cols: list
                Names of the columns in the Snowflake table.
            snow_cols_lower: list
                Lower-case names of the columns in the Snowflake table.
            df_cols: list
                Names of the columns in the DataFrame.
            df_cols_lower: list
                Lower-case names of the columns in the DataFrame.
            snow_dtypes: dict
                A dictionary of column names and the corresponding Snowflake data type derived from its native pandas data type.
        """

        # Get column names in Snowflake and the df to check column disparities
        snow_cols = ctx.cursor().execute(f'show columns in "{tbl}";').fetchall()
        snow_cols = [x[2] for x in snow_cols]
        snow_cols_lower = [x.lower() for x in snow_cols]
        df_cols = df.columns.tolist()
        df_cols_lower = [s.lower() for s in df_cols]

        # When the columns are the same
        if set(snow_cols_lower) == set(df_cols_lower):
            raise exceptions.ModifyColumnsError(db, sch, tbl)

        # Build dictionary of df columns and their corresponding Snowflake data types
        snow_dtypes = convert_cols(df)
        
        return snow_cols, snow_cols_lower, df_cols, df_cols_lower, snow_dtypes

def update_helper(df, ctx, pk, log):
    """
    Extracts DataFrame information and performs MERGE statement, comparing a temporary table of the provided DataFrame to the provided Snowflake table.

    Parameters:
    -----------
        df: DataFrame
            A pandas DataFrame.
        ctx: snowflake.connector.Cursor
            An authenticated Snowflake Cursor object.
        pk: str
            Primary key for specified Snowflake table.
        log: bool
            When True, outputs information about the Snowflake environment and the Snowflake commands run to log.txt file.

    Returns:
    --------
        df: DataFrame
            A pandas DataFrame of the specified Snowflake table. 
    """

    # Extract df info to for column comparisons and MERGE statement
    df_cols = df.columns.tolist()
    # columns_list_query = f'({(",".join(df_cols))})'
    # sr_columns_list = [f'Source.{i}' for i in df_cols]
    # sr_columns_list_query = f'({(",".join(sr_columns_list))})'
    up_columns_list = [f'{i}=Source.{i}' for i in df_cols]
    up_columns_list_query = f'{",".join(up_columns_list)}'
    # rows_to_insert = [row.tolist() for idx, row in df.iterrows()]
    # rows_to_insert = str(rows_to_insert).replace('[', '(').replace(']', ')')[1:][:-1]
    
    # Perform MERGE statement i.e., UPSERT
    # https://stackoverflow.com/questions/59679280/duplicate-row-detected-during-dml-action-snowflake-talend
    # https://community.snowflake.com/s/question/0D50Z00007cevglSAA/is-there-a-way-to-define-the-fileformat-of-a-staged-file-when-using-it-in-a-merge-operation
    ctx.cursor().execute("ALTER SESSION SET QUOTED_IDENTIFIERS_IGNORE_CASE = TRUE;") # This must remain true for the merge to work for some reason
    query = f'MERGE INTO "{tbl}" as Target \
                USING (SELECT * FROM TEMP_TABLE \
                    ) AS Source \
                    ON Target.{pk}=Source.{pk} \
                    WHEN MATCHED THEN \
                        UPDATE SET {up_columns_list_query};'

                        # qualify \
                        # row_number() over ( \
                        #     partition by \
                        #         {pk} \
                        #     order by \
                        #         {pk} desc nulls last \
                        # ) = 1 \

                    # WHEN NOT MATCHED THEN \
                        # INSERT {columns_list_query} \
                        # VALUES {sr_columns_list_query} \
                        
    ctx.cursor().execute(query)
    log_df = ctx.cursor().execute('SELECT * FROM table(RESULT_SCAN( LAST_QUERY_ID() ));').fetch_pandas_all()
    print(f'Successfully updated {log_df["number of rows updated"].iloc[0]} row(s) in table {db}.{sch}.{tbl}\n')
    
    if log:
        message = f'Viewing merge results for table {tbl}...\n'
        log_helper(message, log_df)

    # Reset ignore_case to false
    ctx.cursor().execute("ALTER SESSION SET QUOTED_IDENTIFIERS_IGNORE_CASE = FALSE;")
    ctx.cursor().execute(f"DROP TABLE IF EXISTS temp_table;")
    
    df = ctx.cursor().execute(f'SELECT * FROM "{tbl}";').fetch_pandas_all().reset_index(drop=True)
    
    return df

def internal_staging(ctx, log):
    """
    Creates a Snowflake .csv FILE FORMAT and internal stage in the database specified in the orient_snowflake function. File format and stage used for internal staing methods employed in other functions.

    Parameters:
    -----------
        ctx: snowflake.connector.Cursor
            An authenticated Snowflake Cursor object.
        log: bool
            When True, outputs information about the Snowflake environment and the Snowflake commands run to log.txt file.

    Returns:
    --------
        None    
    """

    global file_format
    global stage

    file_format = 'csv_file_format'
    stage = 'csv_stage'

    # Create file format https://docs.snowflake.com/en/user-guide/data-load-internal-tutorial.html
    query = f"CREATE FILE FORMAT IF NOT EXISTS {file_format} \
                    type = 'CSV' \
                    field_delimiter = ',' \
                    record_delimiter = '\n' \
                    skip_header = 1 \
                    null_if = ('0000-00-00', '', 'None', 'NaN', 'NULL', 'null', 'NaT') \
                    empty_field_as_null = true \
                    field_optionally_enclosed_by = '0x22' \
                    date_format = 'AUTO';"
    ctx.cursor().execute(query)

    if log:
        message = 'File format creation logged to log.txt'
        log_df = ctx.cursor().execute('SELECT * FROM table(RESULT_SCAN( LAST_QUERY_ID() ));').fetch_pandas_all()
        log_helper(message, log_df)

    # Create stage for csv
    query = f"CREATE OR REPLACE STAGE {stage} file_format = {file_format};"
    ctx.cursor().execute(query)

    # log
    if log:
        message = 'Staging creation logged to log.txt'
        log_df = ctx.cursor().execute('SELECT * FROM table(RESULT_SCAN( LAST_QUERY_ID() ));').fetch_pandas_all()
        log_helper(message, log_df)

    return

def put_copy_files(df, ctx, log, chunksize=None, parallel=None, modify_columns=False):
    """
    Saves .csv(s) of the provided DataFrame to PUT the file into a Snowflake internal stage and COPY the file into a temporary table.

    Parameters:
    -----------
        df: DataFrame
            A pandas DataFrame.
        ctx: snowflake.connector.Cursor
            An authenticated Snowflake Cursor object.
        log: bool
            When True, outputs information about the Snowflake environment and the Snowflake commands run to log.txt file.
        chunksize: int, default is None
            The number of rows from the df to write to Snowflake at once.
        parallel: int, default is None
            Specifies the number of threads to use for uploading files. The upload process separate batches of data files by size. Small files (< 64 MB compressed or uncompressed) are staged in parallel as individual files. Larger files are automatically split into chunks, staged concurrently, and reassembled in the target stage. A single thread can upload multiple chunks. Increasing the number of threads can improve performance when uploading large files. Supported values: Any integer value from 1 (no parallelism) to 99 (use 99 threads for uploading files).
            
    Returns:
    --------
        None
    """

    # Create file format and stage
    internal_staging(ctx, log)

    # Create temp folder
    abspath_curr = os.getcwd()
    directory = os.path.join(abspath_curr, 'temp')
    if not os.path.exists(directory):
        os.makedirs(directory)

    # Write df to disk in chunks
    if chunksize != None:
        print(f'Large transfer initiated...\n')
        start = datetime.now()
        print(f'Time at start: {start.strftime("%Y-%m-%d %H:%M:%S")}\n')
        print(f'Writing DataFrame in {chunksize} row chunks to {os.path.abspath(directory)}...\n')
        
        if log:
            message = f'Large transfer initiated...\nTime at start: {start.strftime("%Y-%m-%d %H:%M:%S")}\nWriting DataFrame in {chunksize} row chunks to {os.path.abspath(directory)}...\n'
            log_helper(message)

        count = 0
        rows = 0
        while rows < len(df):
            try:
                file_name = f'{tbl + "_" + str(count)}' if count >= 10 else f'{tbl + "_0" + str(count)}'
                path = os.path.join(directory, file_name)
                if count != 0:
                    if rows+chunksize < len(df):
                        df[rows:rows+chunksize].to_csv(path+'.csv', index=False, line_terminator='\n')
                    else:
                        df[rows:len(df)].to_csv(path+'.csv', index=False, line_terminator='\n')
                else:
                    df[:chunksize].to_csv(path+'.csv', index=False, line_terminator='\n')
            except:
                raise
                
            count += 1
            rows = (chunksize * count)
            message = f'Successfully wrote {rows}/{len(df)} rows to disk' if rows < len(df) else f'Successfully wrote {len(df)}/{len(df)} rows disk\n'
            print(message)
            if log:
                log_helper(message)


        # PUT file into internal stage
        message = f'Putting files from disk into internal Snowflake stage {stage} in {db}.{sch}...\n'
        print(message)
        if log:
            log_helper(message)

        result = ctx.cursor().execute(f"PUT file://{directory}/*.csv @{stage} parallel = {parallel};")

        message = f'Copying files from internal Snowflake stage {stage} in {db}.{sch} to table {tbl}...\n'
        print(message)
        if log:
            log_helper(message)

        # Copy file into temp_table
        query = f"COPY INTO {tbl} FROM @{stage} \
                    FILE_FORMAT = (format_name = {file_format}) \
                    PATTERN = '.*csv.gz' \
                    ON_ERROR = 'SKIP_FILE_10' \
                    PURGE = TRUE;"
        ctx.cursor().execute(query)

        log_df = ctx.cursor().execute('SELECT * FROM TABLE(RESULT_SCAN( LAST_QUERY_ID() ));').fetch_pandas_all()

        if log:
            message = f'Viewing results of copying DataFrame data into {tbl}...\n'
            log_helper(message, log_df)

        end = datetime.now()
        print(f'Time at end: {end.strftime("%Y-%m-%d %H:%M:%S")}\n')
        print(f'Total transfer time: {end-start}\n')

        if log:
            message = f'Time at end: {end.strftime("%Y-%m-%d %H:%M:%S")}\nTotal transfer time: {end-start}\n'
            log_helper(message)
        
        # Remove files
        filelist = glob.glob(os.path.join(os.getcwd(), 'temp', f"{tbl}*.csv"))
        for f in filelist:
            os.remove(f)

    else:
        file_name = f'{tbl}.csv'
        file_path = os.path.join(directory, file_name)
        df.to_csv(file_path, index=False, line_terminator='\n')

        if modify_columns==False:
            # Need to track column order from Snowflake and conform df to it
            cols = ctx.cursor().execute(f'SHOW COLUMNS IN {tbl};').fetchall()
            cols = [x[2] for x in cols]
            df_copy = df.copy()
            df_copy.columns = df_copy.columns.str.upper()
            df_copy = df_copy.reindex(columns=cols)

            # Write file
            df_copy.to_csv(file_path, index=False, line_terminator='\n')
            
            # Remove temp table if it exists and create new
            ctx.cursor().execute("DROP TABLE IF EXISTS TEMP_TABLE;")
            ctx.cursor().execute(f'CREATE TABLE TEMP_TABLE LIKE {tbl};')

        # PUT file into internal stage 
        result = ctx.cursor().execute(f"PUT file://{file_path} @{stage};").fetchall()
        temp_file = result[0][1]

        # Remove files
        filelist = glob.glob(os.path.join(os.getcwd(), 'temp', f"{tbl}*.csv"))
        for f in filelist:
            os.remove(f)

        # COPY file into temp_table
        query = f"COPY INTO TEMP_TABLE FROM @{stage}/{temp_file} file_format = (format_name = {file_format});"
        ctx.cursor().execute(query)

        if log:
            log_df = ctx.cursor().execute('SELECT * FROM TABLE(RESULT_SCAN( LAST_QUERY_ID() ));').fetch_pandas_all()
            message = f'Viewing results of copying DataFrame data into TEMP_TABLE for staging into {tbl}...\n'
            log_helper(message, log_df)
    
    return

def fix_date_cols(df, tz='UTC'):
        """
        Converts columns with pandas datetime data types to UTC to conform to Snowflake's datetime requirements.

        Parameters:
        -----------
            df: DataFrame
                A pandas DataFrame.
            tz: str, default is 'UTC'
                The timezone the datetime column should be converted to.

        Returns:
        --------
            df_copy: DataFrame
                A copy of the df parameter where the date columns have been updated to be in the UTC timezone.
        """

        # Convert datetimes to UTC (this could be long if working with wide dataframes)
        # Rationale: https://github.com/snowflakedb/snowflake-connector-python/issues/319#issuecomment-764145625
        df_copy = df.copy()
        cols = df_copy.select_dtypes(include=['datetime64', 'datetimetz']).columns

        for col in cols:
            try:
                df_copy[col] = df_copy[col].dt.tz_localize(tz)
            except:
                pass

        return df_copy

def convert_cols(df):
    """
    Creates a dictionary of pandas DataFrame columns and their corresponding Snowflake data types.

    Parameters:
    -----------
        df: DataFrame
            A pandas DataFrame.

    Returns:
    --------
        snow_dtypes: dict
            A dictionary of column names and the corresponding Snowflake data type derived from its native pandas data type.
    """

    # Convert datetimes to UTC
    df = fix_date_cols(df)

    # Define pandas data types
    df_dtypes = list(df.dtypes.values)
    df_dtypes = [str(x).replace('dtype(', '').replace(')', '') for x in df_dtypes]
    df_cols = df.columns.tolist()
    col_dtype = {df_cols[i]: df_dtypes[i] for i in range(len(df_cols))}

    # Create dictionary of pandas columns and their corresponding Snowflake data types
    snow_dtypes = {k: 'varchar(16777216)' if v == 'object' else 'float' if re.search("float.*", v) != None and len(str(math.trunc(np.nanmax(df[k].values)))) > len(str(math.trunc(np.nanmin(df[k].values)))) else 'float' if re.search("float.*", v) != None else 'integer' if re.search("float.*", v) != None and math.isnan(np.nanmax(df[k])) == True and math.isnan(np.nanmin(df[k])) == True else 'timestamp_ntz' if re.search('datetime.*', v) else 'boolean' if v == 'bool' else 'integer' if re.search("int.*", v) != None else "variant" for k, v in col_dtype.items()}
    
    return snow_dtypes
