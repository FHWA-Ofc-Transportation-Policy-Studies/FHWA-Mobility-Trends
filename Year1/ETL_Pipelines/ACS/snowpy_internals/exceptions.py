from platforms.connect import helper

class AuthenticationError(Exception):
    """
    Exception raised when user has not authenticated to a Snowflake project and attempts to use functions expecting a connection.

    Parameters:
    -----------
        None
    """

    def __init__(self):
        self.message = f'No Snowflake connection found. Please authenticate to a Snowflake project before proceeding.'
        super().__init__(self.message)

class TableExistsError(Exception):
    """
    Exception raised when the user attempts to create a table that already exists.

    Parameters:
    -----------
        database: str
            The name of the Snowflake database.
        schema: str
            The name of the Snowflake schema.
        table: str
            The name of the Snowflake table.
    """

    def __init__(self, database, schema, table):
        self.database = database 
        self.schema = self.database + '.' + schema
        self.table =  self.schema + '.' + table
        self.message = f"Cannot create new table. Table {self.table} already exists in {self.schema}."
        super().__init__(self.message)
        
class TableDoesNotExistError(Exception):
    """
    Exception raised when the user attempts to modify rows in a table that does not exist.

    Parameters:
    -----------
        database: str
            The name of the Snowflake database.
        schema: str
            The name of the Snowflake schema.
        table: str
            The name of the Snowflake table.
    """

    def __init__(self, database, schema, table):
        self.database = database 
        self.schema = self.database + '.' + schema
        self.table =  self.schema + '.' + table
        self.message = f"Cannot modify a table that does not exist. Table {self.table} does not exists in {self.schema}."
        super().__init__(self.message)
        
class PermissionsError(Exception):
    """
    Exception raised when Snowflake infrastructure does not exist and the user does not have the permissions to create such infrastructure.

    Parameters:
    -----------
        w_error: bool, default is False
            When True, sets the message parameter to inform the user about Snowflakes warehouses.
        d_error: bool, default is False
            When True, sets the message parameter to inform the user about Snowflakes databases.
        s_error: bool, default is False
            When True, sets the message parameter to inform the user about Snowflakes schemas.
    """

    def __init__(self, w_error=False, d_error=False, s_error=False):
        self.w_error = w_error
        self.d_error = d_error 
        self.s_error = s_error
        
        if self.w_error:
            self.message = "User does not have proper authorities to create a warehouse. Please either use an existing warehouse or have your administrator create one for you."
            
        if self.d_error:
            self.message = "User does not have proper authorities to create a database. Please either use an existing database or have your administrator create one for you."
            
        if self.s_error:
            self.message = "User does not have proper authorities to create a schema. Please either use an existing schema or have your administrator create one for you."
        
        super().__init__(self.message)

class CreateTableError(Exception):
    """
    Exception raised when there is a problem when creating a new table.

    Parameters:
    -----------
        ctx: snowflake.connector.Cursor 
            An authenticated Snowflake Cursor object.
        database: str
            The name of the Snowflake database.
        schema: str
            The name of the Snowflake schema.
        table: str
            The name of the Snowflake table.
        stage: str
            The name of the Snowflake stage.
        log: bool
            When True, outputs information about the Snowflake environment and the Snowflake commands run to log.txt file.
        large_transfer: bool
            When True, creates a new table with name from the `table` argument writes DataFrame in chunks specified by the `chunksize` argument to the disk, puts files in Snowflake internal stage, and copies files from the stage into the specified Snowflake table. 
"""

    def __init__(self, ctx, database, schema, table, log, large_transfer, stage=None):
        self.ctx = ctx
        self.database = database 
        self.schema = self.database + '.' + schema
        self.table =  self.schema + '.' + table
        self.stage = self.schema + stage if stage != None else stage
        self.log = log
        self.large_transfer = large_transfer

        if self.large_transfer:
            query_id = ctx.cursor().execute('SELECT last_query_id();').fetchall()
            ctx.cursor().execute(f"DROP TABLE IF EXISTS {self.table};")
            self.message = f'Failed to copy some files into table {self.table} from stage {self.stage}. Run command "SELECT * FROM TABLE(RESULT_SCAN({"".join(query_id[0])}))" to view results of the copy. Any residual data from {self.table} have been dropped from {self.schema}.\n'
            if log:
                helper.log_helper(self.message)
            
        else:
            ctx.cursor().execute(f"DROP TABLE IF EXISTS {self.table};")
            self.message = f"An error occurred while creating table {self.table}. Any residual data from {self.table} have been dropped from {self.schema}.\n"
            if log:
                helper.log_helper(self.message)

        super().__init__(self.message)
        
class PrimaryKeyError(Exception):
    """
    Exception raised when the provided primary key is not found or does not match what is set for the Snowflake table.

    Parameters:
    -----------
        database: str
            The name of the Snowflake database.
        schema: str
            The name of the Snowflake schema.
        table: str
            The name of the Snowflake table.
        primary_key_in: str
            The primary key argument provided by the user.
        primary_key_snow: str
            The primary key that is set for the specified Snowflake table.
        not_found: bool, default is False
            When True, sets the message parameter to inform the user that the provided primary key is not set for the specified Snowflake table.
        incorrect: bool, default is False
            When True, sets the message parameter to inform the user that the provided primary key does not match what is set for the Snowflake table.
    """

    def __init__(self, database, schema, table, primary_key_in, primary_key_snow, not_found=False, incorrect=False):
        self.database = database 
        self.schema = self.database + '.' + schema
        self.table =  self.schema + '.' + table
        self.primary_key_in = primary_key_in
        self.primary_key_snow = primary_key_snow
        self.not_found = not_found 
        self.incorrect = incorrect 
    
        if self.not_found and primary_key_snow==None:
            self.message = f'A primary key is required for this operation. Set primary key for {self.table} before proceeding.\nEither set the "primary_key" argument for this function or set the primary key in Snowflake for this table with statement: "alter table {self.table} add primary key <primary_key_column>".\nAlternativley, set argument force=True to use {self.primary_key_in} as a stand-in primary key, though this may cause undeterministic results.'

        if self.incorrect:
            self.message = f'Primary key {self.primary_key_in} does not match primary key {self.primary_key_snow} set for table {self.table}.\nPlease use the correct primary key passed or set a new one in Snowflake for this table with statement: "alter table {self.table} add primary key <primary_key_column>".\nAlternativley, set argument force=True to use {self.primary_key_in} as a stand-in primary key, though this may cause undeterministic results.'

        super().__init__(self.message)

class DisparateColumnsError(Exception):
    """
        Exception raised when the user attempts to call SnowPy.modify_rows() using a DataFrame whose columns do not match that of the desired Snowflake table. 

    Parameters:
    -----------
        df_cols: list
            List of column names from the DataFrame.
        snow_cols: list
            List of column names from the Snowflake table.
        database: str
            The name of the Snowflake database.
        schema: str
            The name of the Snowflake schema.
        table: str
            The name of the Snowflake table.
    """

    def __init__(self, df_cols, snow_cols, database, schema, table):
        self.df_cols = df_cols
        self.snow_cols = snow_cols
        self.database = database 
        self.schema = self.database + '.' + schema
        self.table =  self.schema + '.' + table
        self.extra_df_cols = list(set(self.df_cols) - set(self.snow_cols))
        self.extra_snow_cols = list(set(self.snow_cols) - set(self.df_cols))
        self.message = f'Cannot add rows when there are disparate column(s) between the DataFrame and Snowflake table.\n'
        if len(self.extra_df_cols) > 0 and len(self.extra_snow_cols) > 0:
            self.message += f'Column(s) {self.extra_snow_cols} in Snowflake table {self.table} do not exist in DataFrame.\nColumn(s) {self.extra_df_cols} in DataFrame do not exist in Snowflake table {self.table}.\n'
        elif len(self.extra_df_cols) > 0: 
            self.message += f'Column(s) {self.extra_df_cols} in DataFrame do not exist in Snowflake table {self.table}.\n'
        elif len(self.extra_snow_cols) > 0: 
            self.message += f'Column(s) {self.extra_snow_cols} in Snowflake table {self.table} do not exist in DataFrame.\n'

        super().__init__(self.message)

class ModifyColumnsError(Exception):
    """
    Exception raised when the user attempts to call SnowPy.modify_columns() and there are no differences between the DataFrame and the Snowflake table.
    
    Parameters:
    -----------
        database: str
            The name of the Snowflake database.
        schema: str
            The name of the Snowflake schema.
        table: str
            The name of the Snowflake table.
    """

    def __init__(self, database, schema, table):
        self.database = database 
        self.schema = self.database + '.' + schema
        self.table =  self.schema + '.' + table
        self.message = f'No column differences found between provided DataFrame and table {self.table}'

        super().__init__(self.message)
