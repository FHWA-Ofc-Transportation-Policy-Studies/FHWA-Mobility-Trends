import snowflake.connector
from sqlalchemy import create_engine
import getpass
from urllib.parse import quote
from datetime import datetime
import os
os.supports_bytes_environ = True
import rsa

def authenticate_snowflake(account, role, user_name=None, password=None, keep_alive=False):

    """
    Authenticate user into a Snowflake project. Users are prompted to enter their credentials for the corresponding Snowflake account. Credentials are encrypted after input and stored as bytes as an environment variable to inject in the necessary fields. See https://docs.snowflake.com/en/user-guide/python-connector-example.html for more information on Snowflake connections.

    Parameters:
    --------
        account: str
            The account URL associated with the Snowflake project. Ex: if the URL for the Snowflake project is "https://ilovepuppies.us-east.gcp.snowflakecomputing.com", then the account string would be "ilovepuppies.us-east.gcp."
        role: str
            The name of the role the logged in user will assume.
        user_name: str, default is None.
            The user name of the user authenticating to the Snowflake project.
        password: str, default is None.
            The password of the user authenticating to the Snowflake project.
        keep_alive: bool, default is False
            When True, connection established with Snowflake will not timeout due to inactivity. Use ctx.close() to close a connection that is kept alive
    
    Returns:
    --------
        ctx: snowflake.connector.Cursor
            An authenticated Snowflake Cursor object.
    """
    global PRIVATE_KEY
    global USER_NAME
    global PASSWORD
    global ACCOUNT
    global ROLE

    if keep_alive == False:
        PUBLIC_KEY, PRIVATE_KEY = rsa.newkeys(1024)
        ACCOUNT = account
        ROLE = role
        if user_name is None:
            USER_NAME = rsa.encrypt(bytearray(input("\nEnter Snowflake username:\n"), encoding='utf-8'), PUBLIC_KEY)
        else:
            USER_NAME = rsa.encrypt(user_name.encode('utf-8'), PUBLIC_KEY)
        if password is None:
            print("\nEnter Snowflake password:")
            PASSWORD = rsa.encrypt(bytearray(getpass.getpass(), encoding='utf-8'), PUBLIC_KEY)
        else:
            PASSWORD = rsa.encrypt(password.encode('utf-8'), PUBLIC_KEY)

    # Create Snowflake connection
    ctx = snowflake.connector.connect(
        user = rsa.decrypt(USER_NAME, PRIVATE_KEY).decode(),
        password = rsa.decrypt(PASSWORD, PRIVATE_KEY).decode(),
        account = account,
        client_session_keep_alive = keep_alive
    )
    
    ctx.cursor().execute(f"USE ROLE {role}")
    if keep_alive == False:
        print(f'\nSuccessfully connected user {ctx.cursor().execute("select current_user();").fetchall()[0][0]} to Snowflake project {account} using role {role} at {datetime.now().strftime("%Y-%m-%d %H:%M:%S")}.\n')
    else:
        print(f'Connection to {account} will be kept alive for this operation.\n')

    return ctx

def authenticate_sql_alchemy(warehouse, database, schema):
    
    """
    Creates a connection to Snowflake via SQLAlchemy. See https://docs.snowflake.com/en/user-guide/sqlalchemy.html#installing-snowflake-sqlalchemy for more information.
    
    Parameters:
    -----------
        warehouse: str
            A Snowflake warehouse.
        database: str
            A Snowflake database within the warehouse.
        schema: str
            A Snowflake schema within the database.
        
    Returns:
    --------
        engine: sqlalchemy.engine.base.Engine
            An authenticated Snowflake connection.
    """

    engine = create_engine(
            'snowflake://{user}:{password}@{account_identifier}/{database_name}/{schema_name}?warehouse={warehouse_name}&role={role_name}'.format( 
                user = rsa.decrypt(USER_NAME, PRIVATE_KEY).decode(),
                password = quote(rsa.decrypt(PASSWORD, PRIVATE_KEY).decode()),
                account_identifier = ACCOUNT,
                database_name = database,
                schema_name = schema,
                warehouse_name = warehouse,
                role_name = ROLE
            )
        )
    
    return engine
