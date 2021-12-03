import inspect
import logging
import mysql.connector as connection
import sqlite3
from sqlite3 import Error


def connect_2_lambo_db(database: str, host: str = "localhost", user: str = "root", passwd: str = "root"):
    """

    :param passwd: The credentials to access the database.
    :param user: The credentials to access the database.
    :param host: provides the hostName of MySQL server.
                 Normally, if we do install in our machine locally then it termed as ‘localhost’.
                 Cases like cloud / dedicated third party server provide the IP address there.
    :param database: provides the name of the database to do manipulation.

    :return: connection
    """

    logging.info(f'Fun. {inspect.stack()[0][3]}')

    conn = None

    try:
        # conn = connection.connect(host=host, database=database, user=user, passwd=passwd, use_pure=True)
        conn = sqlite3.connect(database)
    except Error as e:
        print(f"Error while connecting to DB: {str(e)}")
        if conn:
            conn.close()
    except Exception as e:
        print(f"Error while connecting to DB: {str(e)}")
        if conn:
            conn.close()

    return conn


def close_lambo_db(conn):
    """
    Close a DB connection

    :param conn: Connection object

    :return: None
    """

    logging.info(f'Fun. {inspect.stack()[0][3]}')

    # Close Connection
    conn.close()


def test_connection(conn):
    """
    Test the connection printing DB tables' number

    :param conn: Connection object

    :return: True if connection successfully established, False otherwise
    """

    logging.info(f'Fun. {inspect.stack()[0][3]}')

    print(f"Testing connection:")

    # Instantiate Cursor
    cur = conn.cursor()

    # test
    if count_tables(cur):
        return True
    else:
        return False


def count_tables(cur):
    """
    Count tables

    :param cur: cursor object for the current connection

    :return: Table's number
    """

    logging.info(f'Fun. {inspect.stack()[0][3]}')

    print(f"Counting tables:")

    cur.execute("SELECT count(*) FROM sqlite_master WHERE type = 'table' "
                "AND name != 'android_metadata' AND name != 'sqlite_sequence';")

    num = 0
    for _ in cur:   # for each table
        num = num+1

    print(f"Found {num} tables")

    return num
