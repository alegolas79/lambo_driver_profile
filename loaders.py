import pandas as pd
import inspect
import logging


def select_by_vin(connection, vin: str, table: str, fields: list = None):
    """

    :param connection: Connection object
    :param vin: vehicle ID
    :param table: table name
    :param fields: attributes list to select; if None: select all

    :return: pandas' DF
    """

    logging.info(f'Fun. {inspect.stack()[0][3]}')

    attributes = "*"

    if len(fields) == 1:
        attributes = fields[0]
    elif len(fields) > 1:
        attributes = ", ".join(fields)

    query = f"SELECT {attributes} FROM '{table}' WHERE vin='{vin}'"
    df = pd.read_sql(query, connection)

    return df
