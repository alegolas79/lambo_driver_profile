import os
import pandas as pd
from connections import *
import mysql.connector as connection
import sqlite3
from sqlite3 import Error


def main():
    lambo_db = os.path.join("Lambo_02_12.db")

    # connect
    conn = connect_2_lambo_db(lambo_db)
    if conn is None:
        print("ERROR connection failed")
        exit(1)
    test_connection(conn)

    # extract and save vin count
    print("Extracting vin and count...")
    query = f"SELECT vin, COUNT(vin) as num FROM L0 GROUP BY vin"
    df_vin_count = pd.read_sql(query, con=conn)
    df_vin_count.to_csv(os.path.join('out_db_extractions', 'vin_count.csv'), index=False)

    # for each vin -> extract and save csv
    for index, row in df_vin_count.iterrows():
        print(f"... extranctig {row['vin']}, index {index}")
        query = f"SELECT * FROM L0 WHERE vin = '{row['vin']}'"
        df = pd.read_sql(query, conn)
        df.to_csv(os.path.join("out_db_extractions", f"{row['vin']}.csv"), index=False)

    close_lambo_db(conn)


if __name__ == "__main__":
    main()
