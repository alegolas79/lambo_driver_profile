import os
from connections import *
from loaders import *
from series import *


def main():

    # *** parameters ***

    # DB params
    lambo_db = os.path.join("C:", "Users", "a.giovannini", "OneDrive - Talea Srl", "lamborghini",
                            "MQTT_Data_20211111_all.db")
    vin = 'weETUYWqZWYtU'
    table = 'L0'
    fields = ["time", "speed"]

    # find and split series params
    split_threshold = 300  # sec
    speed_threshold = 60

    # *** connection ***
    conn = connect_2_lambo_db(database=lambo_db)
    if conn is None:
        exit(1)
    # test_connection(conn)

    # *** loading ***
    df = select_by_vin(connection=conn, vin=vin, table=table, fields=fields)

    # *** data cleaning, filling, setup + find interesting series ***

    df = data_cleaning(df)
    # df = df.query('20210531 < time < 20210601')
    # df = df.query('20210415 < time < 20210417')
    # df = df.query('time < 20210530')
    df = find_interesting_series(df, split_threshold=split_threshold, speed_threshold=speed_threshold)

    # *** plot ***
    plot_interesting_series(df, vin=vin, save=True, show=False)

    # *** close connection ***
    close_lambo_db(conn)


if __name__ == "__main__":
    main()
