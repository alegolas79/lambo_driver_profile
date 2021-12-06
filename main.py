import os
from lambo_tsne import *
from pathlib import Path
from series import *


def main():
    # *** parameters ***

    in_csv_path = "in_csv"
    fields = ["time", "speed", "acc_longitudinal", "acc_lateral", "wheel_ang_vel_fl", "wheel_ang_vel_fr",
              "wheel_ang_vel_rl",
              "wheel_ang_vel_rr", "throttle", "brake_pressure", "direction", "angular_speed", "wheel_position",
              "power_instant", "rpm", "coolant_temperature", "oil_temperature"]

    # find and split series params
    split_threshold = 300  # sec
    speed_threshold = 60

    # iterate over files in directory
    for i, filename in zip(range(0, len(os.listdir(in_csv_path))),  os.listdir(in_csv_path)):
        if filename.endswith(".csv"):
            csv_file = os.path.join(in_csv_path, filename)
            vin = Path(csv_file).stem

            print(f'PROCESSING {csv_file} [{i}/{len(os.listdir(in_csv_path))}]')

            # *** loading ***
            df = pd.read_csv(csv_file, usecols=fields)

            # *** data cleaning, filling, setup ***
            df = preprocess_data(df)
            if df is None:
                continue

            # *** data cleaning, filling, setup + find interesting series ***

            # df = df.query('20210531 < time < 20210601')
            # df = df.query('20210415 < time < 20210417')
            # df = df.query('time < 20210530')
            df = find_interesting_series(df, split_threshold=split_threshold, speed_threshold=speed_threshold)
            if df is None:
                continue

            # *** plot ***
            plot_interesting_series(df, vin=vin, save=True, show=False)

            # keep only interesting
            df = df.loc[df['keep_serie']]
            df.drop(columns=["start_stop", "id_serie", "speed_max_serie", "keep_serie",
                             "time", "year", "month", "day"], inplace=True)

            # *** tsne ***
            plot_tsne(df, vin)


if __name__ == "__main__":
    main()
