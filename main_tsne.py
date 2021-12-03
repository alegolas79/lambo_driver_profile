import os
from lambo_tsne import *
from pathlib import Path


def main():

    # *** parameters ***

    in_csv_path = "in_csv"

    fields = ["speed", "acc_longitudinal", "acc_lateral", "wheel_ang_vel_fl", "wheel_ang_vel_fr", "wheel_ang_vel_rl",
              "wheel_ang_vel_rr", "throttle", "brake_pressure",  "direction", "angular_speed", "wheel_position",
              "power_instant", "rpm", "coolant_temperature", "oil_temperature"]

    # iterate over files in directory
    for filename in os.listdir(in_csv_path):
        if filename.endswith(".csv"):

            csv_file = os.path.join(in_csv_path, filename)

            print("WORKING ON {csv_file}")

            # *** loading ***
            df = pd.read_csv(csv_file, usecols=fields)

            # *** data cleaning, filling, setup ***
            df = preprocess_data(df)

            # *** plot ***
            plot_tsne(df, Path(csv_file).stem)


if __name__ == "__main__":
    main()
