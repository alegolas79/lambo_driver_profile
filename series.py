import datetime
import inspect
import logging
import os
import pandas as pd
import numpy as np
import plotly.express as px
import plotly.graph_objects as go

# disable the SettingWithCopyWarning
pd.options.mode.chained_assignment = None  # default='warn'


def data_cleaning(df: pd.DataFrame):
    """
    #initial_setup: filters NAs, orders by time and fills

    :param df: Dataframe with, lat, long, speed, timestamp

    :return: cleaner df
    """

    logging.info(f'Fun. {inspect.stack()[0][3]}')

    df.replace([None], np.nan, inplace=True)

    # drop some NaN
    df.dropna(inplace=True, subset=['time'])
    # df.dropna(inplace=True, subset=['latitude', 'speed'], how='all')

    # set datetime time and sort
    df['time'] = df['time'].astype('datetime64[ns]')

    df.sort_values(by='time', inplace=True, ignore_index=True)

    # # backfill 'latitude', 'longitude'
    # df[['latitude', 'longitude']] = df[['latitude', 'longitude']].fillna(method='bfill')
    #
    # # set types
    # df['latitude'] = df['latitude'].astype(np.float64)
    # df['longitude'] = df['longitude'].astype(np.float64)
    df['speed'] = df['speed'].astype(np.single)

    return df


def find_interesting_series(df: pd.DataFrame,
                            split_threshold: int = 300, speed_threshold: float = 60):
    """

    :param df: dataframe with the following mandatory columns: speed, time (datetime)
    :param split_threshold: tempo che deve intercorrere tra due velocità perchè siano considerate serie separate espresso in secondi
    :param speed_threshold: velocità sotto la quale la serie non è considerata di interesse

    :return: df with new columns: id_serie, speed_max_serie, keep_serie
    """

    logging.info(f'Fun. {inspect.stack()[0][3]}')

    # df_ss = df.copy()
    df_ss = df[['time', 'speed']].copy()

    # al fine di identificare le serie, posso considerare le speed NaN come 0
    df_ss[['speed']] = df_ss[['speed']].fillna(0)

    # creo una colonna con le velocità della riga precedente
    df_ss['speed_prev'] = df_ss['speed'].shift(1)

    # utilizzo il confronto tra velocità attuale e precedente per identificare i momenti di start e stop
    df_ss["start_stop"] = np.nan

    # speed > 0 && speed_prev == 0 -> start
    # speed_prev > 0 && speed = 0 -> stop
    df_ss.loc[(df_ss['speed'] > 0) &
              np.isclose(df_ss['speed_prev'], np.zeros(len(df_ss['speed_prev']))), 'start_stop'] = 'start'
    df_ss.loc[(df_ss['speed_prev'] > 0) &
              np.isclose(df_ss['speed'], np.zeros(len(df_ss['speed']))), 'start_stop'] = 'stop'

    df_ss['start_stop'] = df_ss['start_stop'].astype('category')

    # Seleziono solo record di inizio e fine serie
    df_ss.dropna(inplace=True, subset=['start_stop'])
    if df_ss.empty:
        print("WARNING: df_ss is empty")
        return None

    # Creo colonna con datetime della riga precedente
    df_ss['time_prev'] = df_ss['time'].shift(1)

    # Attenzione, per la prima riga non ho un time_prev.
    # Se la prima riga è di start, inserisco un dummy time_prev > split_threshold
    if df_ss['start_stop'].iloc[0] == 'start':
        df_ss.iloc[0, df_ss.columns.get_loc('time_prev')] = \
            df_ss['time'].iloc[0] - datetime.timedelta(seconds=split_threshold + 1)

    # Trovo quanto tempo la macchina è stata ferma prima di uno start
    df_ss["stop_period_prev"] = np.nan
    df_ss.loc[df_ss['start_stop'] == 'start', 'stop_period_prev'] = df_ss['time'] - df_ss['time_prev']

    # Creo colonna con datetime della riga successiva
    df_ss['time_next'] = df_ss['time'].shift(-1)

    # Attenzione, per l'ultima riga non ho un time_next.
    # Se l'ultima riga è di stop, inserisco un dummy time_next > split_threshold
    if df_ss['start_stop'].iloc[-1] == 'stop':
        df_ss.iloc[-1, df_ss.columns.get_loc('time_next')] = \
            df_ss['time'].iloc[-1] + datetime.timedelta(seconds=split_threshold + 1)

    # Trovo quanto tempo la macchina è rimasta ferma dopo uno stop
    df_ss["stop_period_next"] = np.nan
    df_ss.loc[df_ss['start_stop'] == 'stop', 'stop_period_next'] = df_ss['time_next'] - df_ss['time']

    # Seleziono start e stop sulla base del parametro di tempo (split_threshold),
    # in modo che le serie separate da un periodo < di split_threshold risultino come la stessa serie
    df_ss = df_ss[
        ((df_ss['start_stop'] == 'start') & (df_ss['stop_period_prev'] > datetime.timedelta(seconds=split_threshold)))
        | ((df_ss['start_stop'] == 'stop') & (df_ss['stop_period_next'] > datetime.timedelta(seconds=split_threshold)))]

    # Genero ID di inizio serie per gli start, metto "stop" per gli stop
    df_ss["id_serie"] = np.nan
    df_ss.loc[df_ss['start_stop'] == 'stop', 'id_serie'] = 'stop'
    na_count = df_ss['id_serie'].isna().sum()
    ids = range(1, na_count + 1)
    df_ss.loc[df_ss['start_stop'] == 'start', 'id_serie'] = ids

    # join original df and df_ss on index
    df_ss.drop(columns=['time', 'speed', 'speed_prev', 'time_prev', 'stop_period_prev', 'time_next', 'stop_period_next'],
               inplace=True)
    df = df.join(df_ss)

    # fill id_serie e trasformazione stop in NaN
    df['id_serie'] = df['id_serie'].fillna(method='ffill')
    df['id_serie'] = df['id_serie'].replace('stop', np.nan)

    # calcolo la velocità massima raggiunta nel corso della serie
    df['speed_max_serie'] = df.groupby('id_serie')['speed'].transform('max')

    # Sulla base della velocità massima serie confrontata con parametro velocità
    # creo variabile che indichi se il periodo ci è interessante o no
    df["keep_serie"] = False
    df.loc[df['speed_max_serie'] > speed_threshold, 'keep_serie'] = True

    return df


def plot_interesting_series(df: pd.DataFrame, vin: str, save: bool = False, show: bool = True):
    """

    :param show: if True: show chart
    :param vin:
    :param save: if True: save chart
    :param df: dataframe

    :return: None
    """

    logging.info(f'Fun. {inspect.stack()[0][3]}')

    df['year'] = df['time'].dt.year
    df['month'] = df['time'].dt.month
    df['day'] = df['time'].dt.day

    years = df['year'].unique()
    months = df['month'].unique()
    days = df['day'].unique()

    for year in years:

        df_y = df[df['year'] == year]

        for month in months:

            df_m = df_y[df_y['month'] == month]
            if df_m.empty:
                continue

            for day in days:

                df_d = df_m[df_m['day'] == day]
                if df_d.empty:
                    continue

                fig = go.Figure()

                # all data
                fig.add_trace(go.Scatter(x=df_d['time'], y=df_d['speed'], name='all',
                                         mode='lines+markers',
                                         marker=dict(color='red', size=3),
                                         line=dict(color='lightgray', width=1)))

                # points to keep
                df_keep = df_d.loc[df['keep_serie']]

                fig.add_trace(go.Scatter(x=df_keep['time'], y=df_keep['speed'], name='keep',
                                         mode='markers', marker=dict(color='green', size=3)))

                # Edit the layout
                fig.update_layout(title=f'{vin} {year}-{month}-{day}',
                                  xaxis_title='datetime', yaxis_title='speed',
                                  template='plotly_white')

                if save:
                    out_path = os.path.join("out_series", vin)
                    filename = os.path.join(out_path, f'{year}-{month}-{day}.html')  #

                    # create saving dir if not exists
                    if not os.path.exists(out_path):
                        os.makedirs(out_path)

                    # remove file if exist
                    if os.path.isfile(filename):
                        os.remove(filename)

                    fig.write_html(filename)

                if show:
                    fig.show()
