import inspect
import logging

import pandas as pd
import seaborn as sns
from sklearn.manifold import TSNE
import os
import matplotlib.pyplot as plt


def preprocess_data(df: pd.DataFrame):
    """
    #initial_setup: filters NAs, orders by time and fills

    :param df: Dataframe with, lat, long, speed, timestamp

    :return: cleaner, preprocessed df
    """

    # drop some NaN
    df.dropna(inplace=True, subset=['time'])

    # set datetime time and sort
    try:
        df['time'] = df['time'].astype('datetime64[ns]')
    except Exception as e:
        print('An exception occurred: {}'.format(e))
        return
    df.sort_values(by='time', inplace=True, ignore_index=True)

    df.fillna(method='ffill', inplace=True)
    df.drop_duplicates(inplace=True)
    df.dropna(inplace=True)

    # print(df.info())
    # print(df.describe().T)

    return df


def plot_tsne(df: pd.DataFrame, vin: str = None):
    """
    :param vin:
    :param df: dataframe with the following mandatory columns: speed, time (datetime)

    :return: None
    """

    logging.info(f'Fun. {inspect.stack()[0][3]}')

    normalized_df = (df - df.min()) / (df.max() - df.min())

    tsne = TSNE(n_components=2, random_state=0, verbose=1)

    # Reducing the dimensionality of the data
    try:
        tsne_result = tsne.fit_transform(normalized_df)
    except Exception as e:
        print('An exception occurred: {}'.format(e))
        return

    # Plot the result of our TSNE
    tsne_result_df = pd.DataFrame({'tsne_1': tsne_result[:, 0], 'tsne_2': tsne_result[:, 1]})

    sns.scatterplot(x='tsne_1', y='tsne_2', data=tsne_result_df,
                    s=1, color='black', marker='.').set(title=f't-sne {vin}')

    # save
    tsne_result_df.to_csv(os.path.join("out_tsne", f'{vin}.csv'), index=False)
    plt.savefig(os.path.join("out_tsne", f'{vin}.png'))

    # show
    # plt.show()
