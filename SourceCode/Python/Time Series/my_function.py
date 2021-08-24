import math
import pandas as pd
from statsmodels.tsa.stattools import adfuller,kpss

def sigmoid(x):
    return 1 / (1+math.exp(-x))

def ADF(x):
    indices = ['ADF', 'p value','lags','oservation']
    test = adfuller(x, autolag='AIC')
    res = pd.Series(test[:4],index=indices)
    for key, value in test[4].items():
        res[f'Critical value ({key})'] = value
    return res

def KPSS(x):
    indices = ['KPSS', 'p value','lags']
    test = kpss(x)
    res = pd.Series(test[:3],index=indices)
    for key, value in test[3].items():
        res[f'Critical value ({key})'] = value
    return res

