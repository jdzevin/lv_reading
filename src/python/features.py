import os
import re
import json
import warnings

warnings.simplefilter(action='ignore', category=FutureWarning)

import numpy as np
from gensim.corpora import Dictionary
from gensim.matutils import corpus2dense
from gensim.models.wrappers import LdaMallet
from gensim import utils
from sklearn.preprocessing import StandardScaler
import pandas as pd
from tqdm import tqdm

from src.python.utils import chunk_text


class Features:

    def __init__(self):
        return

    def get_items_features(self, media_diet, min_num=2):
        
        f = lambda row: "{} ({})".format(row["name"], row.media_type)
        media_diet["media_item_with_type"] = media_diet.apply(f, axis=1)
        agg = pd.crosstab(media_diet['qualtrics_id'],
                          [media_diet['media_item_with_type']],
                          #[media_diet['name']],
                          dropna=False)
        agg = agg.applymap(lambda x: int(bool(x)))
        return agg.loc[:, agg.sum() >= min_num]

    def get_types_features(self, media_diet):
        agg = pd.crosstab(media_diet.qualtrics_id,
                          [media_diet.media_type_orig],
                          dropna=False)
        vs = agg.values
        # agg.loc[:,:] = MinMaxScaler().fit_transform(vs)
        vs_normed = vs / vs.sum(axis=1, keepdims=True)
        agg.loc[:, :] = vs_normed
        return agg
