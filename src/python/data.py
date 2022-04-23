import os
import json
import logging
import warnings
from collections import defaultdict
import pandas as pd
import numpy as np
from tqdm import tqdm
tqdm.pandas()
from xlrd import XLRDError

warnings.simplefilter(action='ignore', category=FutureWarning)
logger = logging.getLogger(__name__)

MEDIA_COLS = ["book_childhood", "books", "discussion_boards", "magazines", "movies", "movies_childhood", "music", "music_childhood", "news_paper", "news_tv", "news_blogs_podcasts", "nonnews_blogs_podcasts", "social_media_feeds", "tv_shows", "tv_shows_childhood"]
DEMO_COLS = ["age", "political_interest", "political_scale", "english_best", "schooling_level", "gender", "race_ethnicity", "qualtrics_id", "SES", "sexual_orientation", "list_languages", "lived_cities"]


COL_MAP = {"book_childhood": 'books',
           "books": 'books',
           "magazines": 'magazines',
           "movies": 'movies',
           "movies_childhood": 'movies',
           "music": 'songlyrics',
           "music_childhood": 'songlyrics',
           "news_paper": 'written_news',
           "news_tv": 'spoken_news',
           "news_blogs_podcasts": 'online',
           "nonnews_blogs_podcasts": 'online',
           "social_media_feeds": 'online',
           "discussion_boards": "online",
           "tv_shows": 'tv',
           "tv_shows_childhood": 'tv'}


class Dataset:

    def __init__(self, root_dir, round):
        self.root_dir = root_dir
        self.round = round
        assert self.round in {'round1', 'round2'}

    def get_qualtrics(self):
        """ Read qualtrics files from Google Drive data directory

        NOTE: Needs to be redone if reading different Qualtrics files

        """

        qualtrics_root = os.path.join(self.root_dir, "SurveyData")
        with open(os.path.join(qualtrics_root, "qualtrics_map_number2item.json")) as fo:
            qualtrics_col_map = json.load(fo)

        def __clean_qualtrics(df):
            def clean_list_col(s):
                if s is None or type(s) == float:
                    return list()
                items = [a.strip() for a in s.strip().split(';')]
                items = [item for item in items if item.lower() != "n/a" and len(item) > 0]
                return items

            def attention2(x):
                try:
                    return int(x) == 2
                except ValueError:
                    return False

            df["attention_check1"] = df["attention_check1"].apply(
                lambda x: re.sub(r'[^a-zA-Z]+', '', str(x).lower()) == "yes")
            df["attention_check2"] = df["attention_check2"].apply(attention2)
            add_demo_cols = ['list_languages', 'lived_cities', 'sexual_orientation', 'race_ethnicity', 'gender']
            list_cols = MEDIA_COLS + add_demo_cols
            df.loc[:, list_cols] = df.loc[:, list_cols].applymap(clean_list_col)
            df.loc[:, add_demo_cols] = df.loc[:, add_demo_cols].applymap(lambda x: ";".join(x))

            school_map = {1: "Some HS", 2: "HS or GED", 3: "Some college",
                          4: "Associate Degree", 5: "Bachelor's Degree",
                          6: "Master's Degree", 7: "Professional/Doctorate"}
            pol_scale = {1: "Very Liberal", 2: "Moderately Liberal", 3: "Slightly Liberal",
                         4: "Neutral", 5: "Slightly Conservative", 6: "Moderately Conservative",
                         7: "Very Conservative"}
            df["political_scale_str"] = df.political_scale.astype(int).map(pol_scale)
            df["schooling_level_str"] = df.schooling_level.astype(int).map(school_map)
            df.english_best = df.english_best.fillna("Yes")
            return df

        dfs = list()
        for f in os.listdir(os.path.join(qualtrics_root, self.round)):
            if not (f.endswith('tsv') or f.endswith('csv')):
                continue
            sep = "\t" if f.endswith('tsv') else ','
            f_path = os.path.join(qualtrics_root, self.round, f)
            read_cols = list(qualtrics_col_map.keys()) + ["MTurkCode"]
            try:
                qualtrics_df = pd.read_csv(f_path, skiprows=[1, 2],
                                           sep=sep, usecols=read_cols)
            except UnicodeDecodeError as e:
                qualtrics_df = pd.read_csv(f_path, skiprows=[1, 2],
                                           sep=sep, encoding='utf-16',
                                           usecols=read_cols)
            qualtrics_df.dropna(subset=["MTurkCode"], inplace=True)
            qualtrics_df.rename(columns=qualtrics_col_map, inplace=True)
            qualtrics_df.drop_duplicates(subset=["qualtrics_id"], inplace=True)
            qualtrics_df.drop(columns=['MTurkCode'], inplace=True)
            qualtrics_df["data_collection_group"] = self.round
            dfs.append(qualtrics_df)

        Q = pd.concat(dfs, ignore_index=True, sort=False, axis=0)
        return __clean_qualtrics(Q)


    @staticmethod
    def get_demographics(qualtrics_df):
        return qualtrics_df.loc[:,
               [c for c in qualtrics_df.columns if "scene_description" not in c and c not in MEDIA_COLS]]

    @staticmethod
    def get_media_diet(qualtrics_df):

        long_ = pd.melt(qualtrics_df, id_vars="qualtrics_id",
                        var_name="media_type", value_name="name")
        long_ = long_[long_.media_type.isin(COL_MAP.keys())]
        media_diet = long_.name.apply(pd.Series) \
            .merge(long_, right_index=True, left_index=True) \
            .drop(["name"], axis=1) \
            .melt(id_vars=['qualtrics_id', 'media_type'], value_name="name") \
            .drop("variable", axis=1) \
            .dropna()

        def clean_book_author(row):
            x = row["name"]
            spl = x.split(' by ')
            if len(spl) == 1:
                return pd.Series({'name': x, 'author': None})
            else:
                return pd.Series({'name': spl[0].strip(), 'author': spl[1].strip()})

        media_diet.loc[:, ["name", "author"]] = media_diet.apply(clean_book_author, axis=1)
        media_diet = media_diet.rename(columns={'media_type': 'media_type_orig'})
        media_diet["media_type"] = media_diet.media_type_orig.map(COL_MAP)

        return media_diet

    @staticmethod
    def make_corpora(media_diet, with_text, dest="./"):
        """ Given media diet (long format) and text directory, create corpora

        Note: Unnecessary to work with this function until doing language modeling

        """
        def link_text(g_df):
            g_df_linked = g_df.merge(with_text, how='inner', on=['media_type', 'name'])
            vals = g_df_linked.loc[:, ["db", "text_id", "text"]].values
            vals = ["{}_{}".format(db, int(id_)) for db, id_, text in vals if type(text) != float]
            return vals

        full_text_db = media_diet.drop_duplicates(subset=['media_type', 'name']) \
            .merge(with_text, how='inner', on=['media_type', 'name'])
        full_text_db = full_text_db.loc[:, ["db", "text_id", "text"]].values
        full_text_db = {"{}_{}".format(db, int(id_)): t for db, id_, t in full_text_db if type(t) != float}
        full_text_db = pd.Series(full_text_db)

        clean_text_db = full_text_db.progress_apply(clean_text)

        with open(os.path.join(dest, "alltext_clean.json"), 'w') as fo:
            json.dump(clean_text_db.to_dict(), fo)

        with open(os.path.join(dest, "alltext.json"), 'w') as fo:
            json.dump(full_text_db.to_dict(), fo)

        G = media_diet.groupby('qualtrics_id') \
            .apply(link_text) \
            .to_dict()
        with open(os.path.join(dest, "textid2text.json"), 'w') as fo:
            json.dump(G, fo)

