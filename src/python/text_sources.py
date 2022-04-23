import pandas as pd
import os, json, re
from zipfile import ZipFile

class Sources:
    def __init__(self, root_dir):
        self.root_dir = root_dir

    def get_metadata(self, db_list=['coca', 'coha', 'tv', 'movies', 'songlyrics']):

        dfs = list()
        for db in db_list:
            dfs.append(getattr(self, "get_{}_meta".format(db))())

        df = pd.concat(dfs, ignore_index=True, axis=0, sort=False)
        return df


    def clean_names(self, media_diet, metadata):

        news_names = media_diet[media_diet.media_type == "spoken_news"].name
        potential_names = metadata[metadata.media_type == "spoken_news"].name.unique()
        def match_news_names(name):
            for p in potential_names:
                for t in name.split():
                    if t.startswith(p):
                        return p
            return None
        media_diet.loc[media_diet.media_type == "spoken_news", "name"] = news_names \
            .apply(match_news_names)

        potential_names = metadata[metadata.media_type == "written_news"].name.dropna().unique()
        media_diet.loc[media_diet.media_type == "written_news", "name"] = media_diet \
            .loc[media_diet.media_type == "written_news", "name"] \
            .apply(lambda x: x.replace("The ", ""))

        media_diet.name = media_diet.name \
            .apply(lambda x: "New York Times" if x in ["NYTimes", "NYT", "NY Times"] else x) \
            .apply(lambda x: "Los Angeles Times" if x in ["LA Times", "LA times", "Los Angeles times", "LOS ANGELES TIMES", "la times", "La Opinion"] else x) \
            .apply(lambda x: "Denver" if x == "Denver Post" else x)

        def match_news_names(name):
            return name if name in potential_names else None
        media_diet.loc[media_diet.media_type == "written_news", "name"] = media_diet \
            .loc[media_diet.media_type == "written_news", "name"] \
            .apply(match_news_names)


        metadata.loc[metadata.media_type == "magazines", "name"] = metadata \
            .loc[metadata.media_type == "magazines", "name"] \
            .apply(lambda x: x.strip() if type(x) != float else x) \
            .apply(lambda x: "TIME" if x in ["Time", "Time Magazine"] else x) \
            .apply(lambda x: "Mens Health" if x == "MensHealth" else x) \
            .apply(lambda x: "Southern Living" if x == "SouthernLiv" else x) \
            .apply(lambda x: "Science News" if x == "ScienceNews" else x)
        metadata["media_type"] = metadata["media_type"] \
            .apply(lambda x: "magazines" if "Economist" in x else x)

        media_diet.loc[media_diet.media_type == "magazines", "name"] = media_diet \
            .loc[media_diet.media_type == "magazines", "name"] \
            .apply(lambda x: x.strip()) \
            .apply(lambda x: "People" if x == "PEOPLE" else x) \
            .apply(lambda x: "ESPN" if "ESPN" in x else x) \
            .apply(lambda x: "TIME" if x in ["Time", "Times"] else x) \
            .apply(lambda x: "Entertainment" if x == "Entertainment Weekly" else x) \
            .apply(lambda x: "New Yorker" if x == "The New Yorker" else x) \
            .apply(lambda x: "Mens Health" if x == "Men's Health" else x) \
            .apply(lambda x: "Hollywood Reporter" if x == "The Hollywood Reporter" else x) \
            .apply(lambda x: "Science News" if "Science" in x else x) \
            .apply(lambda x: "Economist" if x == "The Economist" else x)
        top_before = media_diet[media_diet.media_type == "magazines"].name.value_counts()
        top_before = top_before[top_before > 1].index.tolist()
        potential_names = metadata[metadata.media_type == "magazines"].name.dropna().unique()
        media_diet.loc[media_diet.media_type == "magazines", "name"] = media_diet \
            .loc[media_diet.media_type == "magazines", "name"] \
            .apply(lambda x: x if x in potential_names else None)

        #top_after = media_diet[media_diet.media_type == "magazines"].name.value_counts()
        #top_after = top_after[top_after > 1].index.tolist()
        return media_diet, metadata

    def get_text(self, df, save_path=None):

        if save_path is not None and os.path.exists(save_path):
            return pd.read_csv(save_path)

        songs = df[df.db == "songlyrics"]
        songs_text = pd.read_csv("/Users/brendan/Data/lv_corpora/processed/songlyrics.csv", '\t')
        merged_songs = songs_text.merge(songs, how='inner', on=['media_type', 'db', 'text_id', 'name'])

        movies = df[df.db == "Movies"]
        text_dir = "/Users/brendan/Data/lv_corpora/processed/Movies/"
        movies_text = pd.read_csv(os.path.join(text_dir, "text.tsv"), '\t')
        movies_text.text_id = movies_text.text_id.apply(lambda x: int(x.replace("##", "").replace("@@", "")))
        movies_merged = movies.merge(movies_text, how='inner', on='text_id')

        tv = df[df.db == "TV"]
        text_dir = "/Users/brendan/Data/lv_corpora/processed/TV/"
        tv_text = pd.read_csv(os.path.join(text_dir, "text.tsv"), '\t')
        tv_text.text_id = tv_text.text_id.apply(lambda x: int(x.replace("##", "").replace("@@", "")))
        tv_merged = tv.merge(tv_text, how='inner', on='text_id')

        coca = df[df.db == "COCA"]
        text_dir = "/Users/brendan/Data/lv_corpora/processed/COCA/"
        coca_text = pd.concat([pd.read_csv(os.path.join(text_dir, "spoken.tsv"), '\t'),
                               pd.read_csv(os.path.join(text_dir, "magazine.tsv"), '\t'),
                               pd.read_csv(os.path.join(text_dir, "newspaper.tsv"), '\t'),
                               pd.read_csv(os.path.join(text_dir, "fiction.tsv"), '\t'),
                               pd.read_csv(os.path.join(text_dir, "2015_update.tsv"), '\t'),
                               pd.read_csv(os.path.join(text_dir, "2017_update.tsv"), '\t')],
                              axis=0, sort=False, ignore_index=True)
        coca_text.text_id = coca_text.text_id.apply(lambda x: int(x.replace("##", "").replace("@@", "")))
        coca_merged = coca.merge(coca_text, how='inner', on='text_id')

        coha = df[df.db == "COHA"]
        text_dir = "/Users/brendan/Data/lv_corpora/processed/COHA/"
        coha_text = pd.read_csv(os.path.join(text_dir, "text.tsv"), '\t')
        coha_text.text_id = coha_text.text_id.apply(lambda x: int(x.replace("##", "").replace("@@", "")))
        coha_merged = coha.merge(coha_text, how='inner', on='text_id')

        final = pd.concat([merged_songs, tv_merged, movies_merged, coca_merged, coha_merged], 
                          axis=0, ignore_index=True, sort=False) 
        if save_path is not None:
            final.to_csv(save_path, index=False)
        return final

    def match_by_name(self, metadata, media_diet):
        meta_unique = metadata.drop_duplicates(subset=["media_type", "name"]) \
            .drop(columns=['genre', 'song', 'text_id', 'text', 'year', 'sub_name', 'title', 'author']) \
            .set_index(['media_type', 'name'])
        media_diet_unique = media_diet.drop_duplicates(subset=['media_type', 'name']) \
            .drop(columns=['qualtrics_id', 'author']) \
            .set_index(['media_type', 'name'])
        merged = media_diet_unique.join(meta_unique, on=['media_type', 'name'], how='inner').reset_index()
        return merged

    def match_by_author(self, metadata, media_diet):
        meta_unique = metadata.drop_duplicates(subset=["media_type", "author"]) \
            .drop(columns=['genre', 'song', 'name', 'text', 'text_id', 'year', 'sub_name', 'title']) \
            .set_index(['media_type', 'author'])
        media_diet_unique = media_diet.dropna(subset=['author']) \
            .drop_duplicates(subset=['media_type', 'author']) \
            .drop(columns=['qualtrics_id', 'name']) \
            .set_index(['media_type', 'author'])
        merged = media_diet_unique.join(meta_unique, on=['media_type', 'author'], how='inner').reset_index()
        return merged

    def get_match_ids(self, metadata, match_by_name, match_by_author, save_path=None):
        if save_path is not None and os.path.exists(save_path):
            return pd.read_csv(save_path)
        match_by_name["id"] = list(range(len(match_by_name)))
        match_name = metadata.merge(match_by_name, 
                               how='left', 
                               on=['media_type', 'name', 'db']) \
            .dropna(subset=['id']) \
            .loc[:, ['text_id', 'media_type', 'db', 'genre', 'name']]
        match_by_author["id"] = list(range(len(match_by_author)))
        match_author = metadata \
            .dropna(subset=['author']) \
            .merge(match_by_author, 
                   how='left', 
                   on=['media_type', 'author', 'db']) \
            .dropna(subset=['id']) \
            .loc[:, ['text_id', 'media_type', 'db', 'genre', 'author']] \
            .rename(columns={'author': 'name'})

        concat_matches = pd.concat((match_author, match_name), axis=0, sort=False, 
                                   ignore_index=True) \
            .drop_duplicates()
        if save_path is not None:
            concat_matches.to_csv(save_path, index=False)
        return concat_matches

    def get_coca_meta(self):

        sources_path = os.path.join(self.root_dir, "raw", "COCA", "sources.zip")
        df = pd.read_csv(sources_path, '\t', compression='zip', skiprows=[1],
                         encoding="Latin1", usecols=range(7)) \
            .rename(columns={'textID': 'text_id'})
        subgenre = pd.read_csv(os.path.join(self.root_dir, "raw", "COCA", "sub_genre_codes.txt"),
                               sep='\t', header=None, names=["subgen", "subgen_name"]) 
        df = df.merge(subgenre, on='subgen', how='left') 
        df = df.drop(columns=['#words'])
        df = df[df.genre != 'ACAD']
        df = df.rename(columns={'source': 'name'})
        df["media_type"] = df.genre.map({'SPOK': 'spoken_news',
                                'NEWS': 'written_news',
                                'MAG': 'magazines',
                                'FIC': 'books'})
        subgen_is_name = df.subgen_name.apply(lambda x: x.startswith("SPOK") and not x.endswith("Indep") if type(x) != float else False)
        df.loc[subgen_is_name, "name"] = df.loc[subgen_is_name, "subgen_name"].apply(lambda x: x.replace("SPOK:", ""))

        df = df[~((df.media_type == "books") & (df.name.apply(lambda x: not x.startswith("Bk"))))]
        is_book = df.media_type == "books"
        df.loc[is_book, 'name'] = df.loc[is_book, "title"].apply(lambda x: x.split(':')[0].strip())
        df = df.drop(columns=['subgen', 'title'])
        df = df.rename(columns={'subgen_name': 'sub_name'})

        df["db"] = "COCA"

        return df

    def get_coha_meta(self):

        sources_path = os.path.join(self.root_dir, "raw", "COHA", "sources_coha.xlsx")
        df = pd.read_excel(sources_path, usecols=range(7)) \
            .rename(columns={'Publication information': 'publisher', 'textID': 'text_id'}) \
            .drop(columns=['# words'])
        has_publisher = df.genre.isin(('MAG','NEWS'))
        df.loc[has_publisher, "publisher"] = df.loc[has_publisher, "publisher"].apply(lambda x: x.split(':')[0])
        is_fic = df.genre == 'FIC'
        def fix_book_author(auth_str):
            if type(auth_str) == float:
                return None
            auth_tokens = auth_str.split(',')
            if len(auth_tokens) > 2:
                auth_tokens = auth_tokens[:-1]
            return " ".join(auth_tokens[1:] + [auth_tokens[0]])

        df.loc[is_fic, 'author'] = df.loc[is_fic, 'author'].apply(fix_book_author)
        df.loc[has_publisher, 'author'] = df.loc[has_publisher, 'publisher']
        df = df.drop(columns=['publisher'])
        df = df.rename(columns={'genre': 'media_type'})
        df.media_type = df.media_type.map({'MAG': 'magazines',
                                           'NEWS': 'written_news',
                                           'FIC': 'books',
                                           'NF': 'books'})
        is_book = df.media_type == 'books'
        df.loc[is_book, "name"] = df.loc[is_book, "title"]
        df.loc[df.media_type == 'magazines', 'name'] = df \
            .loc[df.media_type == 'magazines', 'author']

        df["db"] = "COHA"
        return df

    def get_tv_meta(self):

        sources_path = os.path.join(self.root_dir, "raw", "TV", "sources_tv.zip")
        df = pd.read_csv(sources_path, '\t', compression='zip', encoding='Latin1', skiprows=[1])
        df = df[df["language(s)"].apply(lambda ls: "English" in [l.strip() for l in ls.split(',')] if type(ls) != float else True)]
        df = df.rename(columns={'series': 'name', 'fileID': 'text_id'})
        df["media_type"] = "tv"
        df = df.drop(columns=["#words", "country", "language(s)", "imdb", 
                              'textID', 'seriesID', 'episodeTitle'])
        df["db"] = "TV"
        return df

    def get_movies_meta(self):

        sources_path = os.path.join(self.root_dir, "raw", "Movies", "sources_movies.zip")
        df = pd.read_csv(sources_path, '\t', compression='zip', encoding='Latin1', skiprows=[1])
        df = df.rename(columns={'title': 'name', 'fileID': 'text_id'})
        df["media_type"] = "movies"
        df = df.drop(columns=['imdb', 'country', '#words', 'language(s)', 'textID'])
        df["db"] = "Movies"
        return df

    def get_songlyrics_meta(self):
        db1 = pd.read_csv(os.path.join(self.root_dir, "raw", "song-lyrics", "lyrics.csv")) \
            .drop(columns=['index', 'genre', 'year']) \
            .rename(columns={'lyrics': 'text'})

        clean_text = lambda x: " ".join([xi.strip().lower() for xi in re.findall(r'[a-zA-Z]+', x)]) if type(x) != float else None
        db1.song = db1.song.apply(clean_text)
        db1.artist = db1.artist.apply(clean_text)
        db2 = pd.read_csv(os.path.join(self.root_dir, "raw", "song-lyrics", "songdata.csv")) \
            .drop(columns=["link"]) 
        db2.loc[:, ['song', 'artist']] = db2.loc[:, ['song', 'artist']].applymap(lambda x: x.lower())
        db = pd.concat((db1, db2), ignore_index=False, axis=0)
        db = db.rename(columns={'artist': 'name'})
        db = db.drop_duplicates(subset=['song', 'name'])
        db["media_type"] = "songlyrics"
        db["db"] = "songlyrics"
        db["text_id"] = list(range(len(db)))
        db.to_csv(os.path.join(self.root_dir, "raw", "song-lyrics", "processed.csv"),
                  index=False, sep='\t')
        return db

    def get_paths(self, text_ids, db, subtype=None):
        for f in os.listdir(root):
            if f.startswith("wlp") and f.endswith("zip"):
                path = os.path.join(root, f)
                paths.append(path)
        delim = "##" if db == "COCA" else "@@"
        return paths, delim

