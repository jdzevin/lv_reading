""" Go from WLP to tidy dataframes"""
import re, os

from zipfile import ZipFile
import pandas as pd
from tqdm import tqdm

group = "coca_update"
if group == "coca_update":
    root = "/Users/brendan/Data/lv_corpora/raw/COCA/"
    dest = "/Users/brendan/Data/lv_corpora/processed/COCA/"
    if not os.path.isdir(dest):
        os.makedirs(dest)
    for genre in ['2017_update']:
        dfs = list()
        for f in os.listdir(os.path.join(root, genre)):
            if "wlp" in f and f.endswith("zip"):
                path = os.path.join(root, genre, f)
                zfile = ZipFile(path)
                for z in zfile.infolist():
                    print(z.filename)
                    raw_tokens = pd.read_csv(zfile.open(z.filename), sep='\t', engine='c',
                                             header=None, usecols=[2], quoting=3,
                                             encoding='Latin1')#[0].values.tolist()
                    raw_tokens = raw_tokens[2].values.tolist()
                    raw_string = " ".join(str(t) for t in raw_tokens)
                    spl = re.compile(r'\@\@[\d]+')
                    docs = [d.strip() for d in spl.split(raw_string) if d.strip() != ""]
                    ids = spl.findall(raw_string)
                    if len(ids) > len(docs):
                        ids = ids[:len(docs)]
                    if len(docs) > len(ids):
                        docs = docs[:len(ids)]
                    print(ids)
                    docs = pd.DataFrame.from_records({'text_id': ids, 'text': docs})
                    dfs.append(docs)
        processed = pd.concat(dfs, axis=0, ignore_index=True, sort=False)
        processed.to_csv(os.path.join(dest, genre + ".tsv"), sep='\t', index=False)


if group == "coca":
    root = "/Users/brendan/Data/lv_corpora/raw/COCA/"
    dest = "/Users/brendan/Data/lv_corpora/processed/COCA/"
    if not os.path.isdir(dest):
        os.makedirs(dest)
    for genre in ['newspaper', 'spoken', 'magazine', 'fiction']:
        dfs = list()
        for f in os.listdir(os.path.join(root, genre)):
            if f.startswith("wlp") and f.endswith("zip"):
                path = os.path.join(root, genre, f)
                zfile = ZipFile(path)
                for z in zfile.infolist():
                    print(z.filename)
                    raw_tokens = pd.read_csv(zfile.open(z.filename), sep='\t', engine='c',
                                             header=None, usecols=[0], quoting=3,
                                             encoding='Latin1')#[0].values.tolist()
                    raw_tokens = raw_tokens[0].values.tolist()
                    raw_string = " ".join(str(t) for t in raw_tokens)
                    spl = re.compile(r'##[\d]+')
                    docs = [d.strip() for d in spl.split(raw_string) if d.strip() != ""]
                    ids = spl.findall(raw_string)
                    if len(ids) > len(docs):
                        ids = ids[:len(docs)]
                    if len(docs) > len(ids):
                        docs = docs[:len(ids)]
                    docs = pd.DataFrame.from_records({'text_id': ids, 'text': docs})
                    dfs.append(docs)
        processed = pd.concat(dfs, axis=0, ignore_index=True, sort=False)
        processed.to_csv(os.path.join(dest, genre + ".tsv"), sep='\t', index=False)

elif group == 'coha':
    root = "/Users/brendan/Data/lv_corpora/raw/COHA/"
    dest = "/Users/brendan/Data/lv_corpora/processed/COHA/"
    if not os.path.isdir(dest):
        os.makedirs(dest)
    dfs = list()
    for f in os.listdir(root):
        if f.startswith("wlp") and f.endswith("zip"):
            path = os.path.join(root, f)
            zfile = ZipFile(path)
            for z in tqdm(zfile.infolist(), desc='coha'):
                raw_tokens = pd.read_csv(zfile.open(z.filename), sep='\t', engine='c',
                                         header=None, usecols=[0], quoting=3,
                                         encoding='Latin1')#[0].values.tolist()
                raw_tokens = raw_tokens[0].values.tolist()
                raw_string = " ".join(str(t) for t in raw_tokens)
                spl = re.compile(r'\@\@[\d]+')
                docs = [d.strip() for d in spl.split(raw_string) if d.strip() != ""]
                ids = spl.findall(raw_string)
                if len(ids) > len(docs):
                    ids = ids[:len(docs)]
                if len(docs) > len(ids):
                    docs = docs[:len(ids)]
                docs = pd.DataFrame.from_records({'text_id': ids, 'text': docs})
                dfs.append(docs)
    processed = pd.concat(dfs, axis=0, ignore_index=True, sort=False)
    processed.to_csv(os.path.join(dest, "text.tsv"), sep='\t', index=False)

elif group == 'tv':
    root = "/Users/brendan/Data/lv_corpora/raw/TV/"
    dest = "/Users/brendan/Data/lv_corpora/processed/TV/"
    if not os.path.isdir(dest):
        os.makedirs(dest)
    dfs = list()
    for f in os.listdir(root):
        if f.startswith("wlp") and f.endswith("zip"):
            path = os.path.join(root, f)
            zfile = ZipFile(path)
            for z in tqdm(zfile.infolist(), desc=f):
                raw_tokens = pd.read_csv(zfile.open(z.filename), sep='\t', engine='c',
                                         header=None, quoting=3,
                                         encoding='Latin1')#[0].values.tolist()
                raw_tokens = raw_tokens[2].values.tolist()
                raw_string = " ".join(str(t) for t in raw_tokens)
                spl = re.compile(r'\@\@[\d]+')
                docs = [d.strip() for d in spl.split(raw_string) if d.strip() != ""]
                ids = spl.findall(raw_string)
                if len(ids) > len(docs):
                    ids = ids[:len(docs)]
                if len(docs) > len(ids):
                    docs = docs[:len(ids)]
                docs = pd.DataFrame.from_records({'text_id': ids, 'text': docs})
                dfs.append(docs)
    processed = pd.concat(dfs, axis=0, ignore_index=True, sort=False)
    processed.to_csv(os.path.join(dest, "text.tsv"), sep='\t', index=False)

elif group == 'movies':
    root = "/Users/brendan/Data/lv_corpora/raw/Movies/"
    dest = "/Users/brendan/Data/lv_corpora/processed/Movies/"
    if not os.path.isdir(dest):
        os.makedirs(dest)
    dfs = list()
    for f in os.listdir(root):
        if f.startswith("wlp") and f.endswith("zip"):
            path = os.path.join(root, f)
            zfile = ZipFile(path)
            for z in tqdm(zfile.infolist(), desc=f):
                raw_tokens = pd.read_csv(zfile.open(z.filename), sep='\t', engine='c',
                                         header=None, quoting=3,
                                         encoding='Latin1')#[0].values.tolist()
                raw_tokens = raw_tokens[2].values.tolist()
                raw_string = " ".join(str(t) for t in raw_tokens)
                spl = re.compile(r'\@\@[\d]+')
                docs = [d.strip() for d in spl.split(raw_string) if d.strip() != ""]
                ids = spl.findall(raw_string)
                if len(ids) > len(docs):
                    ids = ids[:len(docs)]
                if len(docs) > len(ids):
                    docs = docs[:len(ids)]
                docs = pd.DataFrame.from_records({'text_id': ids, 'text': docs})
                dfs.append(docs)
    processed = pd.concat(dfs, axis=0, ignore_index=True, sort=False)
    processed.to_csv(os.path.join(dest, "text.tsv"), sep='\t', index=False)


