# coding: utf-8

import json
import os
import re
from collections import defaultdict

from tqdm import tqdm
from sklearn.model_selection import train_test_split
import pandas as pd
from nltk.tokenize import sent_tokenize  # , word_tokenize
from argparse import ArgumentParser

all_caps = re.compile(r'[A-Z]{2,}')
alpha_re = re.compile(r'(?:[a-zA-Z]{2,20}|[aAiI]{1})')

def filter_text(sentence):
    sentence = sentence.replace("-", " ")
    sentence = sentence.replace(" 're", "re")
    sentence = sentence.replace(" 'll", "ll")
    sentence = sentence.replace(" 've", "ve")
    sentence = sentence.replace(" 'nt", "nt")
    sentence = sentence.replace(" 't", "t")
    sentence = sentence.replace(" 'm", "m")
    sentence = sentence.replace(" 's", "s")
    sentence = sentence.replace("'re", "re")
    sentence = sentence.replace("'ll", "ll")
    sentence = sentence.replace("'ve", "ve")
    sentence = sentence.replace("'nt", "nt")
    sentence = sentence.replace("'m", "m")
    sentence = sentence.replace("'t", "t")
    sentence = sentence.replace("'s", "s")
    no_caps = all_caps.sub('', sentence)
    #only_alpha = alpha_re.sub('', no_caps)
    return no_caps.lower()

def process_stimuli(source_dir="./output/data/{}/",
                    dest_dir="./output/data/{}/text/by_individual",
                    dest_dir_cluster="./output/data/{}/text/by_cluster",
                    data_round='round1'):

    source_dir = source_dir.format(data_round)
    dest_dir = dest_dir.format(data_round)
    dest_dir_clust = dest_dir_cluster.format(data_round)

    spin_by_item = pd.read_csv(os.path.join(source_dir, "spin_by_item.tsv"), sep='\t')
    spin_by_item_high = spin_by_item[spin_by_item.Predictability == 'High']
    spin_by_item_low = spin_by_item[spin_by_item.Predictability == 'Low']

    spin_text_high = spin_by_item_high.Item.drop_duplicates() \
        .apply(filter_text) \
        .apply(lambda x: ' '.join(alpha_re.findall(x)))
    spin_text_low = spin_by_item_low.Item.drop_duplicates() \
        .apply(filter_text) \
        .apply(lambda x: ' '.join(alpha_re.findall(x)))

    """
    for qualtrics_id in spin_by_item.qualtrics_id.drop_duplicates():

        if not os.path.isdir(f'{dest_dir}/{qualtrics_id}'):
            os.makedirs(f'{dest_dir}/{qualtrics_id}')

        write_path_high = f'{dest_dir}/{qualtrics_id}/spin_sentences_high.txt'
        write_path_low = f'{dest_dir}/{qualtrics_id}/spin_sentences_low.txt'

        with open(write_path_high, 'w') as fo:
            fo.write('\n'.join(spin_text_high.values))
        with open(write_path_low, 'w') as fo:
            fo.write('\n'.join(spin_text_low.values))
    """
            
    # now by cluster
    
    cluster_ids = os.listdir(dest_dir_clust)  # dirs are clusters
    
    for cluster_id in cluster_ids:

        if not os.path.isdir(f'{dest_dir_clust}/{cluster_id}'):
            os.makedirs(f'{dest_dir_clust}/{cluster_id}')

        write_path_high = f'{dest_dir_clust}/{cluster_id}/spin_sentences_high.txt'
        write_path_low = f'{dest_dir_clust}/{cluster_id}/spin_sentences_low.txt'

        with open(write_path_high, 'w') as fo:
            fo.write('\n'.join(spin_text_high.values))
        with open(write_path_low, 'w') as fo:
            fo.write('\n'.join(spin_text_low.values))

    span_by_item = pd.read_csv(os.path.join(source_dir, "span_by_item.tsv"), sep='\t')
    span_by_item['clean_text'] = span_by_item.Stimulus \
        .apply(filter_text) \
        .apply(lambda x: ' '.join(alpha_re.findall(x)))
    """
    with open("./output/data/item_texts.txt", 'w') as fo:
        for t in span_by_item.clean_text.values.tolist():
            fo.write(t)
            fo.write('\n')
        for t in spin_text_high.values.tolist():
            fo.write(t)
            fo.write('\n')
        for t in spin_text_low.values.tolist():
            fo.write(t)
            fo.write('\n')

    for qualtrics_id, group in span_by_item.groupby('qualtrics_id')['clean_text'].apply(list).iteritems():

        write_path = f'{dest_dir}/{qualtrics_id}/span_sentences.txt'
        if not os.path.isdir(f'{dest_dir}/{qualtrics_id}'):
            os.makedirs(f'{dest_dir}/{qualtrics_id}')

        with open(write_path, 'w') as fo:
            fo.write('\n'.join(group))
    """
            
    # Now by cluster
    for cluster_id in cluster_ids: #span_by_item.groupby('cluster_id')['clean_text'].apply(list).iteritems():

        write_path = f'{dest_dir_clust}/{cluster_id}/span_sentences.txt'
        if not os.path.isdir(f'{dest_dir_clust}/{cluster_id}'):
            os.makedirs(f'{dest_dir_clust}/{cluster_id}')

        with open(write_path, 'w') as fo:
            fo.write('\n'.join(span_by_item.clean_text.unique()))


def get_tokenized(texts_path, save_path):
    """ texts is dict {id: raw_text} """

    if os.path.exists(save_path):
        with open(save_path) as fo:
            joined_tokenized_sentences = json.load(fo)
    else:
        with open(texts_path) as fo:
            texts = json.load(fo)
        joined_tokenized_sentences = dict()
        for text_id, raw_text in tqdm(list(texts.items())):
            sentences = sent_tokenize(' '.join(filter_text(raw_text).split()))
            tokenized_sentences = [alpha_re.findall(s) for s in sentences]
            tokenized_sentences = [tokens for tokens in tokenized_sentences if len(tokens) >= 5]
            joined_tokenized_sentences[text_id] = [' '.join(tokens) for tokens in tokenized_sentences]
        with open(save_path, 'w') as fo:
            json.dump(joined_tokenized_sentences, fo)

    return joined_tokenized_sentences


def create_indiv_corpora(
    mapper,
    tokenized_sentence_dict,
    source_dir,
    dest_dir
):
    if isinstance(mapper, str): # is path
        with open(os.path.join(source_dir, "textid2text.json")) as fo:
            map_indiv = json.load(fo)
    else:
        assert isinstance(mapper, dict)

    for qualtrics_id, list_of_ids in map_indiv.items():
        text_items = [tokenized_sentence_dict[i] for i in set(list_of_ids)]
        flat_sentences = [sent.strip() for sent_set in text_items for sent in sent_set]

        if len(flat_sentences) > 0:
            if not os.path.isdir(f'{dest_dir}/{qualtrics_id}'):
                os.makedirs(f'{dest_dir}/{qualtrics_id}')
            train_sents, test_sents = train_test_split(flat_sentences, test_size=0.20)

            with open(f'{dest_dir}/{qualtrics_id}/train.txt', 'w') as fo:
                fo.write('\n'.join(train_sents))
            with open(f'{dest_dir}/{qualtrics_id}/test.txt', 'w') as fo:
                fo.write('\n'.join(test_sents))


                
def create_clust_corpora(
    mapper,
    tokenized_sentence_dict,
    source_dir,
    dest_dir
):
    if isinstance(mapper, str): # is path
        with open(os.path.join(source_dir, "textid2text.json")) as fo:
            map_indiv = json.load(fo)
    else:
        assert isinstance(mapper, dict)

    items_clusters_df = pd.read_csv(os.path.join(source_dir, '../items_clusters.csv'))
    #media_diet_df = pd.read_csv(os.path.join(source_dir, '../media_diet.tsv'), '\t') \
        #.merge(items_clusters_df, how='left', on='qualtrics_id')
    indiv2clust_map = items_clusters_df.set_index('qualtrics_id')['cluster.id.items'].to_dict()

    cluster_map = dict()
    for qualtrics_id, list_of_sources in map_indiv.items():
        if qualtrics_id not in indiv2clust_map:
            continue
        cluster_id = indiv2clust_map[qualtrics_id]
        if cluster_id not in cluster_map:
            cluster_map[cluster_id] = set(list_of_sources)
        else:
            cluster_map[cluster_id] = set(list_of_sources).union(cluster_map[cluster_id])

    for cluster_id, list_of_ids in cluster_map.items():
        text_items = [tokenized_sentence_dict[i] for i in set(list_of_ids)]

        flat_sentences = [sent.strip() for sent_set in text_items for sent in sent_set]

        if len(flat_sentences) > 0:
            if not os.path.isdir(f'{dest_dir}/{cluster_id}'):
                os.makedirs(f'{dest_dir}/{cluster_id}')
            train_sents, test_sents = train_test_split(flat_sentences, test_size=0.20)

            with open(f'{dest_dir}/{cluster_id}/train.txt', 'w') as fo:
                fo.write('\n'.join(train_sents))
            with open(f'{dest_dir}/{cluster_id}/test.txt', 'w') as fo:
                fo.write('\n'.join(test_sents))

def create_corpora(
    source_dir="./output/data/{}/text",
    data_round='round1'
):

    source_dir = source_dir.format(data_round)
    indiv_dest_dir = os.path.join(source_dir.format(data_round),
                                  "by_individual")
    clust_dest_dir = os.path.join(source_dir.format(data_round),
                                  "by_cluster")

    if not os.path.isdir(indiv_dest_dir):
        os.makedirs(indiv_dest_dir)
    if not os.path.isdir(clust_dest_dir):
        os.makedirs(clust_dest_dir)

    tokenized_save_path = os.path.join(source_dir, 'saved_texts_tokenized.json')
    texts_path = os.path.join(source_dir, "alltext_clean.json")
    joined_tokenized_sentences = get_tokenized(texts_path=texts_path, save_path=tokenized_save_path)

    indiv_mapper_path = os.path.join(source_dir, "textid2text.json")

    """
    create_indiv_corpora(indiv_mapper_path,
                         joined_tokenized_sentences,
                         source_dir=source_dir,
                         dest_dir=indiv_dest_dir)

    """

    create_clust_corpora(indiv_mapper_path,
                         joined_tokenized_sentences,
                         source_dir=source_dir,
                         dest_dir=clust_dest_dir)


if __name__ == '__main__':

    parser = ArgumentParser(description="Generate clean .txt files for LM training and eval")
    parser.add_argument("--gen_lv", action='store_true', help="If set, will write {train,valid,test}.txt files")
    parser.add_argument("--gen_stim_files", action='store_true', help="Generate spin high/low, span files of clean stimuli for each participant ID")
    parser.add_argument("--data_round", default='round1')
    args = parser.parse_args()

    if args.gen_lv:
        create_corpora(data_round=args.data_round)

    if args.gen_stim_files:
        process_stimuli(data_round=args.data_round)

