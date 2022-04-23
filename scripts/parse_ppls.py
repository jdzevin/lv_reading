import os
import sys
sys.path.append(os.path.expanduser("~/language-variety"))
import re

from tqdm import tqdm
import pandas as pd

from src.python.create_text_files import filter_text, all_caps, alpha_re

SOURCE_DIR = "./output/data/round2/"
spin_by_item = pd.read_csv(os.path.join(SOURCE_DIR, "spin_by_item.tsv"), sep='\t') 

# Fix missing item no.s
text2id_map = spin_by_item.dropna(subset=['TrialNum']) \
    .drop_duplicates(subset=['TrialNum', 'Item']) \
    .set_index('Item') \
    .loc[:, 'TrialNum'] \
    .to_dict()
spin_by_item.TrialNum = spin_by_item.Item.map(text2id_map)
spin_by_item = spin_by_item \
    .drop_duplicates(subset=['TrialNum', 'Item']) \
    .loc[:, ['TrialNum', 'Item', 'Predictability']] \
    .rename(columns={'Item': 'Stimulus'})
    #.dropna(subset=['TrialNum']) 

spin_by_item_high = spin_by_item[spin_by_item.Predictability == 'High']
spin_by_item_low = spin_by_item[spin_by_item.Predictability == 'Low']

spin_by_item_high['sentence'] = spin_by_item_high.Stimulus \
    .apply(filter_text) \
    .apply(lambda x: ' '.join(alpha_re.findall(x)))
spin_by_item_low['sentence'] = spin_by_item_low.Stimulus \
    .apply(filter_text) \
    .apply(lambda x: ' '.join(alpha_re.findall(x)))

span_by_item = pd.read_csv(os.path.join(SOURCE_DIR, "span_by_item.tsv"), sep='\t') \
    .drop_duplicates(subset=['Stimulus']) \
    .loc[:, ['Set', 'Trial', 'Sentence', 'Stimulus']]

span_by_item['sentence'] = span_by_item.Stimulus \
    .apply(filter_text) \
    .apply(lambda x: ' '.join(alpha_re.findall(x)))

def _parse_ppl_file(raw_data):
    chunks = raw_data.split('\n\n')

    sentence_stats = chunks[:-1]
    meta = chunks[-1]
    return_df = pd.DataFrame()
    for sentence_data in sentence_stats:
        lines = sentence_data.split('\n')

        sentence_summary = '\n'.join(lines[-2:])
        sentence_text = lines[0].strip()

        lines = lines[1:-2]

        ppl_data = [l.split('\t')[1:] for l in lines]
        likelihood_data = list()
        for i, (ngram_text, probabilities) in enumerate(ppl_data):

            likelihood, loglikelihood = re.findall(r'[\-]?(?:\d+(?:[.]?\d+)*(?:[eE][+\-]?\d*)*|inf)\b', probabilities)
            sentence_ppl = re.search(r'ppl\= \d+[.]?\d+', sentence_summary).group(0).replace('ppl= ', '')
            batch_ppl = re.search(r'ppl\= \d+[.]?\d+', meta).group(0).replace('ppl= ', '')

            likelihood_data.append({
                'ngram_text': ngram_text,
                'context_size': re.search(r'\[\dgram\]', probabilities).group(0).replace('gram', '').strip(']['),
                'likelihood': float(likelihood),
                'loglikelihood': float(loglikelihood),
                'sentence': sentence_text,
                'sentence_ppl': float(sentence_ppl),
                'batch_ppl': float(batch_ppl),
                'is_lastword': i == len(ppl_data) - 2
            })
        likelihood_df = pd.DataFrame(likelihood_data)
        return_df = return_df.append(likelihood_df)

    return return_df

def parse_ppl_dir(path_to_dir, ngram, qualtrics_id=None, cluster_id=None):
    """ path_to_dir contains .txt files (3) with spin/span ppls """

    span_path = os.path.join(path_to_dir, f"span_{ngram}_ppl.txt")
    with open(span_path) as fo:
        span_data = fo.read()
    span_parsed = _parse_ppl_file(span_data)
    #span_parsed['qualtrics_id'] = re.search(r'R_[a-zA-Z0-9]+', path_to_dir).group(0)
    if qualtrics_id is not None:
        span_parsed['qualtrics_id'] = qualtrics_id
    if cluster_id is not None:
        span_parsed['cluster_id'] = cluster_id
    span_parsed['batch_name'] = 'SPAN'
    if len(span_parsed) == 0:
        return
    span_parsed = span_parsed.merge(span_by_item, how='left', on='sentence')
    span_parsed['TrialNum'] = span_parsed.apply(
        lambda x: f"{x['Set']}-{x['Trial']}-{x['Sentence']}", axis=1
    )

    spin_high_path = os.path.join(path_to_dir, f"spin_high_{ngram}_ppl.txt")
    with open(spin_high_path) as fo:
        spin_high_data = fo.read()
    spin_high_parsed = _parse_ppl_file(spin_high_data)
    #spin_high_parsed['qualtrics_id'] = re.search(r'R_[a-zA-Z0-9]+', path_to_dir).group(0)
    if qualtrics_id is not None:
        spin_high_parsed['qualtrics_id'] = qualtrics_id
    if cluster_id is not None:
        spin_high_parsed['cluster_id'] = cluster_id
    spin_high_parsed['batch_name'] = 'SPIN-HIGH'
    if len(spin_high_parsed) == 0:
        return
    spin_high_parsed = spin_high_parsed.merge(spin_by_item_high, how='left', on='sentence')
    spin_high_parsed['TrialNum'] = spin_high_parsed.TrialNum.apply(lambda x: str(int(x)))

    spin_low_path = os.path.join(path_to_dir, f"spin_low_{ngram}_ppl.txt")
    with open(spin_low_path) as fo:
        spin_low_data = fo.read()
    spin_low_parsed = _parse_ppl_file(spin_low_data)
    #spin_low_parsed['qualtrics_id'] = re.search(r'R_[a-zA-Z0-9]+', path_to_dir).group(0)
    if qualtrics_id is not None:
        spin_low_parsed['qualtrics_id'] = qualtrics_id
    if cluster_id is not None:
        spin_low_parsed['cluster_id'] = cluster_id
    spin_low_parsed['batch_name'] = 'SPIN-LOW'
    if len(spin_low_parsed) == 0:
        return
    spin_low_parsed = spin_low_parsed.merge(spin_by_item_low, how='left', on='sentence')
    spin_low_parsed['TrialNum'] = spin_low_parsed.TrialNum.apply(lambda x: str(int(x)))

    to_concat = [span_parsed, spin_high_parsed, spin_low_parsed]
    lm_df = pd.concat(to_concat, axis=0) 
    lm_df['N'] = int(ngram.replace('gram', ''))
    return lm_df

if __name__ == '__main__':

    ROUND = 'round2'
    CLUSTER_LEVEL = True

    if CLUSTER_LEVEL:
        key_name = "cluster_id"
        root_dir = "./output/data/{}/text/by_cluster".format(ROUND)
        out_dir = os.path.join(f"./output/lm/{ROUND}/by_cluster/")
    else:
        key_name = "qualtrics_id"
        root_dir = "./output/data/{}/text/by_individual/".format(ROUND)
        out_dir = os.path.join(f"./output/lm/{ROUND}/by_individual/")
    
    if not os.path.isdir(out_dir):
        os.makedirs(out_dir)
        
    ppl_by_item_dfs = list()
    ll_by_item_dfs = list()
    
    for n in ['1gram', '2gram', '3gram']:
    
        parsed_dfs = list()
        for dir_ in tqdm(os.listdir(root_dir), desc="Parsing ppl data from files"):
            test_path = f'{root_dir}/{dir_}/stats_files/indomain_test_{n}_ppl.txt'
            if not os.path.isfile(test_path):
                continue
            params = {key_name: dir_}
            parsed_ppl_df = parse_ppl_dir(
                path_to_dir=f"{root_dir}/{dir_}/stats_files",
                ngram=n,
                **params
            )
            if parsed_ppl_df is not None:
                parsed_dfs.append(parsed_ppl_df)
        parsed = pd.concat(parsed_dfs, axis=0, ignore_index=True)

        print(parsed.columns)
        ppl_by_item = parsed \
            .drop_duplicates(subset=[key_name, 'batch_name', 'sentence', 'sentence_ppl']) \
            .loc[:, [key_name, 'batch_name', 'sentence', 'TrialNum', 'sentence_ppl', 'context_size', 'N']]
        ll_by_item = parsed[parsed.is_lastword] \
            .drop_duplicates(subset=[key_name, 'batch_name', 'sentence', 'loglikelihood']) \
            .loc[:, [key_name, 'batch_name', 'sentence', 'TrialNum', 'loglikelihood', 'likelihood', 'ngram_text', 'context_size', 'N']]
        ppl_by_item_dfs.append(ppl_by_item)
        ll_by_item_dfs.append(ll_by_item)
        
    ppl_by_item = pd.concat(ppl_by_item_dfs, axis=0, ignore_index=True)
    ll_by_item = pd.concat(ll_by_item_dfs, axis=0, ignore_index=True)

        
    ppl_by_item.to_csv(os.path.join(out_dir, "ppl_by_item.csv"), index=False)
    ll_by_item.to_csv(os.path.join(out_dir, "ll_by_item.csv"), index=False)

