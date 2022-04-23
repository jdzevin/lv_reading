#!/bin/bash

DATADIR=./output/data/round1/text/by_cluster
n=1

for cluster_dir in $DATADIR/*; do

    if [ -f "${cluster_dir}/train.txt" ]; then

        lm_dir=${cluster_dir}/lm_files
        mkdir -p $lm_dir

        lm=${lm_dir}/${n}gram_model.lm
        stats=${lm_dir}/${n}gram_counts.txt

        stats_dir=${cluster_dir}/stats_files
        mkdir -p $stats_dir
        $SRILM/ngram-count -text ${cluster_dir}/train.txt -lm $lm -write $stats -order $n -no-sos -no-eos -sort

        echo "Finished cluster ${cluster_dir} ngrams"

    fi
done

