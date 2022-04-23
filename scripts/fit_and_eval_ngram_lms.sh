#!/bin/bash

DATADIR=./output/data/round1/text/by_individual

for cluster_dir in $DATADIR/*; do

    if [ -f "${cluster_dir}/train.txt" ]; then

        lm_dir=${cluster_dir}/lm_files
        mkdir -p $lm_dir

        for n in 1 2 3; do

            lm=${lm_dir}/${n}gram_model.lm
            stats=${lm_dir}/${n}gram_counts.txt

            stats_dir=${cluster_dir}/stats_files
            mkdir -p $stats_dir
            $SRILM/ngram-count -text ${cluster_dir}/train.txt -lm $lm -write $stats -unk -kndiscount3 -order $n

            $SRILM/ngram -lm $lm -ppl ${cluster_dir}/test.txt -order $n -unk > ${stats_dir}/indomain_test_${n}gram_ppl.txt
            $SRILM/ngram -lm $lm -ppl ${cluster_dir}/spin_sentences_high.txt -order $n -unk -debug 2 > ${stats_dir}/spin_high_${n}gram_ppl.txt
            $SRILM/ngram -lm $lm -ppl ${cluster_dir}/spin_sentences_low.txt -order $n -unk -debug 2 > ${stats_dir}/spin_low_${n}gram_ppl.txt
            $SRILM/ngram -lm $lm -ppl ${cluster_dir}/span_sentences.txt -order $n -unk -debug 2 > ${stats_dir}/span_${n}gram_ppl.txt
        done

        rm -rf $lm_dir
        echo "Finished cluster ${cluster_dir} ngrams"

    fi
done

