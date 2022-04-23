# Language Variety

Managing data processing, modeling, and evaluation for Language Variety/Media Diet project

## Setup and Downloading Datasets

Download `Media_Diet` directory from Google Drive. The important subdirectories of Media_Diet are:

* CorpusData/
* SurveyData/
* BehavioralData/
* Clustering/

as well as the `mastersheet.tsv` which lists participants' lab IDs and qualtrics IDs.

### Setting up python environment

This project only supports python 3.6+. Code has no guarantees of running on python@2. Install python: (https://www.python.org/downloads/)

Recommended setup: virtual environment (conda, virtualenv, etc.) 
Brief tutorial on virtualenv: https://stackoverflow.com/a/43967181

Next, install python dependencies from the file `requirements.txt`

Within virtual environment:

```
pip install -r requirements.txt
```

Without virtual environment:

```
sudo -H pip install -r requirements.txt

```

Alternatively, dependencies can be installed individually:
```
pip install gensim matplotlib nltk numpy pandas sklearn seaborn spacy tabulate tqdm xlrd
sudo -H pip install gensim matplotlib nltk numpy pandas sklearn seaborn spacy tabulate tqdm xlrd
```

`spacy` and `nltk` both require data to be downloaded before use. To download from command line:
```
python -m spacy download en_core_web_sm
python -m nltk.downloader all
```

If you have downloaded `spacy` and `nltk` data to your computer before, you probably do not need to do so again, even if you are working in a virtual environment. Your python interpreter will look at default locations on your machine for downloaded data.


## Study 1

Study 1 involves the following steps. Steps 1 & 2 can be run with `study1.py`.

1. Collecting and processing participants qualtrics surveys ("Media Diet Survey"), SPiN responses, and SPAN responses. This is handled on the backend and is only called by high-level functions in the file `study1.py`. 
2. Running clustering algorithms, contained in `study1.py`. Three clusterings are generated: media items, counts of media types, and LDA topics
3. Performing statistical analysis of testing performance and the potential effects of clustering. Performed in R via `analyze_study1.R`

### Data collection and processing

Several types of data were collected per participant: media diet survey, in the form of TSV or JSON generated by qualtrics; SPiN and SPAN responses, in the form of .XLSX sheets; and after linkage of media items to entries in text DB, a corpus per participant. Code for collecting and linking these sources is below.

Note: saving object file via `pickle` is implemented in order to save time, i.e., these only need to be run once.


Referencing file `study1.py`: 
```
dataset.add_media_diet_surveys() # adds surveys and processes
dataset.update_demographics("./data/CleanedUpMergedClustering.csv") # update demographics with those coded by Taylor
dataset.add_span() # process SPAN responses. Store in object
dataset.add_spin() # process SPiN responses. Store in object
dataset.add_text() # Based on media diet responses, compile list of text sources per participant and store in object
dataset.bow() # Given raw text lists, compute vocabulary and TF-IDF features
dataset.lda() # Run Mallet LDA with preset params
```

### Clustering

* Using media item indicators + spectral clustering with jaccard distance
    * the jaccard distance between two points is the size of their intersection (the media items they share in common) divided by the size of their union (the media items that at least one reports in their survey). This effectively handles sparsity in media item reporting, as items not reported by either are not factored into the similarity computation per pair of participants.
    * spectral clustering merely performs k-means clustering after [spectral embedding](https://scikit-learn.org/stable/modules/manifold.html#spectral-embedding), which operates on a similarity/affinity matrix
* Using frequencies of media types + k-means clustering
* Using LDA topic features generated from each participant's reported media diet text + k-means clustering
```
# Generate clusters with dataset object, which contains data preloaded

dataset.cluster('items', 'spectral', distance_metric='jaccard')
dataset.cluster('types', 'kmeans')
dataset.cluster('lda', 'kmeans')

```


