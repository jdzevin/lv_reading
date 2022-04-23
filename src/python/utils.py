import re
from nltk.corpus import stopwords

stop_words = stopwords.words('english')
remove = re.compile(r'(?:\[.+\]|http[^\s]+|\@[^\s]+|\#[^\s]+|[\n\t\r]+)')
links_re = re.compile(r'http[^\s]+')
filler_re = re.compile(r'(?:\@\s){2,}')
parens_re = re.compile(r'(?:\(|\[).+(?:\)|\])')
narrator_re = re.compile(r'(?:(?:Mr|Mrs|Ms|Dr)\.\s+)?(?:[A-Z]+|NARRATOR)\s*\:')
mentions_re = re.compile(r'[\@\:\!\#]{1,5}[a-zA-Z0-9\-\'\!]+')
non_alpha = re.compile(r'\b[\#\@\-\'\:\_]\b')

tag_re = re.compile(r'(?:<[a-z]>|<\\[a-z]>)')
spaces_re = re.compile(r'[\s]{2,}')

alpha = re.compile(r'(?:[a-zA-Z]{2,15}|[aiAI])')

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

def tokenize(string):
    tokens = alpha.findall(string)
    return [t.lower() for t in tokens if t not in stop_words]

def clean_text(string):
    no_links = links_re.sub('', string)
    no_filler = filler_re.sub('', no_links)
    no_parens = parens_re.sub('', no_filler)
    no_narrator = narrator_re.sub('', no_parens)
    no_mentions = mentions_re.sub('', no_narrator)
    tags = tag_re.sub('\n', no_mentions)
    spaces = spaces_re.sub('\n', tags)

    no_spares = non_alpha.sub('', spaces)
    return no_spares.strip()

def chunk_text(tokens, chunk_size=200):
    chunk_list = list()
    for start in range(0, len(tokens), chunk_size):
        end = min(len(tokens), start + chunk_size)
        chunk_list.append(tokens[start:end])
    return chunk_list

