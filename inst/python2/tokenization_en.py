from tqdm import tqdm
from nltk.tokenize import sent_tokenize

def tokenize_sentences_en(list_of_strings):
    setninger = [sent_tokenize(text, language = "english") for text in tqdm(list_of_strings)]
    # Tillegg: Newline regnes som setningsavslutning:
    setninger = [[[x for x in line.split("\n") if x] for line in doc] for doc in setninger]
    setninger = [flatten_list(doc) for doc in setninger]
    return setninger
