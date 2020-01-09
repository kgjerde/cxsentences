from tqdm import tqdm
from rusenttokenize import ru_sent_tokenize

def tokenize_sentences_ru(list_of_strings):
    setninger = [ru_sent_tokenize(text) for text in tqdm(list_of_strings)]
    # Tillegg: Newline regnes som setningsavslutning:
    setninger = [[[x for x in line.split("\n") if x] for line in doc] for doc in setninger]
    setninger = [(i, flatten_list(doc)) for i,doc in enumerate(setninger)]
    return setninger
