from tqdm import tqdm
import re


def flatten_list(list_of_lists):
    """
    :param list_of_lists: list
        List of unnested lists, e.g. [[1, 2], [3,4]]
    :return: unnested list
    """
    flat_list = [item for sublist in list_of_lists for item in sublist]
    return flat_list


def sentences_to_lower(list_of_lists):
    return [(i,[sentence.lower() for sentence in doc]) for i,doc in list_of_lists]


def remove_docs_without_hits(kwics):
    kwics = [x for x in kwics if len(x[1]) > 0]
    return kwics


def get_indices(x):
    return [_[0] for _ in x]
    

def sentence_window_i(list_of_sentences, word, window):
    kwics = []
    for i, sentence in enumerate(list_of_sentences):
        if re.search(word, sentence):
            first_index = i - window
            if first_index < 0:
                first_index = 0
            second_index = i + 1 + window
            if second_index > len(list_of_sentences):
                second_index = len(list_of_sentences)
            kwics.append(slice(first_index, second_index))
    return kwics


def sentence_windows_i_for_corpus(sentence_object, word, window, indices_included = None):
    if indices_included is not None:
        if type(indices_included) is not list:
            indices_included = [indices_included]
        docs = set(indices_included)
        sentence_object = [x for x in sentence_object if x[0] in docs]
    return [(doc[0], sentence_window_i(doc[1], word, window)) for doc in sentence_object]


def filter_index_object(index_object, sentence_object, search_pattern):
    new_index_object = []
    for index, slices in index_object:
        sublist = []
        for chunk in slices:
            text = " ".join(sentence_object[index][1][chunk])
            if re.search(search_pattern, text):
                sublist.append(chunk)
        new_index_object.append((index, sublist))
    return new_index_object


def retrieve_sentences_from_nested_indices_one_doc(
    index_object_one_doc, sentence_object
):
    assert len(index_object_one_doc) < 2
    setninger_i_dok = []
    
    if len(index_object_one_doc) == 1:
        index_object_one_doc = index_object_one_doc[0]
        doc_index = index_object_one_doc[0]
        
        for chunk in index_object_one_doc[1]:
                setninger_i_dok.append(sentence_object[doc_index][1][chunk])
                
    return setninger_i_dok


def number_of_extracted_sentences_per_doc(index_object):
    chunks_per_doc = [(doc[0], len(doc[1])) for doc in index_object]
    return chunks_per_doc
