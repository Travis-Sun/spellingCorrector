#-*- coding:utf-8 -*-

import re, collections

CORPUSPATH=r'data'


def words(text):
    return re.findall('[a-z]+', text.lower())


def train(feature):
    model = collections.defaultdict(lambda: 1)
    for f in feature:
        model[f] += 1
    return model


NWORDS = train(words(file('data/words.txt').read()))


alphabet = 'abcdefghijklmnopqrstuvwxyz'

def edits1(word):
    splits = [(word[:i],word[i:]) for i in range(len(word)+1)]
    deletes = [a + b[1:] for a, b in splits if b]
    transposes = [a + b[1] + b[0] + b[2:] for a,b in splits if len(b)>1]
    replaces = [a + c + b[1:] for a,b in splits for c in alphabet if b]
    inserts = [a + c + b for a,b in splits for c in alphabet]
    return set(deletes + replaces + inserts)


def edits2(word):
    return set(e2 for e1 in edits1(word) for e2 in edits1(e1) if e2 in NWORDS)

def correct(word):
    def known(word): return set(w for w in word if w in NWORDS)
    candidates = known([word]) or known(edits1(word)) or known(edits2(word)) or [word]    
    return max(candidates, key=NWORDS.get)


if __name__=='__main__':
    in_word = 'corect'
    print correct(in_word)
