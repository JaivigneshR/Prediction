# -*- coding: utf-8 -*-
"""
Created on Mon Mar 20 19:58:17 2017

@author: Jaivignesh
"""






    
   
import re
from collections import Counter

def words(text): return re.findall(r'\w+', text.lower())
WORDS = Counter(words(open('corpus.txt').read()))
def P(word, N=sum(WORDS.values())): 
        "Probability of `word`."
        return WORDS[word] / N

def correction(word): 
        "Most probable spelling correction for word."
        #print("Tseting purpose")
        return max(candidates(word), key=P)

def candidates(word): 
        "Generate possible spelling corrections for word."
        #print("1tset")
        return (known([word]) or known(edits1(word)) or known(edits2(word)) or [word])

def known(words): 
        "The subset of `words` that appear in the dictionary of WORDS."
        #print("2tset")
        return set(w for w in words if w in WORDS)

def edits1(word):
        "All edits that are one edit away from `word`."
        #print("3tset")
        letters    = 'abcdefghijklmnopqrstuvwxyz'
        splits     = [(word[:i], word[i:])    for i in range(len(word) + 1)]
        deletes    = [L + R[1:]               for L, R in splits if R]
        transposes = [L + R[1] + R[0] + R[2:] for L, R in splits if len(R)>1]
        replaces   = [L + c + R[1:]           for L, R in splits if R for c in letters]
        inserts    = [L + c + R               for L, R in splits for c in letters]
        return set(deletes + transposes + replaces + inserts)

def edits2(word): 
        "All edits that are two edits away from `word`."
        #print("4tset")
        return (e2 for e1 in edits1(word) for e2 in edits1(e1))
    
print ("type someting to begin")

a=True

while (a == True):
                print("")
                print("user:")
                In = input()
                if(re.match("^[A-Za-z-\s]*$", In)):

                
                 if(In.lower()!="bye"):
                  Splited = In.split()
                  for i in range(len(Splited)):
                    if(i==0):
                     print("BOT:")
                     
                    print((correction(Splited[i])), end= " ")
                       
                
                 else:
                    a=False
                else:
                      print("Invalid Input")