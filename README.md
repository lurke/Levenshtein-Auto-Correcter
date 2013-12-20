# Levenshtein Auto-Correcter #

Created by Lauren Urke, Mike Rizzo, Sasha Johnson-Freyd, and Jane Thomas

## Implementing Levenshtein Automata in OCaml

Created by: Lauren Urke, Mike Rizzo, Sasha Johnson-Freyd, and Jane Thomas
To compile, simply type <code>make</code> in the command line.  This will 
generate two executable files, 'run' and 'tests.'  run represents 
the main corpus of our project while 'tests' has some tests for 
our different functions that we use.  Execute tests using 
<code>./tests</code> and it should complete without error.  When executing
run, there are two choices, the find_matches function and the 
spellcheck one.  To find all the matches of a word a given
levenshtein distance away from it in a designated dictionary,
use the following command 
<pre><code>./run find_matches (word) (distance) (dictionary)</code></pre>
As an example, try executing 
<pre><code>./run find_matches aba 1 simple.txt</code></pre>
This will return with four matches.  
To run the spellchecker, use the command
<pre><code>./run spellcheck (file) (dictionary)</code></pre>
and the designated will be spellchecked against the dictionary.  Try 
<pre><code>./run spellcheck text.txt dict.txt</code></pre>
to try spellchecking an excerpt from Harvard's CS51 website against a dictionary.  This will
yield four misspelled words and possible matches each a 
levenshtein distance of two away from the misspelled word.

### Contents ###

<pre>
order.ml
    from moogle. defines the different order types for comparison used in the
    dict and set
type.ml
    defines basic types, signatures, and interfaces used elsewhere in our project
helpers.ml
    defines some basic functions used in our various implementations.
    right now includes option functions deopt and is_some
twothree.ml
    from moogle with a twist.  the implementation of the twothree tree.  This is 
    used for the dictionary part of our finite automata as well as constructing sets.
sets.ml
    from moogle with a twist.  the implementation of a set using a twothree tree.
dfa.ml
    the definition for a DFA functor that can generate a dfa object.  Also contains
    some tests and examples at the bottom.
nfa.ml
    the definition for a NFA functor that can generate a nfa object.
    also includes the powerset constructor as the construct method of the nfa.
freq.ml
    constructs a dictionary for the 5000 most common english words with key word and 
    value frequency.  This is used for sorting the results given by our 
    find_word_matches function
matcher.ml
    creates the dictionary return/matcher object for looking up our possible matches 
    in the dictionary.  right now is very inefficient, but we will look on improving 
    this once we get our project working.
lev.ml
    the finale of our project.  Constructs a nfa from a word and a distance representing 
    states as a tuple of ints then constructing a dfa from this using powersets.  Finally 
    we match possible words from the dfa against our dictionary.
find_matches.ml
    defines a function for finding all the matches given a word, distance, and dictionary.
    Makes calls to lev.ml to get all the matches for a word.
    <code>USAGE: ./run find_matches (word:string) (distance:int) (dictionary:string)</code>
spellcheck.ml
    defines a function for finding all the misspelled words in a document by checking these
    against a dictionary then constructs levenshtein automata for each of these 2 units away
    and returns all the matches in the diction as possible correct words.
    <code>USAGE: ./run spellcheck (file:string) (dictionary:string)</code>
run.ml
    the executable file which takes arguments of either find_matches or spellcheck and runs 
    their corresponding functions.
    <code>USAGE: ./run find_matches | spellcheck</code>
tests.ml
    contains the basic tests for the different (directly testable) components of our algorithm.
Makefile
    makefile for our project.  generates executable file for run.
simple.txt
    A simple dictionary we constructed for basic and quick testing.
dict.txt
    A more extensive dictionary of english words used for more in depth searching and matching.
frequencies.csv
    A list obtained from http://www.wordfrequency.info/free.asp of the most common 5000 
    english words with their frequencies.  This is used for constructing the dictionary 
    of word frequencies in freq.ml.
text.txt
    A simple text file excerpted from the cs51 website used for basic testing of spellcheck
</pre>