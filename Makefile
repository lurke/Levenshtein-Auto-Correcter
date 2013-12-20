all: lev

# These must be in the right order--no forward refs
FILES = order.ml \
	type.ml helpers.ml \
	twothree.ml sets.ml \
	states.ml \
	dfa.ml nfa.ml \
	freq.ml \
	matcher.ml lev.ml \
	find_matches.ml \
	spellcheck.ml \

lev: $(FILES)
	ocamlc -g -o run unix.cma str.cma $(FILES) run.ml
	ocamlc -g -o tests unix.cma str.cma $(FILES) tests.ml

clean: 
	rm -f run tests *.cmi *.cmo