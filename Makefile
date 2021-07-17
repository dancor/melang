bin/tat-gen-cloze: src/Main/tat-gen-cloze.hs
	ghc --make $< -o $@ -odir build -hidir build

bin/tat-count-pairs: src/Main/tat-count-pairs.hs
	ghc --make $< -o $@ -odir build -hidir build

bin/tat-count-words: src/Main/tat-count-words.hs
	ghc --make $< -o $@ -odir build -hidir build

all:
	cabal build
/home/data/enwikt-es-wds-7.txt: /home/danl/data/enwikt-es-wds-3-to-7.txt
	grep '^.\{7\}$' /home/danl/data/enwikt-es-wds-3-to-7.txt | grep -v 'à\|ç\|œ' > /home/danl/data/enwikt-es-wds-7.txt
