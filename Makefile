all:
	cabal build
/home/data/enwikt-es-wds-7.txt: /home/danl/data/enwikt-es-wds-3-to-7.txt
	grep '^.\{7\}$' /home/danl/data/enwikt-es-wds-3-to-7.txt | grep -v 'à\|ç\|œ' > /home/danl/data/enwikt-es-wds-7.txt
