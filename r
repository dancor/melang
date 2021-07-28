#!/bin/sh

#make bin/tat-count-pairs && pv ~/data/t/cur/links.tsv | /usr/bin/time bin/tat-count-pairs | tee out

#make bin/tat-count-words && pv ~/data/t/cur/sentences-por.tsv | /usr/bin/time bin/tat-count-words | tee ~/data/t/cur/word-count-por.txt

#make bin/tat-gen-cloze && pv ~/data/t/cur/links.tsv | /usr/bin/time bin/tat-gen-cloze | tee ~/data/t/cur/por-from-spa-cloze.txt
#make bin/tat-gen-cloze && pv ~/data/t/cur/links.tsv | /usr/bin/time bin/tat-gen-cloze | tee ~/data/t/cur/por-from-deu-cloze.txt

make bin/tat-gen-mem && pv ~/data/t/cur/por-from-deu-cloze.txt | /usr/bin/time bin/tat-gen-mem

