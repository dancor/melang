# Data Sources
- CEDICT (for Mandarin)
  - Source of: Definitions, Pronunciations, Words
  - Note: Pronunciations overuse fifth tone.
- E-books
  - Source of: Sentences
  - Note: Hard to find good ones, older writing is probably quite different.
  - Examples: Pride and Prejudice
- Google Ngrams (e.g. 30 billion word Chinese corpus of books since 1980)
  - Source of: Frequencies, Parts-of-speech, Words
  - Note:
    - Some non-words (yi2tiao4 "a strip" is not one word).
    - Many parts-of-speech are quite wrong and need hand-correction.
    - Doesn't even match much-better Google Translate's individual-words-info.
- LCMC (for Mandarin)
  - Source of: Parts-of-speech, Sentences, Words
  - Note:
    - Pronunciations only have one for each word, possibly not even
      most common (e.g. they have only di4 for 地).
- Wikipedia
  - Source of: Sentences
  - Note:
    - Wiki Markup might be tricky to process? Maybe not if just stripping
      things out to find sentences.
- Wiktionary
  - Source of: Definitions, Parts-of-speech, Pronunciations, Senteces, Words
  - Note:
    - Unconstrained format less consistent than it could be;
      entries somewhat hard to machine-process.
    - Not sure about difficulty of separating
      Simplified versus Traditional Mandarin.
    - Integrating "Translingual" Chinese info could also be hard.
    - Projects for parsing wiktionary all seem a bit wack.
    - Coverage not great? For Mandarin Goog top-10k:
      - 4k have no wiktionary entry (at least one Goog word isn't a real word.)
      - 1k have wiktionary entries with seemingly no part of speech or
        good definiton.

# Todo
- Pull definitions from Mandarin wiktionary.
- Scoring best Tatoeba sentences.
- Interactive practice inputting Mandarin translations in fixed-form sentences?
  As website?
- Texts reading plan.
- See how CEDICT looks lately.
- Try to pull sentences out of zhwiki.
- Automated/fake/non-hand-checked glosses for top-10k Mandarin words.

# Maybe Ever
- Compare Goog and LCMC most-common-part-of-speech info, and also across
  Goog years / decades
  - Generally: Word .. is number .. in .. files, but .. in .., ..
- Look for a Chinese wordnet (may only exist for Taiwan traditional?)
- Consider Scrabble dictionary as English word source?

# Spanish
- Spanish sentences plan:
  - Functional categories?
  - Using top words?
  - From stories?
- Spanish:
  - Speaking scenarios.
  - Work toward reading Don Quijote?
  - Song lyrics.
- Spanish app, challenge-response dynamic sentence translation w/ stats:
  - spa dict wtf: rosa, 12928:Éstos
  - Add feminines and plurals.
  - Add verb declensions.
  - Relatives and the most common Spanish-language names?
