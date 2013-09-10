# Data Sources
* CEDICT (for Mandarin)
  * Source of: Definitions, Pronunciations, Words
  * Note: Pronunciations overuse fifth tone.
* E-books
  * Source of: Sentences
  * Note: Hard to find good ones, older writing is probably quite different.
  * Examples: Pride and Prejudice
* Google Ngrams (e.g. 30 billion word Chinese corpus of books since 1980)
  * Source of: Frequencies, Parts-of-speech, Words
  * Note:
    * Some non-words (yi2tiao4 "a strip" is not one word).
    * Many parts-of-speech are quite wrong and need hand-correction.
    * Doesn't even match much-better Google Translate's individual-words-info.
* Wikipedia
  * Source of: Sentences
  * Note:
    * Wiki Markup might be tricky to process? Maybe not if just stripping
      things out to find sentences.
* Wiktionary
  * Source of: Definitions, Parts-of-speech, Pronunciations, Senteces, Words
  * Note:
    * Unconstrained format less consistent than it could be;
      entries somewhat hard to machine-process.
    * Not sure about difficulty of separating
      Simplified versus Traditional Mandarin.
    * Integrating "Translingual" Chinese info could also be hard.

# Todo
* Consider Scrabble dictionary as a word source?
