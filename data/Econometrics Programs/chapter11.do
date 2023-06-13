*************************************
*** This file executes the 
*** empirical work reported
*** in Chapter 11
*************************************
*** Uses data file DDK2022.dta
*************************************



clear

use DDK2011.dta, clear

pca wordscore sentscore letterscore spellscore  additions_score substractions_score multiplications_score

factor wordscore sentscore letterscore spellscore  additions_score substractions_score multiplications_score, ml factors(2)

factor wordscore sentscore letterscore spellscore  additions_score substractions_score multiplications_score, pcf factors(2)
