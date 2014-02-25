/** Cryptograms - Assignment #6
  * CIT 591, Fall 2013
  * @author Chris Herdt
  * @author Yupeng Lu
  */

package cryptograms
import scala.util.Random
import scala.io.Source, scala.io.Source._, scala.swing.FileChooser
import java.io.File

object Cryptograms {

  // Placeholder for unknown/unguessed characters
  val unknownChar = '_'

    // Use bestGuess to track the best guess (most correct words) we have so far
  var bestGuess: String = ""
  var correctWordsInBestGuess = 0
  var lastCorrectWordCount = 0
 
  // Store the frequency string for the given cryptogram
  var cryptogramFrequencyString = ""
  
  // TODO: we could compile N-grams from the list of quotes and/or dictionary
  // Source: http://www.cryptograms.org/letter-frequencies.php#Quadrigrams
  val commonQuadrigrams = Vector("THAT", "THER", "WITH", "TION", "HERE",
                                 "OULD", "IGHT", "HAVE", "HICH", "WHIC",
                                 "THIS", "THIN", "THEY", "ATIO", "EVER",
                                 "FROM", "OUGH", "WERE", "HING", "MENT")

  // Source: http://www.cryptograms.org/letter-frequencies.php#Trigrams
  val commonTrigrams = Vector("THE","AND","ING","HER","HAT",
                              "HIS","THA","ERE","FOR","ENT",
                              "ION","TER","WAS","YOU","ITH",
                              "VER","ALL","WIT","THI","TIO")

   
  // Source: http://www.cryptograms.org/letter-frequencies.php#Bigrams
  val commonBigrams = Vector("TH","HE","IN","ER","AN",
                             "RE","ND","ON","EN","AT",
                             "OU","ED","HA","TO","OR",
                             "IT","IS","HI","ES","NG")  

  // Source: http://www.cryptograms.org/letter-frequencies.php#Relative_frequencies_of_letters
  // Letters, sorted in order of frequency in English: ETAOINSHRDLCUMWFGYPBVKJXQZ
  // The more letters we include below, the longer the solve method takes!
  val frequentLetters = "ETAOINS"

  /** Start the not-very-interactive program
    * It will try to solve randomly-selected cryptograms
    * until the user tells it to stop
    */
  def main(args: Array[String]) = {
    var userInput = "Y"
    var puzzle = ""
    while (!(userInput.toUpperCase startsWith "N")) {
      puzzle = getCryptogram
      println("\n" + puzzle)
      println("...thinking...")
      println("\nI think the answer is: " + solve(puzzle) + "\n")
      println("Try another cryptogram?")
      userInput = readLine()
    }
  }
    
  /** Returns the number of unguessed letters in a word
    * (It would be better to take unique unguessed letters, but we turned them into placeholders) 
    * @param word the word to evaluate
    * @return an integer 
    */
  def getUnguessedLetterCount(word: String): Integer = {
    // Anything that is not a letter we consider unguessed
    word match {
      // Check dictionary for the case of punctuation, e.g. "DON'T" or "WASN'T"
      case w if Dictionary.isIn(word) => 0
      case _ => word.count(ch => ch < 'A' || ch > 'Z')
    }
  }
    
  /** Returns the ratio of unguessed letters to the word length
    * @param word the word to evaluate
    * @return a ratio (a double)
    */
  def getWordScore(word: String): Double = {
    getUnguessedLetterCount(word).toDouble/word.length().toDouble
  }

  /** Returns a set of characters that appear at least twice in succession 
    * in the given string.
    * @param str the string to evaluate
    * @return a Set of Chars
    */
  def findDoubleLetters(str: String): Set[Char] = {
    var doubledLetters:Set[Char] = Set()
    var previousCharacter = '*'
    for (ch <- str) {
      if (previousCharacter == ch) doubledLetters += ch
      previousCharacter = ch
    }
    doubledLetters
  }
  
  /** Returns a set of N-grams that appear more than once in the given string
    * @param str the string the evaluate
    * @param n the length of n-gram to look for
    * @return a Set of strings 
    */
  def findNgrams(str: String, n: Int): Set[String] = {
    var ngrams:Set[String] = Set()
    var repeatedNgrams:Set[String] = Set()
    var ngram:String = ""
      
    // Evaluate all words in the string long enough to contain an N-gram
    for (word <- parseWords(str) if word.length >= n) {
      // Remember every N-gram so we know if we've seen it before
      for (i <- 0 to word.length - n) {
        ngram = for {
          ch <- word drop i take n
        } yield ch
        if (ngrams contains ngram) repeatedNgrams += ngram
        ngrams += ngram
      }
    }

    repeatedNgrams
  }
  
  /** Returns a set of quadrigrams that appear more than once
    * in the given string 
    * @param str the string to evaluate
    * @return a Set of strings
    */
  def findQuadrigrams(str: String): Set[String] = {
    findNgrams(str, 4)
  }

  /** Returns a set of trigrams that appear more than once
    * in the given string 
    * @param str the string to evaluate
    * @return a Set of strings
    */
  def findTrigrams(str: String): Set[String] = {
    findNgrams(str, 3)
  }

  /** Returns a set of bigrams that appear more than once
    * in the given string 
    * @param str the string to evaluate
    * @return a Set of strings
    */
  def findBigrams(str: String): Set[String] = {
    findNgrams(str, 2)
  }

  
  /** Returns a collection of all 26 uppercase letters, shuffled
    * @return a collection of length 26 Chars 
    */
  def shuffleAlphabet: scala.collection.immutable.IndexedSeq[Char] = {
    val alphabet = for (ch <- 'A' to 'Z') yield ch
    val shuffledAlphabet = Random.shuffle(alphabet)
    // Make sure no letter is encoded as itself
    val selfEncoded = (for (i <- 0 until alphabet.length) yield alphabet(i) == shuffledAlphabet(i)).reduce((a,b) => a || b)
    selfEncoded match {
      case true => shuffleAlphabet
      case false => shuffledAlphabet
    }
  }

  /** Returns a string retaining only letters and spaces (no punctuation)
    * @param str the string from which to remove punctuation
    * @return a string of letters and spaces 
    */
  def removePunctuation(str: String): String = {
    // filter the new string for A-Z, a-z, and space characters
    for (
        ch <- str 
        if ch == ' ' || 
        (ch >= 'A' && ch <= 'Z') ||
        (ch >= 'a' && ch <= 'z')
    ) yield ch    
  }
  
  /** Returns an array of strings (words)
    * @param str the string to parse
    * @return an array of strings
    */
  def parseWords(str: String): Array[String] = {
    // split("\\W+") will split on "?", not what we want
    // [^A-Za-z'//?]+ will split on anything that is not a:
    // letter, apostrophe, or underscore
    str.split("[^A-Za-z_']+")
  }
  
  /** Returns a string where each letter from the given string is replaced
    * with a corresponding letter from guess, or by a question mark 
    * @param str the encoded string in which to replace letters
    * @param code the encoded letters to replace
    * @param guess the replacement letters
    * @return a string
    */
  def substituteLetters(str: String, code: String, guess: String): String = {
    require(code.length == guess.length)
    val substitutionStr = str.map((s: Char) => s match {
      case s if code.indexOf(s) >= 0 => guess.charAt(code.indexOf(s))
      case s if s < 'A' || s > 'Z' => s
      case _ => unknownChar
    })
    substitutionStr
  }
  
  /** Returns a random string from a collection of strings */
  def getRandomQuote: String = {
    val quotes = loadQuotes.toArray
    quotes(Random.nextInt(quotes.length))
  }

  /** Returns a random cryptogram from the available quotes */
  def getCryptogram: String = {
    encrypt(getRandomQuote)
  }
  
  /** Returns a string, encrypted with a random substitution cipher
    * Converts all lowercase letters to uppercase.
    * All spaces and punctuation characters are left unchanged 
    * @param str the string to encrypt
    * @return the encrypted string 
    */
  def encrypt(str: String):String = {
    val shuffledAlphabet = shuffleAlphabet
    str.toUpperCase() map ((ch:Char) =>
      ch match {
        case s if s >= 'A' && s <= 'Z' => shuffledAlphabet(s.toInt - 65)
        case _ => ch
      }  
    )
  }

  /** Returns an Int of the number of occurrences in the given string of the given character
    * @param str the string to evaluate
    * @param ch the character to count
    * @return an integer
    */
  def characterCount(str: String, ch: Char): Int = {
    var count = 0
    for (c <- str if c == ch) count += 1
    count
  } 
  
  /** Returns a string of characters representing the frequency of letters in the given string
    * @param str the string to evaluate 
    * @return a string of characters from highest frequency to lowest frequency
    */  
  def getFrequencyString(str: String): String = {
    var freqArray = Array[Char]()
    var freqString = ""
    for (ch <- str if ch >= 'A' && ch <= 'Z') 
      if (!(freqArray contains ch)) freqArray = freqArray :+ ch
    // Sort the string
    freqArray = freqArray.sortBy((ch => characterCount(str,ch))).reverse
    for (ch <- freqArray) freqString = freqString + ch.toString
    freqString
  }

  /** Returns the number of words in the given string that are in the dictionary
    * @param str the string to evaluate
    * @return the number of words with dictionary matches 
    */
  def getCorrectWordCount(str: String) = {
    parseWords(str).map(
      (word => 
        word match {
          case w if Dictionary.isIn(word) => 1
          case _ => 0
        }
      )  
    ).sum
  }
  
  /** Returns true if all the words in the given string are in the dictionary
    * @param str the string to evaluate
    * @return true if the words are in the dictionary 
    */
  def isSolved(str: String): Boolean = {
    parseWords(str).map(
        (word => Dictionary.isIn(word))
    ).reduce(
        (a,b) => a && b
    )
  }


  /** Returns true if any of the words in the given string contain a bigram that has no dictionary match
    * @param str the string to evaluate
    * @return true if any of the words contain a bigram with no dictionary match
    */
  def isFail(str: String): Boolean = {
    !(for (word <- parseWords(str)) yield 
      word.sliding(2,1).forall(
        // Ignore bigrams that contain an unknown letter,
        // or a punctuation mark,
        // or in a single-letter word!
        pair => if (pair.length != 2 || 
                    pair(0) < 'A' || 
                    pair(0) > 'Z' || 
                    pair(1) < 'A' || 
                    pair(1) > 'Z') true
                else Dictionary.findBigram(pair) 
      )
    ).reduce(
      (a,b) => a && b
    )
  }

  
  /** find word with lowest score */
  def findLowestScoreWord(str: String): String = {
    parseWords(str) reduce (
      (word1, word2) => if (getWordScore(word1) == 0 || (getWordScore(word2) != 0 && getWordScore(word2) < getWordScore(word1))) {
        word2
      } else {
        word1
      }
    )
  }

  /** tryWords 
    * This part looks at works that are partially complete.
    * It finds possible matches in the dictionary and tries them.
    * @param str the cryptogram string to be solved
    * @param code the code letters to substitute
    * @param guess the corresponding guesses to the code letters
    * @param letters the list of characters to try
    * @return the solved cryptogram
    */
  def tryWords(str: String, code: String, guess: String, letters: String): Option[String] = {
    var solution: Option[String] = None
    val substitutionString = substituteLetters(str, code, guess)
    val lettersNotGuessed = letters.filterNot((ch => guess contains ch))
    
    // Find the word with the lowest non-zero score
    val lowScoreWord = findLowestScoreWord(substitutionString)
    
    // Based on the score, this word is most likely to give us results?
    if (getWordScore(lowScoreWord) > 0 && getWordScore(lowScoreWord) < 1) {
      // Get all possible matches from the dictionary
      val wordsToGuess = Dictionary.allPossibleWords(lowScoreWord)
      // If there are too many, we may as well guess more letters, right?
      // TODO: improve this logic, no magic numbers
      if (wordsToGuess.length < 10 && wordsToGuess.length > 0) {
        for (word <- wordsToGuess if solution == None) {
          // Hmm, it would be nice if we knew which word it was
          val newCode = (code + parseWords(str)(parseWords(substitutionString).indexOf(lowScoreWord))).distinct
          val newGuess = (guess + word).distinct
          // If the word we are guessing already contains a letter we've guessed, reject it
          if (newCode.length == newGuess.length) {
            solution = solve(str, newCode, newGuess, lettersNotGuessed.filterNot((ch => newGuess contains ch)))              
          }
        }
      }
    }
    solution
  }

 
  /** try frequent letters 
    * This part tests letters in our frequent letters list.
    * It should only run if the partial word tests above did not find a solution.
    * @param str the cryptogram string to be solved
    * @param code the code letters to substitute
    * @param guess the corresponding guesses to the code letters
    * @param letters the list of characters to try
    * @return the solved cryptogram
    */
  def tryFrequentLetters(str: String, code: String, guess: String, letters: String): Option[String] = {
    var solution: Option[String] = None
    val substitutionString = substituteLetters(str, code, guess)
    val lettersNotGuessed = letters.filterNot((ch => guess contains ch))
    val freq = cryptogramFrequencyString

    // Loop over the letters we have yet to try
    for (i <- 0 until lettersNotGuessed.length if solution == None) {
      // New code is existing code and the first letter from the frequency list that isn't in the code
      val newCode = code + freq(freq.indexWhere((ch => !(code contains ch))))
      val newGuess = guess + lettersNotGuessed(i)
      // TODO: if there is more than one valid solution within this loop, solution will contain both!
      solution = solve(str, newCode, newGuess, lettersNotGuessed.take(i) + lettersNotGuessed.drop(i + 1))
    }
    solution
  }


  /** Returns a string of the solved cryptogram
    * This attempts to solve the puzzle recursively, by substituting a letter
    * from the letters Vector and trying to solve the result.
    * @param str the cryptogram string to be solved
    * @param code the code letters to substitute
    * @param guess the corresponding guesses to the code letters
    * @param letters the list of characters to try
    * @return the solved cryptogram
    */
  def solve(str: String, code: String, guess: String, letters: String): Option[String] = {
    val substitutionString = substituteLetters(str, code, guess)
    val lettersNotGuessed = letters.filterNot((ch => guess contains ch))
    val freq = cryptogramFrequencyString

    // Check to see if this is our best guess so far
    val correctWordCount = getCorrectWordCount(substitutionString)
    if (correctWordCount > correctWordsInBestGuess) {
      bestGuess = substitutionString
      correctWordsInBestGuess = correctWordCount
    }
    
    // Base case: the cryptogram is already solved!
    if (isSolved(substitutionString)) Some(substitutionString)
    // Base case: the proposed solution is a failure and should be rejected
    /* This can happen a couple ways:
     * - A known bad case (e.g. a word or letter combo not in our dictionary)
     * - We have a guess for every letter in the original string, but it isn't the solution
     */
    // This can happen if we find a known bad case
    else if (isFail(substitutionString) || code.length == freq.length) None
    // Do a little work, then call solve recursively 
    else {
      var solution: Option[String] = tryWords(str, code, guess, letters)
      solution match {
        case Some(s) => Some(s)
        case None => tryFrequentLetters(str, code, guess, letters)
      }
    }
  }

  /** Overloaded method, calls solve in initial state: no letters guessed, starting with most common letters
    * 
    */
  def solve(str: String): Option[String] = {
    //val code = removePunctuation(str)
    val code = str
    var solution: Option[String] = None
    // Reset best guess
    bestGuess = code
    correctWordsInBestGuess = 0
    lastCorrectWordCount = 0
    // Calculate frequency string
    cryptogramFrequencyString = getFrequencyString(code)

    // Try trigrams
    /*
    val trigrams = findTrigrams(code)
    for (trigram <- trigrams if solution == "") {
      for (commonTrigram <- commonTrigrams if solution == None) {
        solution = solve(code, trigram, commonTrigram, frequentLetters.filterNot((letter => commonTrigram.contains(letter.toString))))
      }
    }
    */
    
    // Try bigrams
    val bigrams = findBigrams(code)
    for (bigram <- bigrams if solution == "") {
      for (commonBigram <- commonBigrams if solution == None) {
        solution = solve(code, bigram, commonBigram, frequentLetters.filterNot((letter => commonBigram.contains(letter.toString))))
      }
    }

    // Try frequent letters
    if (solution == None) solution = solve(code, "", "", frequentLetters)
    if (solution == None) solution = Some(bestGuess)
    
    solution
  }

  /** Returns a string of letters that do not occur in the given string
    * @param str the string to evaluate
    * @return a string
    */
  def getMissingLetters(str: String) = {
    var alphabet = ""
    for (ch <- 'A' to 'Z') alphabet = alphabet + ch
    alphabet.diff(str.toUpperCase)
  }
  
  /** Returns a string of letters, replacing a placeholder in the given string with unused letters
    * @param str a string that may contain question marks
    * @return a string without question marks
    */
  def fillInMissingLetters(str: String) = {
    var updatedString = str
    var missingLetters = getMissingLetters(str)
    // Replace the placeholder with missing letters
    var i = 0
    for (j <- 0 until updatedString.length) {
      if (updatedString(j) == unknownChar) {
        updatedString = updatedString.updated(j,missingLetters(i))
        i = i + 1
      }
    }
    updatedString
  }
  
  /** Given a plain text and the corresponding encodedText,
    *  generate the code map.
    *  Non-letters (i.e. "?") are not added into code map.
    * @param encodedText the encoded sentence
    * @return code in type Map  
    */
  def getCodeMap(encodedText: String): Map[Char, Char] = {
    var plainText = ""
    var codeMap: Map[Char, Char] = Map()
    solve(encodedText) match {
      case Some(s) => plainText = s
      case _ => plainText = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    }
    for (i <- 0 to encodedText.length - 1 if plainText(i).isLetter) {
      codeMap += (plainText(i) -> encodedText(i))
    }
    
    codeMap
  }
  
  /** Generate code in type String
    * Unknown codes are shown as a placeholder character
    * @param message the encoded sentence
    * @return a tring of code  
    */
  def getCodes(message: String): String = {
    val codeMap = getCodeMap(message)
    var codeString = ""
    for (i <- 65 to 90) {
      codeString += codeMap.getOrElse(i.toChar, unknownChar)
    }
    
    fillInMissingLetters(codeString)
  }  
  
  /** Returns all lines from a file
    * @return a collection of strings
    */
  def getFile: Iterator[String] = {
    var fileContents = Vector("Error")
    val chooser = new FileChooser
    val response = chooser.showOpenDialog(null)
    if (response == FileChooser.Result.Approve) Source.fromFile(chooser.selectedFile).getLines
    else Iterator("An error occurred reading the file.")
  }


  /** Returns all lines from the given file
    * Overloaded method 
    * @returns a collection of strings
    */
  def getFile(path: String): Iterator[String] = {
    val file = new File(path)
    Source.fromFile(file).getLines
  }

  
  def loadDictionary:Iterator[String] = {
    /* We could ask the user to pick a file...
    println("Please select dictionary file")
    getFile
    */
    // Specify a file
    getFile("dictionary.txt")
  }

  
  def loadQuotes:Iterator[String] = {
    getFile("quotes.txt")
  }

}