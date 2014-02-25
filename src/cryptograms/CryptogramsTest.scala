/** Cryptograms - Assignment #6
  * CIT 591, Fall 2013
  * @author Chris Herdt
  * @author Yupeng Lu
  */

package cryptograms

import org.scalatest.FunSuite
import scala.util.Random

class CryptogramsTest extends FunSuite {

  /** Test method to count unguessed letters in a word/string */
  test("get unguessed letter count") {
    assert(Cryptograms.getUnguessedLetterCount("_B_B_B_") == 4)
    assert(Cryptograms.getUnguessedLetterCount("ABCDE") == 0)
  }
  
  /** Test method to get a word's score, a ratio of unguessed letters to length */
  test("get word score") {
    assert(Cryptograms.getWordScore("_ABCDE") == 1.0/6.0)
    assert(Cryptograms.getWordScore("ABCDEF") == 0)
    assert(Cryptograms.getWordScore("______") == 1.0/1.0)
  }
  
  test("find double letter") {
    val s1 = Set('p','t','o','k','e')
    assert(Cryptograms.findDoubleLetters("happy settle bookkeeper") == s1)
  }
  
  /** Test finding repeated N-grams (bigrams, trigrams, etc.) in strings */
  test("find N-grams") {
    val quote = "the night might have the stars, but the day gave us the light."
    assert(Cryptograms.findNgrams(quote,2) contains "th")
    assert(Cryptograms.findNgrams(quote,3) contains "ave")
    assert(Cryptograms.findNgrams(quote,4) contains "ight")
    
  }

  /** Test finding repeated quadrigrams in strings */
  test("find quadrigrams") {
    val quote = "The night might have the stars, but the day gave us the light."
    val code = Cryptograms.encrypt(quote)
    val s1 = Set("ight")
    assert(Cryptograms.findQuadrigrams(quote) == s1)
    assert(Cryptograms.findQuadrigrams(quote).size == Cryptograms.findQuadrigrams(code).size)
  }

  /** Test finding repeated trigrams in strings */
  test("find trigrams") {
    val quote = "The night might have the stars, but the day gave us the light."
    val code = Cryptograms.encrypt(quote)
    assert(Cryptograms.findTrigrams(quote) contains "igh")
    assert(!(Cryptograms.findTrigrams(quote) contains "ars"))
    assert(Cryptograms.findTrigrams(quote).size == Cryptograms.findTrigrams(code).size)
  }

  /** Test finding repeated bigrams in strings */
  test("find bigrams") {
    val quote = "The night might have the stars, but the day gave us the light."
    val code = Cryptograms.encrypt(quote)
    assert(Cryptograms.findBigrams(quote) contains "ig")
    assert(!(Cryptograms.findBigrams(quote) contains "da"))
    assert(Cryptograms.findBigrams(quote).size == Cryptograms.findBigrams(code).size)
  }

  
  test("shuffle alphabet") {
    // How do we test to make sure something is actually shuffled_
    // Make sure it is 26 characters
    assert(Cryptograms.shuffleAlphabet.length == 26)
  }
  
  test("remove punctuation") {
    val s = "/Earth is 98% full...  Please delete anyone you can."
    assert(Cryptograms.removePunctuation(s) == "Earth is  full  Please delete anyone you can")
  }
  
  test("parse words") {
    val s = "EARTH IS FULL PLEASE DELETE ANYONE YOU CAN" split " "
    val t = Cryptograms.parseWords(Cryptograms.removePunctuation("/Earth is 98% full...  Please delete anyone you can.".toUpperCase()))
    val wordsMatch = for (i <- 0 until s.length) yield s(i) == t(i)
    assert(wordsMatch.forall(a => a))
  }

  test("get cryptogram") {
    /* We should probably additionally test this by making sure that the encoded quote
       can be turned back in to the original quote */
    val randomQuote = Cryptograms.getRandomQuote
    val encodedQuote = Cryptograms.encrypt(randomQuote)
    assert(randomQuote.length() == encodedQuote.length())
  }
  
  /** Encrypt a quote. Only the letters should be encrypted */
  test("encrypt quote") {
    assert(Cryptograms.encrypt("Hello world").length == 11)
    assert(Cryptograms.encrypt("Hello world") != "Hello world")
    assert(Cryptograms.encrypt("Who goes there_!").charAt(3) == ' ' &&
           Cryptograms.encrypt("Who goes there_!").charAt(8) == ' ' &&
           Cryptograms.encrypt("Who goes there_!").charAt(14) == '_' &&
           Cryptograms.encrypt("Who goes there_!").charAt(15) == '!')
  }

  test("substitute letters") {
    assert(Cryptograms.substituteLetters("ZXCCV BVNCM","ZXCVBNM","HELOWRD") == "HELLO WORLD")
    assert(Cryptograms.substituteLetters("ZXCCV BVNCM","ZXBN","HEWR") == "HE___ W_R__")  
  }
  
  /** count occurrences of a character in a string */
  test("character count") {
    assert(Cryptograms.characterCount("MISSISSIPPI",'M') == 1)
    assert(Cryptograms.characterCount("MISSISSIPPI",'I') == 4)
    assert(Cryptograms.characterCount("MISSISSIPPI",'S') == 4)
    assert(Cryptograms.characterCount("MISSISSIPPI",'P') == 2)
  }
  
  /** generating a frequency string */
  test("frequency string") {
    assert(Cryptograms.getFrequencyString("I ATE ONE TOO").startsWith("O"))
  }
  
  /** how many of the words in the given string are in the dictionary? */
  test("test count of dictionary words") {
    assert(Cryptograms.getCorrectWordCount("S_DDENL_ THE FLOOR STOPPED _OR__N_") == 3)
    assert(Cryptograms.getCorrectWordCount("SUDDENLY THE FLOOR STOPPED WORKING") == 5)
  }

  /** is the current string in a solved state? */
  test("is the current string solved, e.g. are all the words in the dictionary?") {
    assert(Cryptograms.isSolved("I ONE TOO NO"))
  }

  /** is the current string in a failed state? */
  test("is failed solution?") {
    assert(Cryptograms.isFail("____ __QQ__ ____"))
    assert(Cryptograms.isFail("SUDDENLY THE FLQQR STQPPED WQRKING"))
    assert(!Cryptograms.isFail("LIFE ISN'T FAIR"))
    assert(!Cryptograms.isFail("SUDDENLY THE FLOOR STOPPED WORKING"))
  }
  
  /** test lowest score word: should be word with highest percentage of letters present */
  test("lowest word score") {
    assert(Cryptograms.findLowestScoreWord("S_DDENL_ THE FLOOR STOPPED _OR__N_") == "S_DDENL_")
    assert(Cryptograms.findLowestScoreWord("SUDDENLY THE FLOOR STOPPED _OR__N_") == "_OR__N_")
    
  }

  /** test filling in missing letters */
  test("fill in missing letters") {
    assert(Cryptograms.fillInMissingLetters("__________________________") == "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
    assert(Cryptograms.fillInMissingLetters("A_C_E_G_I_K_M_O_Q_S_U_W_Y_") == "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
    assert(Cryptograms.fillInMissingLetters("ABCDEFGHIJKLMNOPQRSTUVWXYZ") == "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
  }
  
  /** Try to solve a simple puzzle */
  test("solve a puzzle") {
    // This is a bad/good example, 2 possible matches: "A ONE TOO NO" and "I ONE TOO NO"
    // assert(Cryptograms.solve("A BCD EBB CB") == "I ONE TOO NO")

    // We know these words are in the dictionary and are composed of frequent letters 
    assert(Cryptograms.solve("BCD EBB CB") == Some("ONE TOO NO"))
    // Include some words twice so to trigger bigrams
    assert(Cryptograms.solve("ZXZC VZBX ZXZC VZBX VBQ QBNBC QBNBC") == Some("NONE INTO NONE INTO ITS STATE STATE"))
    // Try passing solve a known good bigram
    assert(Cryptograms.solve("ZXZC VZBX ZXZC VZBX VBQ QBNBC QBNBC", "XZ", "ON", "ETAIS") == Some("NONE INTO NONE INTO ITS STATE STATE"))
    // This would be interesting to test by excluding 'S' from the frequent letters list,
    // as the program should still be able to figure out "NONE INTO IT_ _TATE" on its own
    assert(Cryptograms.solve("ZXZC VZBX VBQ QBNBC") == Some("NONE INTO ITS STATE"))
    assert(Cryptograms.solve(Cryptograms.encrypt("THE NONE INTO ITS STATE")) == Some("THE NONE INTO ITS STATE"))
    assert(Cryptograms.solve("OPUUYGVD QEY IVRRB OQRCCYU") == Some("SUDDENLY THE FLOOR STOPPED"))
    assert(Cryptograms.solve("OPUUYGVD QEY IVRRB OQRCCYU KRBXJGZ") == Some("SUDDENLY THE FLOOR STOPPED WORKING"))
    assert(Cryptograms.solve("OPUUYGVD QEY IVRRB OQRCCYU KRBXJGZ.") == Some("SUDDENLY THE FLOOR STOPPED WORKING."))

    // "WINE" is not in our dictionary, but this should still produce some result.
    assert(Cryptograms.solve("DRINK NO WINE BEFORE ITS TIME") != None)
    // Test one with an apostrophe
    assert(Cryptograms.solve(Cryptograms.encrypt("SUDDENLY THE FLOOR WASN'T WORKING")) == Some("SUDDENLY THE FLOOR WASN'T WORKING"))
    
    // Create a cryptogram of random words
    var randomPuzzle = ""
    for (i <- 1 to 10) randomPuzzle = randomPuzzle + " " + Dictionary.dicArray(Random.nextInt(Dictionary.dicArray.length))
    assert(Cryptograms.solve(Cryptograms.encrypt(randomPuzzle.trim)) == Some(randomPuzzle.trim))
    
  }


  /* this doesn't actually test anything, but will prompt the tester to select a file */
  /*
  test("get file") {
    Cryptograms.getFile
  }
  */
  

  
  test("load dictionary") {
    // This assumes a specific dictionary file is selected
    val dictionary = Cryptograms.loadDictionary
    val word = dictionary.next
    assert(word == "1\tthe")
  }

  test("load quotes") {
    // This assumes a specific quotes file is selected
    val quotes = Cryptograms.loadQuotes
    val quote = quotes.next
    assert(quote == "b \"Virtual\" means never knowing where your next byte is coming from.")
  }


}