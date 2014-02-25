/** Cryptograms - Assignment #6
  * CIT 591, Fall 2013
  * @author Chris Herdt
  * @author Yupeng Lu
  */

package cryptograms

import scala.io.Source, scala.io.Source._
import scala.swing.FileChooser, java.io.File

/**
 * This is object Dictionary.
 * able to choose raw files as dictionary
 * method isIn, possibleWords can be used outside
 * !! all things here should use upper case letters.
 */
object Dictionary {
  
  var dicArray = new Array[String](0)
  var bigrams: Set[String] = Set()

  // Uncomment the following to let the user pick the dictionary
  /*
  val chooser = new FileChooser
  val response = chooser.showOpenDialog(null)
  if (response == FileChooser.Result.Approve) {
    for (line <- Source.fromFile(chooser.selectedFile).getLines) {
      // Discarding the number by using ".apply(1)"
      dicArray = dicArray :+ line.split("\\s+").apply(1).toUpperCase
    }
    populateBigrams
  }
  */

  // Read the dictionary
  for (line <- Source.fromFile("dictionary.txt").getLines) {
    // Discarding the number by using ".apply(1)"
    dicArray = dicArray :+ line.split("\\s+").apply(1).toUpperCase    
  }
  populateBigrams
  
  /** Iterate over all words in the dictionary and add each pair
    * of letters to the bigrams set.
    */
  def populateBigrams = {
    for (word <- dicArray) {
      for (bigram <- word.sliding(2,1)) bigrams += bigram
    }
  }

  /** Returns true if the bigram exists in the dictionary */
  def findBigram(bigram: String): Boolean = {
    if (bigrams contains bigram) true
    else false
  }

  
  /**
   * Determine whether the given word has a exact match in the dictionary
   */
  def isIn(input: String): Boolean = {
    dicArray.contains(input)
  }
  
  /**
   * Give an array of string containing the words that are found possible.
   * Upper case letters stands for known letters.
   * Lower case letters are considered to be unknown letters.
   */
  def allPossibleWords(input: String): Array[String] = {
    var tempDicArray = dicArray
    tempDicArray = tempDicArray.filter(_.length == input.length)
    for (n <- 0 to input.length - 1 if input(n).isUpper) {
      tempDicArray = tempDicArray.filter(_(n) == input(n))
    }
    tempDicArray
  }
  
  /**
   * Using lower case letters to indicate patterns.
   * i.e. HAPPY -> abccd; PEOPLE -> abcadb
   */
  def getPattern(input: String): String = {
    var pattern = ""
    var tempLetter = 'a'
    val l = input.length
    var nextLetter = 'a'
    for (i <- 0 to l - 1) {
      if (input.take(i).contains(input(i))) {
        tempLetter = pattern(input.indexOf(input(i)))
        pattern += tempLetter
      } else {
        pattern += nextLetter
        nextLetter = (nextLetter + 1).toChar
      }
    }
    pattern
  }
  
  def possibleWords(input: String): Array[String] = {
    val list = allPossibleWords(input)
    list.filter(n => getPattern(n) == getPattern(input.toUpperCase))
  }
    
}