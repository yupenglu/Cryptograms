/** Cryptograms - Assignment #6
  * CIT 591, Fall 2013
  * @author Chris Herdt
  * @author Yupeng Lu
  */

package cryptograms

import org.scalatest.FunSuite

class DictionaryTest extends FunSuite {

  test("test allPossibleWords") {
    assert(Dictionary.allPossibleWords("_OUR") contains "YOUR")
    assert(Dictionary.allPossibleWords("___ING") contains "MOVING")
    assert(Dictionary.allPossibleWords("D__R") contains "DOOR")
  }
  
  /** Test dictionary lookups */
  test("dictionary lookup") {
    assert(Dictionary.isIn("TEST"))
    assert(Dictionary.isIn("DON'T"))
    assert(!(Dictionary.isIn("ZXCVB")))
  }
  
  /** Test possible word lookup */
  test("all possible words") {
    var words = Dictionary.allPossibleWords("?OUR")
    assert(words contains "HOUR")
    assert(words contains "YOUR")
  }

  /** Test bigram dictionary */
  test("possible bigrams") {
    assert(Dictionary.findBigram("QQ") == false)
    assert(Dictionary.findBigram("EE") == true)
  }
}
