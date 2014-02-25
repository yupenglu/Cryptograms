/** Cryptograms - Assignment #6
  * CIT 591, Fall 2013
  * @author Chris Herdt
  * @author Yupeng Lu
  */

package cryptograms
import org.scalatest._

class DaveTest extends FunSuite{

  test("encode") {
    assert(Dave.encode("abcd, efghi!!", "QWERTYUIOPASDFGHJKLZXCVBNM") == "QWER, TYUIO!!")
    assert(Dave.encode("abcd, efghi!!", "AWERTYUIOPQSDFGHJKLZXCVBNM") == "This code has letters that are encoded to itself.")
    assert(Dave.encode("QWER, TYUIO!!", "QWERTYUIOPASDFGHJKLZXCVBNM") != "abcd, efghi!!")
  }
  
  test("decode") {
    assert(Dave.decode("QWER, TYUIO!!", "QWERTYUIOPASDFGHJKLZXCVBNM") == "ABCD, EFGHI!!")
    assert(Dave.decode("ABCD, EFGHI!!", "QWERTYUIOPASDFGHJKLZXCVBNM") != "QWER, TYUIO!!")
  }

  test("discover code") {
    assert(Dave.discoverCode(Cryptograms.encrypt("THIS IS A TEST")).length == 26)
    // discoverCode needs to return 26-uppercase letters, per the specification
    //assert(Dave.discoverCode("ZXZC VZBX VBQ QBNBC") == "N???C???V????ZX???QB??????")
    assert(Dave.discoverCode("ZXZC VZBX VBQ QBNBC") ==   "NADECFGHVIJKLZXMOPQBRSTUWY")    
  }
}