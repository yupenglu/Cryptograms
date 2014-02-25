/** Cryptograms - Assignment #6
  * CIT 591, Fall 2013
  * @author Chris Herdt
  * @author Yupeng Lu
  */

package cryptograms

object Dave {

  def main(args: Array[String]): Unit = {}
     
  def encodeHelper(n: Char, code: String): Char = {
    if (n.isLetter) code(n - 65).toChar
    else n
  }
  
  def decodeHelper(n: Char, code: String): Char = {
    if (n.isLetter) (code.indexOf(n) + 65).toChar
    else n
  }
  
  /**
  * No letter should be encoded as itself. 
  * Return true if there is a letter encoded as itself.
  */
  def isSelfEncoded(code: String): Boolean = {
    var result = false
    val codeUpperCase = code.toUpperCase
    for (i <- 0 to code.length - 1 if !result) {
      result = (codeUpperCase(i) == (i + 65).toChar)
    }
    result
  }
  
  /**
   * Encodes the plainText message and returns the coded message. 
   * Encoding means replacing each 'A' in the message with the 
   * first letter in the code, each 'B' with the second letter 
   * in the code, etc. The code should be exactly 26 letters long. 
   * No letter should be encoded as itself. Before doing anything else, 
   * all lowercase letters in the arguments should be replaced by the 
   * corresponding uppercase letters. Non-letters should be unchanged.
   */
  def encode(plainText: String, code: String): String = {
    val textUpperCase = plainText.toUpperCase
    val codeUpperCase = code.toUpperCase
    if (isSelfEncoded(code)) "This code has letters that are encoded to itself."
    else textUpperCase.map((n: Char) => encodeHelper(n, codeUpperCase))
  }
  
  /**
   * This method is the inverse of encode: The 26-letter code String 
   * is used in the "opposite direction." What is encoded with a 
   * given code can be decoded with the same code. Also, the decode 
   * method can assume neither the encodedText nor the code contains 
   * any lowercase letters.
   */
  def decode(encodedText: String, code: String): String = {
    encodedText.map((n: Char) => decodeHelper(n, code))
  }

  /**
   * Given a cryptogram, tries to determine the code that was used. 
   * The return value should be a String consisting of exactly 26 
   * uppercase letters. If the cryptogram (encoded message) and the 
   * return value are given to the decode method, the result should 
   * be the original, plain-text message (or as close to it as 
   * possible).
   */
  def discoverCode(message: String): String = {
    Cryptograms.getCodes(message)
  }
}