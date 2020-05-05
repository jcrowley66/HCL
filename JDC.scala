import scala.annotation.tailrec

import org.scalatest._

/** *
 *Build a full scala project runnable with sbt with tests (specs or scalatest) to show your ideas work. Write the following:
*1.            Object that takes a generic list and can reverse it using a method that allows tail recursion. The test can implement an instance of any concrete object
*2.            Object that takes a string of similar consecutive letters (all letters must be same case) and transforms this into a sequence consisting of the count of each consecutive letter followed by that letter. If a letter only appears once, do not apply a count. This should also demonstrate tail recursion. Also write a method that reverses this so that the output of the first method ran with this method will return the original result.
              *e.g. given AAAAABBBCCC map this to 5A3B3C
              *if you have some singles do not show "1", so AAAAABCDDD maps to 5ABC3D
             *the reverse maps the right side to the left.?????
 */

/** Notes:
 * -- Check out jdcrowley.net for a working Scala app using Play (also a useful utility).
 *    (Now needs at least JRE 1.8, not 1.7 -- they repackaged 1.7 so some libraries which
 *     used to be included are no longer in the base rt.jar)
 *
 */
class JDC {

  /** Reverse a generic list using the built-in functionality */
  def reverseEasy(seq: Seq[Any]): Seq[Any] = seq.reverse

  /** Reverse a generic list explicitly using tail-recursion */
  def reverseTailRec(seq: Seq[Any]): Seq[Any] = {
    @tailrec
    def tailRec(result: Seq[Any], seq: Seq[Any]): Seq[Any] =
      if (seq.isEmpty) result
      else tailRec(seq.head +: result, seq.tail)

    tailRec(Nil, seq)
  }

  /** Converts a string of >1 consecutive letters to nLetter - e.g. AAABBCDDD = 3A2BC3D
   * Note: Case-sensitive, ASSUMES that only letters appear!!!
   */
  def consecutiveLetters(str: String): String = {
    @tailrec
    def cons(result: String, workingOn: String, remains: String): String =
      if (remains.isEmpty) result + (if (workingOn.length == 1) "" else workingOn.length.toString) + workingOn.head
      else if (workingOn.head == remains.head)
        cons(result, workingOn + remains.head, remains.tail)
      else {
        cons(result + (if (workingOn.length == 1) "" else workingOn.length.toString) + workingOn.head, remains.take(1), remains.tail)
      }

    if (str.isEmpty) "" else cons("", str.take(1), str.tail)
  }

  /** Reverse the operation of the consecutiveLetters method
   * SIMPLIFICATION -- Assumes only a single-digit count appears + well-formed input
   */
  def reverseLetters(str: String): String = {
    def rev(result: String, input: String): String =
      if (input.isEmpty) result
      else if (Character.isDigit(input.head))
        rev(result + (input.drop(1).take(1)) * input.take(1).toInt, input.drop(2))
      else
        rev(result + input.head, input.tail)

    rev("", str)
  }
}

class JDCTest extends FunSuite {
  val jdc = new JDC()

  val lst = List("A", "B", "C", "1", "2", "3")
  val rLst = List("3", "2", "1", "C", "B", "A")

  val letters1       = "AAAAABBBCCC"
  val lettersResult1 = "5A3B3C"

  val letters2       = "AAAAABCDDD"
  val lettersResult2 = "5ABC3D"

  test("Check easy reverse"){
    assert(rLst == jdc.reverseEasy(lst))
  }
  test("Check tail recursion reverse"){
    assert(rLst == jdc.reverseTailRec(lst))
  }
  test("Check simple reverse vs tail recursion"){
    assert(jdc.reverseEasy(lst) == jdc.reverseTailRec(lst))
  }
  test( "Check double list reverse"){
    assert(lst == jdc.reverseTailRec(jdc.reverseTailRec(lst)))
  }

  test( "Basic letters to nLetter"){
    assert(lettersResult1 == jdc.consecutiveLetters(letters1))
  }

  test( "Letters + single letter no digit 1"){
    assert(lettersResult2 == jdc.consecutiveLetters(letters2))
  }

  test( "Basic letters and reverse process"){
    assert(letters1 == jdc.reverseLetters(jdc.consecutiveLetters(letters1)))
  }
}
