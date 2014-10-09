package s_mach.string

import StringOps._

import org.scalatest.{Matchers, FlatSpec}

/**
 * Test suite for StringOps
 *
 * @author Gustavo Hidalgo
 * @version 1.0
 */
class StringOps$Test extends FlatSpec with Matchers with TestStrings{


  "indent()" should "add an indentation format" in {
    val initString ="a"
    indent(initString,4," ") should equal (
      "    a"
    )
    it should not equal {
      "a"
    }
    val initString2 ="a\nb"
    indent(initString2,4," ") should equal (
      "    a\n    b"
    )
    it should not equal {
      "a\nb"
    }
    indent(initString2,4,"a") should equal (
      "aaaaa\naaaab"
    )
  }

  "toOption()" should "convert a non-empty string to a Some(String)" in {
    toOption(hello) should equal(Some(hello))
  }
  it should "give None on an empty String" in {
    toOption(emptyString) should equal(None)
  }

  "toDoubleOpt()" should "parse a Double out of a String into a Some(Double)" in {
    toDoubleOpt(doubleString) should equal(Some(1.0))
    toDoubleOpt(intString) should equal(Some(1.0))
  }
  it should "not parse non-doubles" in {
    toDoubleOpt(hello) should equal(None)
    toDoubleOpt(complicated) should equal(None)
    toDoubleOpt(complicated2) should equal(None)
    toDoubleOpt(complicated3) should equal(None)
  }

  "toIntOpt()" should "parse an Integer out of a String into a Some(Integer)" in {
    toIntOpt(intString) should equal(Some(1))
  }
  it should "not parse non-integers" in {
    toIntOpt(doubleString) should equal(None)
    toIntOpt(long) should equal(None)
    toIntOpt(hello) should equal(None)
  }

  "toLongOpt()" should "parse a Double out of a String into a Some(Double)" in {
    toLongOpt(long) should equal(Some(100000000000L))
    toLongOpt(intString) should equal(Some(1L))
  }

  it should "not parse non-Longs" in {
    toLongOpt(hello) should equal(None)
    toLongOpt(doubleString) should equal(None)
  }

  //TODO implement this test
  /*"toTitleCase()" should "collapse whitespace and make words title cased" in {

  }*/

}
