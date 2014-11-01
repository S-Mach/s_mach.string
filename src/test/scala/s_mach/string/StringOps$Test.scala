package s_mach.string

import StringOps._

import org.scalatest.{Matchers, FlatSpec}
import s_mach.string.WordSplitter.{PascalCase, CamelCase, WhitespaceOrUnderscore, Whitespace}

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

  "ensureSuffix()" should "ensure a string has a suffix" in {
    ensureSuffix(simpleCamelCase, "CamelCase") should equal(simpleCamelCase)
  }

  it should "add a prefix if it is not present" in {
    ensureSuffix(intString, hello) should equal(intString + hello)
  }

  "toProperCase()" should "capitalize the first word of a string" in {
    toProperCase(singleWord) should equal(hello)
  }

  it should "leave strings that begin with a capital letter intact" in {
    toProperCase(hello) should equal(hello)
  }

  it should "ignore empty strings" in {
    val a = ""
    toProperCase(a) should equal(a)
  }

  "toTitleCase()" should "collapse whitespace and make words title cased" in {
    toTitleCase(sentence)(Whitespace) should equal("The Rain In Spain.")
  }

  it should "only modify the first letter of words" in {
    toTitleCase(simpleCamelCase)(Whitespace) should equal("Simplecamelcase")
  }

  it should "ignore empty strings" in {
    val a = ""
    toTitleCase(a)(Whitespace) should equal(a)
  }

  it should "work with different splitters" in {
    toTitleCase(someUnderscores)(WhitespaceOrUnderscore) should equal("Test Variable With Underscores")
    toTitleCase(sentence)(WhitespaceOrUnderscore) should equal("The Rain In Spain.")
    toTitleCase(sentence)(Whitespace) should equal("The Rain In Spain.")
    toTitleCase(simpleCamelCase)(CamelCase) should equal("Simple Camel Case")
    toTitleCase(simpleCamelCase)(PascalCase) should equal("Camel Case")
  }

  "toCamelCase()" should "transform a sequence of strings into camelCase" in {
    toCamelCase(sentence)(Whitespace) should equal("theRainInSpain.")
  }

  it should "adopt the behavior of the splitter" in {
    toCamelCase(simpleCamelCase)(Whitespace) should equal(simpleCamelCase.toLowerCase)
    toCamelCase(simpleCamelCase)(CamelCase) should equal(simpleCamelCase)
    toCamelCase(someUnderscores)(WhitespaceOrUnderscore) should equal("testVariableWithUnderscores")
  }

  it should "ignore empty strings" in {
    val a = ""
    toCamelCase(a)(Whitespace) should equal(a)
    toCamelCase(a)(WhitespaceOrUnderscore) should equal(a)
    toCamelCase(a)(CamelCase) should equal(a)
    toCamelCase(a)(PascalCase) should equal(a)
  }

  "toSnakeCase()" should "transform a sequence of strings into snake_case" in {
    toSnakeCase(sentence)(Whitespace) should equal ("the_rain_in_spain.")
  }

  it should "adopt the behavior of the splitter" in {
    toSnakeCase(simpleCamelCase)(Whitespace) should equal(simpleCamelCase.toLowerCase)
    toSnakeCase(simpleCamelCase)(CamelCase) should equal("simple_camel_case")
    toSnakeCase(pascalCase)(PascalCase) should equal("pascal_case")
    toSnakeCase("A_B")(WhitespaceOrUnderscore) should equal("a_b")
  }

  "toPascalCase()" should "transform a sequence of strings into PascalCase" in {
    toPascalCase(simpleCamelCase)(CamelCase) should equal("SimpleCamelCase")
  }

  "collapsWhitespace()" should "remove all extraneous white space" in {
    collapseWhitespace("   space and    more space!   ") should equal("space and more space!")
  }

  "findRegexReplaceMatch()" should "replace all Matches from a sequence of regexes to a paired string" in {
    val matches = Seq(("rain".r, spainMatchFunction), ("spain".r, spainMatchFunction))
    findRegexReplaceMatch(sentence, matches) should equal("The heavy rain in Spain.")
  }

  it should "not perform recursive replacements" in {
    val matches = Seq(("bar".r,fooMatchFunction),("foo".r,fooMatchFunction))
    findRegexReplaceMatch("bar foo", matches) should equal("foo bla")
  }

  it should "pass this test" in {
    val matches = Seq(("Int".r,scalaMatchFunction),("\\.".r,scalaMatchFunction))
    findRegexReplaceMatch("var a : Int = String.mkString", matches) should equal("var a : String = String mkString")
  }

  "findRegexReplace()" should "replace all matches of a sequence of regexes with a paired string" in {
    val matches = Seq(("rain".r, "heavy rain"), ("spain".r, "Spain"))
    findRegexReplace(sentence, matches) should equal ("The heavy rain in Spain.")
  }

  it should "not perform recursive replacements" in {
    val matches = Seq(("bar".r,"foo"),("foo".r,"bla"))
    findRegexReplace("bar foo", matches) should equal("foo bla")
  }

  it should "pass this test" in {
    val matches = Seq(("Int".r,"String"),("\\.".r," "))
    findRegexReplace("var a : Int = String.mkString", matches) should equal("var a : String = String mkString")
  }



}
