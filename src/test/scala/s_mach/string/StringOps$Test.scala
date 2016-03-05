/*
                    ,i::,
               :;;;;;;;
              ;:,,::;.
            1ft1;::;1tL
              t1;::;1,
               :;::;               _____       __  ___              __
          fCLff ;:: tfLLC         / ___/      /  |/  /____ _ _____ / /_
         CLft11 :,, i1tffLi       \__ \ ____ / /|_/ // __ `// ___// __ \
         1t1i   .;;   .1tf       ___/ //___// /  / // /_/ // /__ / / / /
       CLt1i    :,:    .1tfL.   /____/     /_/  /_/ \__,_/ \___//_/ /_/
       Lft1,:;:       , 1tfL:
       ;it1i ,,,:::;;;::1tti      s_mach.concurrent
         .t1i .,::;;; ;1tt        Copyright (c) 2016 S-Mach, Inc.
         Lft11ii;::;ii1tfL:       Author: zambrano.hidalgo@gmail.com
          .L1 1tt1ttt,,Li
            ...1LLLL...
*/
package s_mach.string

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
    val initString = "a"
    initString.indent(4, " ") should equal(
      "    a"
    )
    it should not equal {
      "a"
    }

    val initString2 ="a\nb"
    initString2.indent(4," ") should equal (
      "    a\n    b"
    )
    it should not equal {
      "a\nb"
    }
    initString2.indent(4,"a") should equal (
      "aaaaa\naaaab"
    )
  }

  "toOption()" should "convert a non-empty string to a Some(String)" in {
    hello.toOption should equal(Some(hello))
  }
  it should "give None on an empty String" in {
    emptyString.toOption should equal(None)
  }

  "toDoubleOpt()" should "parse a Double out of a String into a Some(Double)" in {
    doubleString.toDoubleOpt should equal(Some(1.0))
    intString.toDoubleOpt should equal(Some(1.0))
  }
  it should "not parse non-doubles" in {
    hello.toDoubleOpt should equal(None)
    complicated.toDoubleOpt should equal(None)
    complicated2.toDoubleOpt should equal(None)
    complicated3.toDoubleOpt should equal(None)
  }

  "toIntOpt()" should "parse an Integer out of a String into a Some(Integer)" in {
    intString.toIntOpt should equal(Some(1))
  }
  it should "not parse non-integers" in {
    doubleString.toIntOpt should equal(None)
    long.toIntOpt should equal(None)
    hello.toIntOpt should equal(None)
  }

  "toLongOpt()" should "parse a Double out of a String into a Some(Double)" in {
    long.toLongOpt should equal(Some(100000000000L))
    intString.toLongOpt should equal(Some(1L))
  }

  it should "not parse non-Longs" in {
    hello.toLongOpt should equal(None)
    doubleString.toLongOpt should equal(None)
  }

  "ensureSuffix()" should "ensure a string has a suffix" in {
    simpleCamelCase.ensureSuffix("CamelCase") should equal(simpleCamelCase)
  }

  it should "add a prefix if it is not present" in {
    intString.ensureSuffix(hello) should equal(intString + hello)
  }

  "mapWords" should "map only words in a string" in {
    sentence.mapWords(s => s + '!')(Whitespace) should equal("  The! rain! in!  spain.!  ")
  }

  "toProperCase()" should "capitalize the first word of a string" in {
    singleWord.toProperCase should equal(hello)
  }

  it should "just capitalize single letters" in {
    "a".toProperCase should equal("A")
  }

  it should "leave strings that begin with a capital letter intact" in {
    hello.toProperCase should equal(hello)
  }

  it should "ignore empty strings" in {
    "".toProperCase should equal("")
  }

  "toTitleCase()" should "make words title cased" in {
    sentence.toTitleCase(Whitespace) should equal("  The Rain In  Spain.  ")
  }

  it should "only modify the first letter of words" in {
    simpleCamelCase.toTitleCase(Whitespace) should equal("Simplecamelcase")
  }

  it should "ignore empty strings" in {
    "".toTitleCase(Whitespace) should equal("")
  }

  it should "work with different splitters" in {
    someUnderscores.toTitleCase(WhitespaceOrUnderscore) should equal("Test_Variable_With_Underscores")
    sentence.toTitleCase(WhitespaceOrUnderscore) should equal("  The Rain In  Spain.  ")
    sentence.toTitleCase(Whitespace) should equal("  The Rain In  Spain.  ")
    simpleCamelCase.toTitleCase(CamelCase) should equal("SimpleCamelCase")
    simpleCamelCase.toTitleCase(PascalCase) should equal("SimpleCamelCase")
  }

  "toCamelCase()" should "transform a sequence of strings into camelCase" in {
    sentence.toCamelCase(Whitespace) should equal("theRainInSpain.")
  }

  it should "adopt the behavior of the splitter" in {
    simpleCamelCase.toCamelCase(Whitespace) should equal(simpleCamelCase.toLowerCase)
    simpleCamelCase.toCamelCase(CamelCase) should equal(simpleCamelCase)
    someUnderscores.toCamelCase(WhitespaceOrUnderscore) should equal("testVariableWithUnderscores")
  }

  it should "ignore empty strings" in {
    "".toCamelCase(Whitespace) should equal("")
    "".toCamelCase(WhitespaceOrUnderscore) should equal("")
    "".toCamelCase(CamelCase) should equal("")
    "".toCamelCase(PascalCase) should equal("")
  }

  "toSnakeCase()" should "transform a sequence of strings into snake_case" in {
    sentence.toSnakeCase(Whitespace) should equal ("the_rain_in_spain.")
  }

  it should "adopt the behavior of the splitter" in {
    simpleCamelCase.toSnakeCase(Whitespace) should equal(simpleCamelCase.toLowerCase)
    simpleCamelCase.toSnakeCase(CamelCase) should equal("simple_camel_case")
    pascalCase.toSnakeCase(PascalCase) should equal("pascal_case")
    "A_B".toSnakeCase(WhitespaceOrUnderscore) should equal("a_b")
  }

  "toPascalCase()" should "transform a sequence of strings into PascalCase" in {
    simpleCamelCase.toPascalCase(CamelCase) should equal("SimpleCamelCase")
  }

  "collapseWhitespace()" should "remove all extraneous white space" in {
    "   space and    more space!   ".collapseWhitespace should equal("space and more space!")
  }

  "collapseGlue()" should "remove all extraneous glue" in {
    "_   space and  _  more space!  _ ".collapseGlue("_")(WhitespaceOrUnderscore) should equal("space_and_more_space!")
  }

  "findRegexReplaceMatch()" should "replace all Matches from a sequence of regexes to a paired string" in {
    val matches = Seq(("rain".r, spainMatchFunction), ("spain".r, spainMatchFunction))
    sentence.findRegexReplaceMatch(matches) should equal("  The heavy rain in  Spain.  ")
  }

  it should "not perform recursive replacements" in {
    val matches = Seq(("bar".r,fooMatchFunction),("foo".r,fooMatchFunction))
    "bar foo".findRegexReplaceMatch( matches) should equal("foo bla")
  }

  it should "ignore empty strings" in {
    val matches = Seq(("bar".r,fooMatchFunction),("foo".r,fooMatchFunction))
    "".findRegexReplaceMatch(matches) should equal("")
  }

  it should "pass this test" in {
    val matches = Seq(("Int".r,scalaMatchFunction),("\\.".r,scalaMatchFunction))
    "var a : Int = String.mkString".findRegexReplaceMatch(matches) should equal("var a : String = String mkString")
  }

  it should "not perform any replacements if no words match the sequence of replacement" in {
    val matches = Seq(("bar".r,fooMatchFunction),("foo".r,fooMatchFunction))
    sentence.findRegexReplaceMatch(matches) should equal (sentence)
  }

  "findRegexReplace()" should "replace all matches of a sequence of regexes with a paired string" in {
    val matches = Seq(("rain".r, "heavy rain"), ("spain".r, "Spain"))
    sentence.findRegexReplace(matches) should equal ("  The heavy rain in  Spain.  ")
  }

  it should "not perform recursive replacements" in {
    val matches = Seq(("bar".r,"foo"),("foo".r,"bla"))
    "bar foo".findRegexReplace( matches) should equal("foo bla")
  }

  it should "ignore empty strings" in {
    val matches = Seq(("bar".r,"foo"),("foo".r,"bla"))
    "".findRegexReplace(matches) should equal("")
  }

  it should "pass this test" in {
    val matches = Seq(("Int".r,"String"),("\\.".r," "))
    "var a : Int = String.mkString".findRegexReplace(matches) should equal("var a : String = String mkString")
  }

  "findReplace()" should "replace all matches of a sequence of strings with a paired string" in {
    val matches = Seq(("rain", "heavy rain"), ("spain.", "Spain."))
    sentence.findReplace(caseSensitive =  true , zomFindReplace = matches) should equal ("  The heavy rain in  Spain.  ")
  }

  it should "be aware of case" in {
    val matches = Seq(("RAIN", "heavy rain"), ("SPAIN", "Spain"))
    sentence.findReplace(caseSensitive =  false , zomFindReplace = matches) should equal ("  The heavy rain in  Spain.  ")
  }

  it should "not perform recursive replacements" in {
    val matches = Seq(("bar","foo"),("foo","bla"))
    "bar foo".findReplace(matches) should equal("foo bla")
  }

  it should "ignore empty strings" in {
    val matches = Seq(("bar","foo"),("foo","bla"))
    "".findReplace(matches) should equal("")
  }

  it should "pass this test" in {
    val matches = Seq(("Int","String"),("""."""," "))
    "var a : Int = String.mkString".findReplace(matches) should equal("var a : String = String mkString")
  }

  "findReplaceWords()" should "preserve the glue between words" in {
    sentence.findReplaceWords(
      caseSensitive = true,
      zomFindReplace = replacementSequence
    )(Whitespace) should equal("  The Rain in  Espana.  ")
  }

  it should "ignore case when ordered to" in {
    sentence.findReplaceWords(
      caseSensitive = false,
      zomFindReplace =replacementSequence.map{ case (word, b) =>
        (word.toUpperCase, b)
      }
    )(Whitespace) should equal("  The Rain in  Espana.  ")
  }

  it should "not perform recursive replacements" in {
    "bar foo".findReplaceWords(
      caseSensitive = false,
      zomFindReplace =fooMatchSequence
    )(Whitespace) should equal("foo bla")
  }

  it should "ignore empty strings" in {
    "".findReplaceWords(
      caseSensitive = true,
      zomFindReplace =replacementSequence
    )(Whitespace) should equal("")
  }

  "findAllReplace()" should "preserve the glue between words" in {
    sentence.findAllReplace(
      Seq((Seq("rain"), "Rain")),
      caseSensitive = false
    ) should equal("  The Rain in  spain.  ")
  }

  it should "honor case when asked to" in {
    sentence.findAllReplace(
      Seq((Seq("rain", "The"), "Rain")),
      caseSensitive = true
    ) should equal ("  Rain Rain in  spain.  ")
  }

  it should "ignore the empty string" in {
    "".findAllReplace(
      Seq((Seq("rain", "The"), "Rain")),
      caseSensitive = true
    ) should equal ("")
  }

  it should "not perform recursive replacements" in {
    "bar foo".findAllReplace(
      caseSensitive = false,
      zomFindReplace = Seq((Seq("bar"), "foo"), (Seq("foo"), "bla"))
    ) should equal("foo bla")
  }

  "findAllReplaceWords()" should "preserve the glue between words" in {
    sentence.findAllReplaceWords(
      Seq((Seq("RAIN"), "Rain")),
      caseSensitive = false
    )(Whitespace) should equal("  The Rain in  spain.  ")
  }

  it should "honor case when asked to" in {
    sentence.findAllReplaceWords(
      Seq((Seq("rain", "The"), "Rain")),
      caseSensitive = true
    )(Whitespace) should equal ("  Rain Rain in  spain.  ")
  }

  it should "ignore the empty string" in {
    "".findAllReplaceWords(
      Seq((Seq("rain", "The"), "Rain")),
      caseSensitive = true
    )(Whitespace) should equal ("")
  }

  it should "not perform recursive replacements" in {
    "bar foo".findAllReplaceWords(
      caseSensitive = false,
      zomFindReplace = Seq((Seq("bar"), "foo"), (Seq("foo"), "bla"))
    )(Whitespace) should equal("foo bla")
  }

  "toWords()" should "split a string based on a particular splitter" in {
    sentence.toWords(WhitespaceOrUnderscore).toStream should contain allOf("The", "rain", "in", "spain.")
    sentence.toWords(Whitespace).toStream should contain allOf("The", "rain", "in", "spain.")
  }

  "toWordsWithGlue()" should "split a string based on a particular splitter preserving glue" in {
    sentence.toWordsWithGlue(Whitespace).map(
      leadingGlue = { lg:String => lg + '1' },
      word = { w:String => w + '2' },
      glue = { g:String => g + '3' },
      trailingGlue = { tg:String => tg + '4' }
    ) should equal("  1The2 3rain2 3in2  3spain.2  4")
    sentence.toWordsWithGlue(Whitespace).toString should equal("WordSplitResult(LeadingGlue(  ),Word(The),Glue( ),Word(rain),Glue( ),Word(in),Glue(  ),Word(spain.),TrailingGlue(  ))")
  }

  "convert()" should "convert a string based on some convertion function" in {
    "123".convert(java.lang.Integer.parseInt).get should equal (123)
    """Notanint""".convert(java.lang.Integer.parseInt) should equal (None)
  }
}

