/*
                    ,i::,
               :;;;;;;;
              ;:,,::;.
            1ft1;::;1tL
              t1;::;1,
               :;::;               _____        __  ___              __
          fCLff ;:: tfLLC         / ___/      /  |/  /____ _ _____ / /_
         CLft11 :,, i1tffLi       \__ \ ____ / /|_/ // __ `// ___// __ \
         1t1i   .;;   .1tf       ___/ //___// /  / // /_/ // /__ / / / /
       CLt1i    :,:    .1tfL.   /____/     /_/  /_/ \__,_/ \___//_/ /_/
       Lft1,:;:       , 1tfL:
       ;it1i ,,,:::;;;::1tti      s_mach.concurrent
         .t1i .,::;;; ;1tt        Copyright (c) 2014 S-Mach, Inc.
         Lft11ii;::;ii1tfL:       Author: zambrano.hidalgo@gmail.com
          .L1 1tt1ttt,,Li
            ...1LLLL...
*/
package s_mach.string

import org.scalatest.{Matchers, FlatSpec}
import s_mach.string.WordSplitter._
/**
 * Splitter tests
 *
 * @author Gustavo Hidalgo
 * @version 1.0
 */
class WordSplitter$Test extends FlatSpec with Matchers with TestStrings{

  "White space splitter" should "split a string by whitespace and newlines" in {
    Whitespace.split(sentence).toStream should contain allOf (
      "The",
      "rain",
      "in",
      "spain."
      )
    Whitespace.split(statements).toStream should contain allOf (
      "a[i]",
      "=",
      "a[1];",
      "println(a)"
    )
    Whitespace.split(singleWord).toStream should contain only "hello!"
  }

  it should "return an iterator with an empty string if it can't split" in {
    Whitespace.split("").toStream should contain ("")
  }

  it should "split a string and maintain its glue correctly" in {
    Whitespace.splitWithGlue("aa ").toStream should contain (("aa", " "))
  }

  it should "split a string and maintain its glue correctly when glue occurs first" in {
    Whitespace.splitWithGlue(" aa ").toStream should contain.inOrderOnly(("", " "), ("aa", " "))
  }

  it should "pass this slightly harder test" in {
    Whitespace.splitWithGlue(" aa a ").toStream should contain.inOrderOnly(("", " "), ("aa", " "), ("a", " "))
  }

  it should "degenerate to regular splitting when splitting a string with no glue" in {
    Whitespace.splitWithGlue(singleWord).toStream should contain only(("hello!", ""))
  }

  it should """return an iterator of ("","") on an empty string"""" in {
    Whitespace.splitWithGlue("").toStream should contain only(("", ""))
  }

  "Whitespace or underscore splitter" should "split a string by whitespace, newlines, and underscores" in {
    WhitespaceOrUnderscore.split(sentence).toStream should contain allOf (
      "The",
      "rain",
      "in",
      "spain."
      )
    WhitespaceOrUnderscore.split(statements).toStream should contain allOf (
      "a[i]",
      "=",
      "a[1];",
      "println(a)"
      )
    WhitespaceOrUnderscore.split(singleWord).toStream should contain only "hello!"
    WhitespaceOrUnderscore.split(someUnderscores).toStream should contain allOf (
      "test",
      "variable",
      "with",
      "underscores"
    )
    WhitespaceOrUnderscore.split(moreUnderscores).toSeq should equal ( List(
      "test",
      "variable",
      "with",
      "underscores",
      "another",
      "variable",
      "for",
      "testing"
      )
    )
  }

  it should "return an iterator with an empty string if it can't split" in {
    WhitespaceOrUnderscore.split("").toStream should contain ("")
  }

  it should "split a string and maintain its glue correctly" in {
    WhitespaceOrUnderscore.splitWithGlue("aa ").toStream should contain (("aa", " "))
  }

  it should "split a string and maintain its glue correctly with mixed spaces and underscores" in {
    WhitespaceOrUnderscore.splitWithGlue("_a_aa ").toStream should contain inOrderOnly(("","_"),("a","_"),("aa", " "))
  }

  it should "degenerate to regular splitting when splitting a string with no glue" in {
    WhitespaceOrUnderscore.splitWithGlue(singleWord).toStream should contain (("hello!", ""))
  }

  it should """return an iterator of ("","") on an empty string"""" in {
    WhitespaceOrUnderscore.splitWithGlue("").toStream should contain (("", ""))
  }


  "Camel case word splitter" should "separate a camelCased string into words" in {
    CamelCase.split(simpleCamelCase).toStream should contain allOf (
      "simple",
      "Camel",
      "Case"
    )
    CamelCase.split(harderCamelCase).toStream should contain allOf (
      "simple",
      "Camel",
      "Case",
      "With",
      "WORDSINCAPSNnot",
      "Caught"
    )
  }

  it should "return an iterator with an empty string if it can't split" in {
    CamelCase.split("").toStream should contain ("")
  }

  it should "degenerate to regular splitting when splitting a string with no glue" in {
    CamelCase.splitWithGlue(singleWord).toStream should contain (("hello!", ""))
  }

  it should """return an iterator of ("","") on an empty string"""" in {
    CamelCase.splitWithGlue("").toStream should contain (("", ""))
  }

  "Pascal case word splitter" should "separate PascalCase strings into words" in {
    PascalCase.split(pascalCase).toStream should contain allOf (
      "Pascal",
      "Case"
    )

    PascalCase.split(simpleCamelCase).toStream should contain allOf (
      "Camel",
      "Case"
      )
  }

  it should "degenerate to regular splitting when splitting a string with no glue" in {
    PascalCase.splitWithGlue(singleWord).toStream should contain (("hello!", ""))
  }

  it should """return an iterator of ("","") on an empty string"""" in {
    PascalCase.splitWithGlue("").toStream should contain (("", ""))
  }

}
