package s_mach.string

import org.scalatest.{Matchers, FlatSpec}
import s_mach.string.WordSplitter.{PascalCase, Whitespace, WhitespaceOrUnderscore, CamelCase}
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

}
