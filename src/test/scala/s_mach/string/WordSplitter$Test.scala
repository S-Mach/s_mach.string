package s_mach.string

import org.scalatest.{Matchers, FlatSpec}
import WordSplitter.{Whitespace, WhitespaceOrUnderscore}
/**
 * Splitter tests
 *
 * @author Gustavo Hidalgo
 * @version 1.0
 */
class WordSplitter$Test extends FlatSpec with Matchers with TestStrings{

  "A white space splitter" should "split a string by whitespace and newlines" in {
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

  "A whitespace or underscore splitter" should "split a string by whitespace, newlines, and underscores" in {
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

}
