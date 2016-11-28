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
import s_mach.string.Lexer._

class LexerTest extends FlatSpec with Matchers with TestStrings{

  "RegexSplitter.separate" should "separate a string into tokens" in {
    Whitespace.tokens(sentence).toStream should contain allOf (
      "The",
      "rain",
      "in",
      "spain."
      )
    Whitespace.tokens(statements).toStream should contain allOf (
      "a[i]",
      "=",
      "a[1];",
      "println(a)"
    )
    Whitespace.tokens(singleWord).toStream should contain only "hello!"
  }

  it should "return the string if there are no tokens" in {
    Underscore.tokens(sentence).toSeq should contain(sentence)
  }

  it should "return Iterator.empty if the string is empty" in {
    Underscore.tokens("").toSeq should equal(Seq.empty)
  }

  "RegexSplitter.lex" should "separate a string tokens with delimiter" in {
    Whitespace.lex(sentence).map(
      leadingDelim = { lg:String => lg + '1' },
      token = { w:String => w + '2' },
      delim = { g:String => g + '3' },
      trailingDelim = { tg:String => tg + '4' }
    ) should equal("  1The2 3rain2 3in2  3spain.2  4")
  }

  it should "return the entire string as a token if there is no delimiter" in {
    Underscore.lex(sentence).map(
      leadingDelim = { lg:String => lg + '1' },
      token = { w:String => w + '2' },
      delim = { g:String => g + '3' },
      trailingDelim = { tg:String => tg + '4' }
    ) should equal("  The rain in  spain.  2")
  }

  it should "return nothing if the string is empty" in {
    Underscore.lex("").map(
      leadingDelim = { lg:String => lg + '1' },
      token = { w:String => w + '2' },
      delim = { g:String => g + '3' },
      trailingDelim = { tg:String => tg + '4' }
    ) should equal("")
  }


  "CaseLexer.tokens" should "separate a camelCased string into tokens" in {
    CamelCase.tokens(simpleCamelCase).toStream should contain allOf(
      "simple",
      "Camel",
      "Case"
    )
  }

  it should "return the entire string as a token if there is no delimiter" in {
    CamelCase.tokens(simpleCamelCase.toLowerCase).toStream should equal(Stream(
      "simplecamelcase"
    ))
  }

  it should "return Iterator.empty if the string is empty" in {
    CamelCase.tokens("").toStream should equal(Stream.empty)
  }

  "CaseLexer.lex" should "separate a camelCased string into tokens" in {
    CamelCase.lex(simpleCamelCase).map(
      leadingDelim = { lg:String => lg + '1' },
      token = { w:String => w + '2' },
      delim = { g:String => g + '3' },
      trailingDelim = { tg:String => tg + '4' }
    ) should equal("simple2Camel2Case2")
  }

  it should "return the entire string as a token if there is no delimiter" in {
    CamelCase.lex(simpleCamelCase.toLowerCase).map(
      leadingDelim = { lg:String => lg + '1' },
      token = { w:String => w + '2' },
      delim = { g:String => g + '3' },
      trailingDelim = { tg:String => tg + '4' }
    ) should equal("simplecamelcase2")
  }

  it should "return Iterator.empty if the string is empty" in {
    CamelCase.lex("").map(
      leadingDelim = { lg:String => lg + '1' },
      token = { w:String => w + '2' },
      delim = { g:String => g + '3' },
      trailingDelim = { tg:String => tg + '4' }
    ) should equal("")
  }
}
