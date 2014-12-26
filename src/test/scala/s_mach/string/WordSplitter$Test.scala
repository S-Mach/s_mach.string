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
         .t1i .,::;;; ;1tt        Copyright (c) 2014 S-Mach, Inc.
         Lft11ii;::;ii1tfL:       Author: zambrano.hidalgo@gmail.com
          .L1 1tt1ttt,,Li
            ...1LLLL...
*/
package s_mach.string

import org.scalatest.{Matchers, FlatSpec}
import s_mach.string.WordSplitter._

class WordSplitter$Test extends FlatSpec with Matchers with TestStrings{

  "RegexSplitter.split" should "split a string into words" in {
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

  it should "return the string if there are no words" in {
    Underscore.split(sentence).toSeq should contain(sentence)
  }

  it should "return Iterator.empty if the string is empty" in {
    Underscore.split("").toSeq should equal(Seq.empty)
  }

  "RegexSplitter.splitWithGlue" should "split a string words with glue" in {
    Whitespace.splitWithGlue(sentence).map(
      leadingGlue = { lg:String => lg + '1' },
      word = { w:String => w + '2' },
      glue = { g:String => g + '3' },
      trailingGlue = { tg:String => tg + '4' }
    ) should equal("  1The2 3rain2 3in2  3spain.2  4")
  }

  it should "return the entire string as a word if there is no glue" in {
    Underscore.splitWithGlue(sentence).map(
      leadingGlue = { lg:String => lg + '1' },
      word = { w:String => w + '2' },
      glue = { g:String => g + '3' },
      trailingGlue = { tg:String => tg + '4' }
    ) should equal("  The rain in  spain.  2")
  }

  it should "return nothing if the string is empty" in {
    Underscore.splitWithGlue("").map(
      leadingGlue = { lg:String => lg + '1' },
      word = { w:String => w + '2' },
      glue = { g:String => g + '3' },
      trailingGlue = { tg:String => tg + '4' }
    ) should equal("")
  }


  "CaseWordSplitter.split" should "separate a camelCased string into words" in {
    CamelCase.split(simpleCamelCase).toStream should contain allOf(
      "simple",
      "Camel",
      "Case"
    )
  }

  it should "return the entire string as a word if there is no glue" in {
    CamelCase.split(simpleCamelCase.toLowerCase).toStream should equal(Stream(
      "simplecamelcase"
    ))
  }

  it should "return Iterator.empty if the string is empty" in {
    CamelCase.split("").toStream should equal(Stream.empty)
  }

  "CaseWordSplitter.splitWithGlue" should "separate a camelCased string into words" in {
    CamelCase.splitWithGlue(simpleCamelCase).map(
      leadingGlue = { lg:String => lg + '1' },
      word = { w:String => w + '2' },
      glue = { g:String => g + '3' },
      trailingGlue = { tg:String => tg + '4' }
    ) should equal("simple2Camel2Case2")
  }

  it should "return the entire string as a word if there is no glue" in {
    CamelCase.splitWithGlue(simpleCamelCase.toLowerCase).map(
      leadingGlue = { lg:String => lg + '1' },
      word = { w:String => w + '2' },
      glue = { g:String => g + '3' },
      trailingGlue = { tg:String => tg + '4' }
    ) should equal("simplecamelcase2")
  }

  it should "return Iterator.empty if the string is empty" in {
    CamelCase.splitWithGlue("").map(
      leadingGlue = { lg:String => lg + '1' },
      word = { w:String => w + '2' },
      glue = { g:String => g + '3' },
      trailingGlue = { tg:String => tg + '4' }
    ) should equal("")
  }
}
