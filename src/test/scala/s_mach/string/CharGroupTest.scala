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
       ;it1i ,,,:::;;;::1tti      s_mach.string
         .t1i .,::;;; ;1tt        Copyright (c) 2016 S-Mach, Inc.
         Lft11ii;::;ii1tfL:       Author: lance.gatlin@gmail.com
          .L1 1tt1ttt,,Li
            ...1LLLL...
*/
package s_mach.string

import org.scalatest.{FlatSpec, Matchers}
import CharGroup._

class CharGroupTest extends FlatSpec with Matchers {
  "CharGroupRegex.apply(CharGroups*)" should "return a regex built from the supplied char groups" in {
    val groups = CharGroup.all
    CharGroupRegex(
      groups:_*
    ).regex should be (
      s"[${groups.map(_.pattern).mkString}]"
    )
  }
  "CharGroupRegex.unapply(String)" should "parse a sequence of char groups from a correctly formed regex" in {
    val uclc = List(CharGroup.UppercaseLetter,CharGroup.LowercaseLetter)
    val combinations = CharGroup.all.combinations(3)
    combinations
      .filterNot(_.sliding(2).exists(_ == uclc))
      .foreach { gs =>
        CharGroupRegex.unapplySeq(
          CharGroupRegex(gs:_*).pattern.pattern()
        ) should be(Some(
          gs
        ))
      }
  }

  "CharGroupPattern.apply(CharGroups*)" should "return a full string regex built from the supplied char groups" in {
    val groups = CharGroup.all
    CharGroupPattern(
      groups:_*
    ).regex should be (
      s"^[${groups.map(_.pattern).mkString}]*$$"
    )
  }
  "CharGroupPattern.unapply(String)" should "parse a sequence of char groups from a correctly formed regex" in {
    val uclc = List(CharGroup.UppercaseLetter,CharGroup.LowercaseLetter)
    val combinations = CharGroup.all.combinations(3)
    combinations
      .filterNot(_.sliding(2).exists(_ == uclc))
      .foreach { gs =>
      CharGroupPattern.unapplySeq(
        CharGroupPattern(gs:_*).pattern.pattern()
      ) should be(Some(
        gs
      ))
    }
  }

}
