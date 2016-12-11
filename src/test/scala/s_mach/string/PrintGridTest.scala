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


class PrintGridTest extends FlatSpec with Matchers {
  "Grid.printGrid" should "print a formatted grid" in {
    IndexedSeq(
      IndexedSeq("a a","bb","c","d"),
      IndexedSeq("d","eee","f","g"),
      IndexedSeq("hh","i","jjjj","k")
    ).printGrid shouldBe
"""
a a bb  c    d
d   eee f    g
hh  i   jjjj k
""".trim + "\n"
  }
}
