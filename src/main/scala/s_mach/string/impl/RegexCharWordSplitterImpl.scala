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
       ;it1i ,,,:::;;;::1tti      s_mach.string
         .t1i .,::;;; ;1tt        Copyright (c) 2014 S-Mach, Inc.
         Lft11ii;::;ii1tfL:       Author: lance.gatlin@gmail.com
          .L1 1tt1ttt,,Li
            ...1LLLL...
*/
package s_mach.string.impl

import s_mach.string.WordSplitter

import scala.util.matching.Regex

class RegexCharWordSplitterImpl(
  singleCharWordRegex: Regex,
  singleCharGlueRegex: Regex
) extends WordSplitter {
  val splitRegex = s"${singleCharGlueRegex.regex}+".r

  override def split(s: String): Iterator[String] = {
    // Note: split always returns a non empty iterator
    // for "" it returns Array("")
    // for strings with no glue it returns Array(string)
    // for strings with leading glue, it returns Array("", word, glue, ...)
    splitRegex.split(s) match {
      case Array("",a@_*) => a.iterator
      case a => a.iterator
    }
  }

  val wordBoundaryRegex = s"(?<=${singleCharWordRegex.regex})(?=${singleCharGlueRegex.regex})|(?<=${singleCharGlueRegex.regex})(?=${singleCharWordRegex.regex})".r

  override def splitWithGlue(s: String) = new AbstractWordSplitResult {
    def foreach(
      leadingGlue: String => Unit,
      word: String => Unit,
      glue: String => Unit,
      trailingGlue: String => Unit
    ) : Unit = {
      def process2(result: Iterator[String], w: String) = {
        word(w)
        if(result.hasNext) {
          val g = result.next()
          if(result.hasNext) {
            glue(g)
          } else {
            trailingGlue(g)
          }
        }
      }

      def process(result: Iterator[String]) = {
        while(result.hasNext) {
          val w = result.next()
          process2(result, w)
        }
      }

      if(s.nonEmpty) {
        val result = wordBoundaryRegex.split(s).iterator
        val head = result.next()
        if(splitRegex.pattern.matcher(head).matches) {
          leadingGlue(head)
          process(result)
        } else {
          process2(result, head)
          process(result)
        }
      }
    }
  }
}
