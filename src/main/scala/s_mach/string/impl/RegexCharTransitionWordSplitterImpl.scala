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
package s_mach.string.impl

import s_mach.string.WordSplitter

import scala.util.matching.Regex

class RegexCharTransitionWordSplitterImpl(oomBeforeAfter: Seq[(Regex,Regex)]) extends WordSplitter {

  val regex =
    oomBeforeAfter.map { case (before, after) =>
      s"(?<=${before.regex})(?=${after.regex})"
    }.mkString("|").r

  override def split(s: String): Iterator[String] = {
    regex.split(s) match {
      case Array("") => Iterator.empty
      case a => a.iterator
    }
  }

  override def splitWithGlue(s: String) = new AbstractWordSplitResult {
    override def foreach(
      leadingGlue: String => Unit, 
      word: String => Unit, 
      glue: String => Unit, 
      trailingGlue: String => Unit
    ): Unit = {
      split(s).foreach(word)
    }
  }
}