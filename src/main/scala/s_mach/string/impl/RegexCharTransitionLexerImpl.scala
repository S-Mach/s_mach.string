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

import s_mach.string.Lexer
import scala.util.matching.Regex

class RegexCharTransitionLexerImpl(oomBeforeAfter: Seq[(Regex,Regex)]) extends Lexer {

  val regex =
    oomBeforeAfter.map { case (before, after) =>
      s"(?<=${before.regex})(?=${after.regex})"
    }.mkString("|").r

  override def tokens(s: String): Iterator[String] = {
    regex.split(s) match {
      case Array("") => Iterator.empty
      case a => a.iterator
    }
  }

  override def lex(s: String) = new AbstractLexResult {
    override def foreach(
      leadingDelim: String => Unit,
      token: String => Unit,
      delimiter: String => Unit,
      trailingDelim: String => Unit
    ): Unit = {
      tokens(s).foreach(token)
    }
  }
}