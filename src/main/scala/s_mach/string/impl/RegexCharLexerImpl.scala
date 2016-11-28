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

class RegexCharLexerImpl(
  singleCharTokenRegex: Regex,
  singleCharDelimRegex: Regex
) extends Lexer {
  val separateRegex = s"${singleCharDelimRegex.regex}+".r

  override def tokens(s: String): Iterator[String] = {
    // Note: separate always returns a non empty iterator
    // for "" it returns Array("")
    // for strings with no delimiter it returns Array(string)
    // for strings with leading delimiter, it returns Array("", token, delimiter, ...)
    separateRegex.split(s) match {
      case Array("",a@_*) => a.iterator
      case a => a.iterator
    }
  }

  val tokenBoundaryRegex = s"(?<=${singleCharTokenRegex.regex})(?=${singleCharDelimRegex.regex})|(?<=${singleCharDelimRegex.regex})(?=${singleCharTokenRegex.regex})".r

  override def lex(s: String) = new AbstractLexResult {
    def foreach(
      leadingDelim: String => Unit,
      token: String => Unit,
      delimiter: String => Unit,
      trailingDelim: String => Unit
    ) : Unit = {
      def process2(result: Iterator[String], w: String) = {
        token(w)
        if(result.hasNext) {
          val g = result.next()
          if(result.hasNext) {
            delimiter(g)
          } else {
            trailingDelim(g)
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
        val result = tokenBoundaryRegex.split(s).iterator
        val head = result.next()
        if(separateRegex.pattern.matcher(head).matches) {
          leadingDelim(head)
          process(result)
        } else {
          process2(result, head)
          process(result)
        }
      }
    }
  }
}
