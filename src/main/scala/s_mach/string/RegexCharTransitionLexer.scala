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

import s_mach.string.impl.RegexCharTransitionLexerImpl

import scala.util.matching.Regex

object RegexCharTransitionLexer {
  /**
   * Create a lexer that separates tokens based on character transitions
   * Note: this lexer produces no delimiters
   *
   * @param oomBeforeAfter one or more (before regex,after regex) tuples
   * @return a lexer that separates tokens based on the token transitions
   *         defined by oomBeforeAfter
   */
  def apply(
    oomBeforeAfter: Seq[(Regex, Regex)]
  ): Lexer = new RegexCharTransitionLexerImpl(oomBeforeAfter)
}

