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
         Lft11ii;::;ii1tfL:       Author: zambrano.hidalgo@gmail.com
          .L1 1tt1ttt,,Li
            ...1LLLL...
*/
package s_mach.string

import s_mach.string.impl.RegexCharLexerImpl

import scala.util.matching.Regex

object RegexCharLexer {
  /**
   * Create a lexer by defining one or more delimiters
 *
   * @param delimiterChars a sequence of delimiter characters
   * @return a lexer that separates token and delimiter as defined by the
   *         supplied delimiter chars
   */
  def apply(delimiterChars: Seq[Char]) : Lexer = {
    val delimiter = delimiterChars.mkString
    apply(
      singleCharTokenRegex = s"[^$delimiter]".r,
      singleCharDelimRegex = s"[$delimiter]".r
    )
  }

  /**
   * Create a lexer by defining the regex that matches tokens and the
   * regex that matches delimiter. Note: singleCharTokenRegex and singleCharDelimRegex
   * must be mutually exclusive. If they match the same characters the results
   * of the lexer are undefined.
   *
   * @param singleCharTokenRegex a regex that matches a single char of a token
   * @param singleCharDelimRegex a regex that matches a single char of delimiter
   * @return a lexer based on the supplied token and delimiter regexes
   */
  def apply(
    singleCharTokenRegex: Regex,
    singleCharDelimRegex: Regex
  ) : Lexer = new RegexCharLexerImpl(
    singleCharTokenRegex = singleCharTokenRegex,
    singleCharDelimRegex = singleCharDelimRegex
  )
}