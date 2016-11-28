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


/**
 * A trait for a lexer that allows separating a string into "tokens" and
 * "delimiters". Each lexer defines tokens and delimiters differently. A whitespace
 * lexer assigns whitespace runs as delimiters and non-whitespace runs as
 * "tokens". A capitalization-based lexer creates tokens when capitalization
 * changes from lower case to upper case and produces no delimiters.
 */
trait Lexer {

  /**
   * Separate a string into tokens based on some boundary condition ignoring
   * any delimiter characters between any two tokens.
 *
   * @param s string to separate into tokens
   * @return Iterator of tokens
   */
  def tokens(s: String) : Iterator[String]

  /**
   * Lazily separate a string into tokens based on some boundary condition and
   * preserve the delimiter characters that were between the boundaries, including
   * any leading or trailing delimiters.
   *
   * @param s string to separate
   * @return a lazy result
   */
  def lex(s: String) : LexResult
}

object Lexer {
  val whitespaceChars = " \t\r\n\f".toCharArray
  implicit val Whitespace = RegexCharLexer(whitespaceChars)
  implicit val WhitespaceOrUnderscore = RegexCharLexer('_' +: whitespaceChars)
  implicit val Underscore = RegexCharLexer(Seq('_'))

  implicit val CamelCase = RegexCharTransitionLexer(Seq(("[a-z]".r,"[A-Z]".r)))
  implicit val PascalCase = CamelCase
  implicit val SnakeCase = Underscore
}

