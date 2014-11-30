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
         Lft11ii;::;ii1tfL:       Author: zambrano.hidalgo@gmail.com
          .L1 1tt1ttt,,Li
            ...1LLLL...
*/
package s_mach.string

import s_mach.string.impl.{RegexCharTransitionWordSplitterImpl, RegexCharWordSplitterImpl}
import scala.util.matching.Regex

/**
 * A trait for a word splitter that allows splitting a string into "words" and
 * "glue". Each word splitter defines words and glue differently. A whitespace
 * splitter assigns whitespace runs as "glue" and non-whitespace runs as
 * "words". A capitalization-based splitter creates words when capitalization
 * changes from
 *
 */
trait WordSplitter {

  /**
   * Splits a string into words based on some boundary condition ignoring
   * any "glue" characters between any two words.
   * @param s string to split into words
   * @return Iterator of the words created from the split
   */
  def split(s: String) : Iterator[String]

  /**
   * Splits a string into words based on some boundary condition and preserves
   * the "glue" characters that were between the boundaries, including the
   * leading and trailing glue.
   * @param s string to split
   * @return a lazy word split result
   */
  def splitWithGlue(s : String) : WordSplitResult
}

object RegexCharWordSplitter {
  /**
   * Create a word splitter by defining the glue
   * @param glueChars a sequence of glue characters
   * @return a word splitter that splits word and glue as defined by the
   *         supplied glue chars
   */
  def apply(glueChars: Seq[Char]) : WordSplitter = {
    val glue = glueChars.mkString
    apply(
      singleCharWordRegex = s"[^$glue]".r,
      singleCharGlueRegex = s"[$glue]".r
    )
  }

  /**
   * Create a word splitter by defining the regex that matches words and the
   * regex that matches glue. Note: singleCharWordRegex and singleCharGlueRegex
   * must be mutually exclusive. If they match the same characters the results
   * of the word splitter are undefined.
   * @param singleCharWordRegex a regex that matches a single char of a word
   * @param singleCharGlueRegex a regex that matches a single char of glue
   * @return a word splitter based on the supplied word and glue regexes
   */
  def apply(
    singleCharWordRegex: Regex,
    singleCharGlueRegex: Regex
  ) : WordSplitter = new RegexCharWordSplitterImpl(
    singleCharWordRegex = singleCharWordRegex,
    singleCharGlueRegex = singleCharGlueRegex
  )
}

object RegexCharTransitionWordSplitter {
  /**
   * Create a word splitter that splits words based on transitions
   * @param oomBeforeAfter one or more (before regex,after regex) tuples
   * @return a word splitter that splits words based on the word transitions
   *         defined by oomBeforeAfter
   */
  def apply(
    oomBeforeAfter: Seq[(Regex,Regex)]
  ) : WordSplitter = new RegexCharTransitionWordSplitterImpl(oomBeforeAfter)
}

object WordSplitter {
  val whitespaceChars = " \t\r\n\f".toCharArray
  implicit val Whitespace = RegexCharWordSplitter(whitespaceChars)
  implicit val WhitespaceOrUnderscore = RegexCharWordSplitter('_' +: whitespaceChars)
  implicit val Underscore = RegexCharWordSplitter(Seq('_'))

  implicit val CamelCase = RegexCharTransitionWordSplitter(Seq(("[a-z]".r,"[A-Z]".r)))
  implicit val PascalCase = CamelCase
  implicit val SnakeCase = Underscore
}