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
package s_mach.string.impl

import s_mach.string.WordSplitter
import scala.util.matching.Regex

object SplitterOps {

  val magicWhiteSpace = "(?<=\\S)(?=\\s)|(?<=\\s)(?=\\S)".r
  val magicWhitespaceOrUnderscores= "(?<=[^_])(?=(_|\\s))|(?<=(\\s|_))(?=[^_\\s])".r
  val magicCamelOrPascal = "(?<=[^A-Z])(?=[A-Z])".r
  val whiteSpace = """\s+""".r
  val whiteSpaceOrUnderscores = """(\s|_)+""".r
  val allLowerPrefix = """[a-z_]+""".r
  val capitalizedWord = """([A-Z]+[a-z_0-9]*)""".r

  /**
   * This method handles an optional prefix on a split
   * @param optPrefix optional prefix
   * @param str string to split
   * @param regex regex to split by.
   * @return iterator of words that match the regex and the optional prefix prepended
   */
  def splitterAccumulate(optPrefix : Option[String], str : String, regex : Regex) : Iterator[String] = {
    if(str.nonEmpty) {
      val accum = List.newBuilder[String]
      optPrefix.foreach( accum.+= )
      regex.findAllIn(str).foreach(accum.+=)
      accum.result().iterator
    } else {
      Iterator("")
    }
  }

  /**
   * Helper method for the glue-aware split methods
   * @param glueDetector Magic regex that splits strings at boundaries of a particular patter
   * @param glue the regular splitting regex that these objects use
   * @param s string to split
   * @return An Iterator[(String, String)] with (word, glue) elements
   */
  def glueSplit(glueDetector : Regex, glue : Regex, s : String) : Iterator[(String, String)] = {
    glue.findFirstMatchIn(s) match {
      case Some(beginGlue) =>
        if (beginGlue.start == 0) {
          Iterator(("", beginGlue.toString())) ++
            glueDetector.split(s).tail.sliding(2, 2).map { a => if (a.length == 2) (a(0), a(1)) else (a(0), "")} //voodoo
        } else {
          glueDetector.split(s).sliding(2, 2).map { a => if (a.length == 2) (a(0), a(1)) else (a(0), "")}
        }
      case None => glue.split(s).map(a => (a, "")).iterator
    }
  }

  class WhitespaceWordSplitter extends WordSplitter {

    override def split(s: String): Iterator[String] = whiteSpace.split(s).iterator

    override def splitWithGlue(s: String): Iterator[(String, String)] = {
      glueSplit(magicWhiteSpace, whiteSpace, s)
    }
  }

  class WhitespaceOrUnderscoreWordSplitter extends WordSplitter {

    override def splitWithGlue(s: String): Iterator[(String, String)] = {
      glueSplit(magicWhitespaceOrUnderscores, whiteSpaceOrUnderscores, s)
    }

    override def split(s: String): Iterator[String] = whiteSpaceOrUnderscores.split(s).iterator
  }

  class CamelCaseWordSplitter extends WordSplitter {

    override def splitWithGlue(s: String): Iterator[(String, String)] = {
      glueSplit(magicCamelOrPascal, capitalizedWord, s)
    }

    override def split(s: String): Iterator[String] = {
      splitterAccumulate(allLowerPrefix.findFirstIn(s), s, capitalizedWord)
    }
  }

  class PascalCaseWordSplitter extends WordSplitter {

    override def splitWithGlue(s: String): Iterator[(String, String)] = {
      glueSplit(magicCamelOrPascal, capitalizedWord, s)
    }

    override def split(s: String): Iterator[String] = {
      splitterAccumulate(None, s, capitalizedWord)
    }
  }
}
