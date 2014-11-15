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
       ;it1i ,,,:::;;;::1tti      s_mach.concurrent
         .t1i .,::;;; ;1tt        Copyright (c) 2014 S-Mach, Inc.
         Lft11ii;::;ii1tfL:       Author: zambrano.hidalgo@gmail.com
          .L1 1tt1ttt,,Li
            ...1LLLL...
*/
package s_mach.string

import scala.collection.mutable.ArrayBuffer
import scala.util.matching.Regex

trait WordSplitter {

  def split(s: String) : Iterator[String]
  //def splitWithGlue(s : String) : Iterator[(String, String)]
  /**
   * This method handles an optional prefix on a split
   * @param optPrefix optional prefix
   * @param str string to split
   * @param regex regex to split by.
   * @return iterator of words that match the regex and the optional prefix prepended
   */
  protected def splitterAccumulate(optPrefix : Option[String], str : String, regex : Regex) : Iterator[String] = {
    //TODO There's probably a less ugly way to do this
    if(str.nonEmpty) {
      val accum = List.newBuilder[String]
      optPrefix.foreach( accum.+= )
      regex.findAllIn(str).foreach(accum.+=)
      accum.result().iterator
    } else {
      Iterator("")
    }

  }
}

class WhitespaceWordSplitter extends WordSplitter {
  import WordSplitter._

  override def split(s: String): Iterator[String] = whiteSpace.split(s).iterator

  //make sure to test leading whitespace and non-leading whitespace
//  override def splitWithGlue(s: String): Iterator[(String, String)] = {
//    whiteSpace.findFirstMatchIn(s) match {
//      case Some(spaces) =>
//        Iterator(("", spaces.toString())) ++
//        magicWhiteSpace.split(s).sliding(2,2).map{ a => (a(0), a(1)) }
//      case None => magicWhiteSpace.split(s).sliding(2,2).map{ a => (a(0), a(1)) }
//    }
//  }
}

class WhitespaceOrUnderscoreWordSplitter extends WordSplitter {
  import WordSplitter.whiteSpaceOrUnderscores
  override def split(s: String): Iterator[String] = whiteSpaceOrUnderscores.split(s).iterator
}

class CamelCaseWordSplitter extends WordSplitter {
  import WordSplitter.{allLowerPrefix, capitalizedWord}
  override def split(s: String): Iterator[String] = {
    splitterAccumulate(allLowerPrefix.findFirstIn(s), s, capitalizedWord)
  }
}

class PascalCaseWordSplitter extends WordSplitter {
  import WordSplitter.capitalizedWord
  override def split(s: String): Iterator[String] = {
    splitterAccumulate(None, s, capitalizedWord)
  }
}

object WordSplitter {

  val magicWhiteSpace = "(?<=\\S)(?=\\s)|(?<=\\s)(?=\\S)".r
  val whiteSpace = """\s+""".r
  val whiteSpaceOrUnderscores = """(\s|_)+""".r
  val allLowerPrefix = """[a-z_]+""".r
  val capitalizedWord = """([A-Z]+[a-z_0-9]*)""".r

  object Whitespace extends WhitespaceWordSplitter
  object WhitespaceOrUnderscore extends WhitespaceOrUnderscoreWordSplitter
  object CamelCase extends CamelCaseWordSplitter
  object PascalCase extends PascalCaseWordSplitter
}