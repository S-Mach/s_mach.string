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
         .t1i .,::;;; ;1tt        Copyright (c) 2014 S-Mach, Inc.
         Lft11ii;::;ii1tfL:       Author: zambrano.hidalgo@gmail.com
          .L1 1tt1ttt,,Li
            ...1LLLL...
*/
package s_mach.string.impl

import scala.language.implicitConversions
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.matching.Regex
import scala.util.matching.Regex.Match
import s_mach.string.WordSplitter

object StringOps {

  implicit def match2tuple(m : Match) : (Int, Int) = (m.start, m.end)

  private def overlaps(regions : ArrayBuffer[((Int,Int), String)], region : (Int, Int)) : Boolean = {
    regions.exists { case ((begin, end),_) =>
      region._1 > begin && region._1 < end ||
      region._2 > begin && region._2 < end
    }
  }

  def ensureSuffix(s: String, suffix: String) : String = {
    if(s.endsWith(suffix)) s else s + suffix
  }

  def findRegexReplaceMatch(s: String, zomRegex: Seq[(Regex, Match => String)]) : String = {
    if(s.nonEmpty) {
      val matchedRegions = collection.mutable.HashSet[(Int, Int)]()
      val willReplace = ArrayBuffer[((Int, Int), String)]()
      for ((regex, matcher) <- zomRegex) {
        regex.findAllMatchIn(s).foreach { match_ =>
          if (!overlaps(willReplace, match_)) {
            matchedRegions += match_
            willReplace.append((match_, matcher(match_)))
          }
        }
      }
      willReplace.append(((s.length, 0), "")) //why did we need this?
      if (willReplace.length > 1) {
        val sb = new mutable.StringBuilder()
        sb.append(s.substring(0, willReplace.head._1._1))
        willReplace.sliding(2, 1).foreach { a =>
          val first = a(0)
          val next = a(1)
          sb.append(first._2)
          sb.append(s.slice(first._1._2, next._1._1))
        }
        sb.result()
      } else s
    } else ""
  }

  def findRegexReplace(
    s: String,
    zomRegex: Seq[(Regex, String)]
  ) : String = {
    findRegexReplaceMatch(s, zomRegex.map{
      case (regex, replacement) => (regex, {_ : Match => replacement})
    })
  }

  def findReplace(
    s: String,
    caseSensitive: Boolean,
    fr: Seq[(String, String)]
  ) : String = {
    findRegexReplace(s, fr.map { case (find, replacement) =>
      val regexFlags = if(!caseSensitive) "(?i)" else ""
      val regex = (regexFlags + Regex.quote(find)).r
      (regex, replacement)
    })
    }

  def findReplaceWords(
    s: String,
    caseSensitive: Boolean,
    fr: Seq[(String, String)]
  )(implicit splitter:WordSplitter) : String = {
    val caseFunction : (String, String) => Boolean =
      if(caseSensitive) {
        { (a,b) => a == b }
      } else {
        { (a,b) => a.equalsIgnoreCase(b) }
      }
    splitter.splitWithGlue(s).map(
      leadingGlue = { lg:String => lg },
      word = { w:String =>
        fr.find(t => caseFunction(t._1, w)) match {
          case Some((_, replacement)) => replacement
          case None => w
        }
      },
      glue = { g:String => g },
      trailingGlue = { tg:String => tg }
    )
  }

  def findAllReplace(
    s: String,
    caseSensitive: Boolean,
    fr: Seq[(Seq[String], String)]
  ) : String = {
    findRegexReplace(s, fr.map { case (find, replacement) =>
      val regexFlags = if(caseSensitive) "(?i)" else ""
      val regex = s"$regexFlags(${find.map(Regex.quote).mkString("|")})".r
      (regex, replacement)
    })
  }

  def findAllReplaceWords(
    s: String,
    caseSensitive: Boolean,
    fr: Seq[(Seq[String], String)]
  )(implicit splitter:WordSplitter) : String = {
    val caseFunction : (String, String) => Boolean =
      if(caseSensitive) {
        { (a,b) => a == b }
      } else {
        { (a,b) => a.equalsIgnoreCase(b) }
      }
    splitter.splitWithGlue(s).map(
      leadingGlue = { lg:String => lg },
      word = { w:String =>
        fr.find(t => t._1.exists(caseFunction(_, w))) match {
          case Some((_, replacement)) => replacement
          case None => w
        }
      },
      glue = { g:String => g },
      trailingGlue = { tg:String => tg }
    )
  }

  def collapseGlue(
    s: String,
    glueSubst: String
  )(implicit splitter:WordSplitter) : String = {
    splitter.splitWithGlue(s).map(
      leadingGlue = { lg:String => "" },
      word = { w:String => w },
      glue = { g:String => glueSubst },
      trailingGlue = { tg:String => "" }
    )
  }

  def collapseWhitespace(s: String) : String =
    collapseGlue(s," ")(WordSplitter.Whitespace)

  def toProperCase(s: String) : String = {
    s.length match {
      case long if long > 1 => s.head.toUpper + s.tail.toLowerCase
      case oneChar if oneChar == 1 => s.head.toUpper.toString
      case _ => ""
    }
  }

  def mapWords(s: String)(f: String => String)(implicit splitter:WordSplitter) : String = {
    splitter.splitWithGlue(s).map(
      leadingGlue = { lg:String => lg },
      word = { w:String => f(w) },
      glue = { g:String => g },
      trailingGlue = { tg:String => tg }
    )
  }

  def toTitleCase(s: String)(implicit splitter:WordSplitter) : String = {
    mapWords(s)(toProperCase)
  }


  def toCamelCase(s: String)(implicit splitter:WordSplitter) : String = {
    val sb = new StringBuilder(s.length)
    val words = splitter.split(s)
    if(words.nonEmpty) {
      sb.append(words.next().toLowerCase)
      words.foreach(word => sb.append(toProperCase(word)))
    }
    sb.result()
  }

  def toPascalCase(s: String)(implicit splitter:WordSplitter) : String = {
    splitter.split(s).map(toProperCase).mkString
  }

  def toSnakeCase(s: String)(implicit  splitter:WordSplitter) : String = {
    splitter.split(s).map(_.toLowerCase).mkString("_")
  }

  def indent(s: String, n: Int, spacer: String = " ") : String = {
    val builder : StringBuilder = StringBuilder.newBuilder
    val indent = spacer * n
    s.linesWithSeparators.foreach {
      line => builder.append(indent + line)
    }
    builder.toString()
  }

  def toOption(s: String) : Option[String] = {
    if(s.length == 0) None else Some(s)
  }

  def toDoubleOpt(s: String) : Option[Double] = convert[Double](s, java.lang.Double.parseDouble)

  def toLongOpt(s: String) : Option[Long] = convert[Long](s, java.lang.Long.parseLong)

  def toIntOpt(s: String) : Option[Int] = convert[Int](s, java.lang.Integer.parseInt)

  def convert[A](s: String, f: String => A) : Option[A] = {
    try {
      Some(f(s))
    }
    catch {
      case e : Exception  => None
    }
  }

}
