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

import scala.annotation.tailrec
import scala.util.matching.Regex
import s_mach.string._
import s_mach.string.CharGroup._

object CharGroupOps {
   def explainCharGroup(cg: CharGroup) : String = {
     cg match {
       case UnicodeLetter => "unicode letters"
       case UppercaseLetter => "uppercase letters"
       case LowercaseLetter => "lowercase letters"
       case Letter => "letters"
       case WordLetter => "word letters"
       case Digit => "digits"
       case Underscore => "underscores"
       case Hyphen => "hyphens"
       case Space => "spaces"
       case Whitespace => "whitespace"
     }
   }

   def explainCharGroups(groups: Seq[CharGroup]) : String = {
    val printGroups = groups.map(explainCharGroup)

    val csmiddle = printGroups.init.mkString(", ")
    val last = if(printGroups.size > 1) s" or ${printGroups.last}" else printGroups.last
    s"must contain only $csmiddle$last"
  }

  def mkCharGroupRegex(groups: CharGroup*) : Regex =
    new Regex(s"[${groups.map(_.pattern).mkString}]")

  // Always try to parse longer patterns first
  val sortedCharGroups =
    CharGroup.all
      .zipWithIndex
      .sortBy { case (g,i) => (-g.pattern.length,i) }
      .map(_._1)
      .toVector

  def unapplyCharGroupRegex(s: String) : Option[Seq[CharGroup]] = {
    if(s.startsWith("[") && s.endsWith("]")) {
      val inner = s.substring(1,s.length - 1)
      @tailrec def loop(current: String, allowed: Vector[CharGroup], acc: List[CharGroup]) : List[CharGroup] = {
        if(current.isEmpty) {
          acc.reverse
        } else {
          if(allowed.isEmpty) {
            Nil
          } else {
            // Important to search for longer patterns first
            allowed.find { g => current.startsWith(g.pattern) } match {
              case Some(found) =>
                loop(
                  current.substring(found.pattern.length),
                  allowed.filterNot(_ == found),
                  found :: acc
                )
              case None =>
                Nil
            }
          }
        }
      }

      loop(inner,sortedCharGroups,Nil) match {
        case Nil => None
        case groups => Some(groups)
      }
    } else {
      None
    }
  }

  def mkCharGroupPattern(groups: CharGroup*) : Regex =
    new Regex(s"^${CharGroupRegex(groups:_*)}*$$")

  val unapplyCharGroupPatternRegex = s"^\\^(.+)\\*\\$$$$".r
  def unapplyCharGroupPattern(s: String) : Option[Seq[CharGroup]] = {
    s match {
      case unapplyCharGroupPatternRegex(inner) =>
        inner match {
          case CharGroupRegex(groups@_*) =>
            Some(groups)
          case _ => None
        }
      case _ => None
    }
  }

}