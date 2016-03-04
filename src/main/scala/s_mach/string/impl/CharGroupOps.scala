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
import s_mach.string._

object CharGroupOps {
   def explainCharGroups(groups: Seq[CharGroup]) : String = {
    import CharGroup._
    val printGroups =
      groups
        .map {
          case UnicodeLetter => (0,"unicode letters")
          case UppercaseLetter => (1, "uppercase letters")
          case LowercaseLetter => (2, "lowercase letters")
          case Letter => (3,"letters")
          case WordLetter => (4,"word letters")
          case Digit => (5,"digits")
          case Underscore => (6,"underscores")
          case Hyphen => (7,"hyphens")
          case Space => (8,"spaces")
          case Whitespace => (9,"whitespace")
        }
        .sortBy(_._1)
        .map(_._2)

    val csmiddle = printGroups.init.mkString(", ")
    val last = if(printGroups.size > 1) s" or ${printGroups.last}" else printGroups.last
    s"must contain only $csmiddle$last"
  }

  def mkCharGroupRegex(groups: CharGroup*) : String =
    s"[${groups.sortBy(_.order).map(_.pattern).mkString}]"

  val charGroupsList = CharGroup.all.toList
  def unapplyCharGroupRegex(s: String) : Option[Seq[CharGroup]] = {
    if(s.startsWith("[") && s.endsWith("]")) {
      val inner = s.substring(1,s.length - 1)
      @tailrec def loop(current: String, expected: List[CharGroup], acc: List[CharGroup]) : List[CharGroup] = {
        expected match {
          case Nil => if(current.isEmpty) {
            acc
          } else {
            Nil
          }
          case head :: tail =>
            if(current.startsWith(head.pattern)) {
              loop(current.substring(head.pattern.length),tail,head :: acc)
            } else {
              loop(current,tail,acc)
            }
        }
      }

      loop(inner,charGroupsList,Nil) match {
        case Nil => None
        case groups => Some(groups)
      }
    } else {
      None
    }
  }

  def mkCharGroupPattern(groups: CharGroup*) : String =
    s"^${CharGroupRegex(groups:_*)}*$$"

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