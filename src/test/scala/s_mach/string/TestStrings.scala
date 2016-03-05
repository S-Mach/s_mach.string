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
       ;it1i ,,,:::;;;::1tti      s_mach.concurrent
         .t1i .,::;;; ;1tt        Copyright (c) 2016 S-Mach, Inc.
         Lft11ii;::;ii1tfL:       Author: zambrano.hidalgo@gmail.com
          .L1 1tt1ttt,,Li
            ...1LLLL...
*/
package s_mach.string

import scala.util.matching.Regex.Match

/**
 * Set of test strings used in the test classes. do not change.
 */
trait TestStrings {
  val sentence = "  The rain in  spain.  "
  val singleWord = "hello!"
  val statements = "a[i] = a[1];\nprintln(a)"
  val doubleString = "1.0"
  val intString = "1"
  val hello = "Hello!"
  val long = "100000000000"
  val complicated = "Hello! 1.0"
  val complicated2 = "1.0 Hello! 1.0"
  val complicated3 = "1 Hello! 1.0"
  val emptyString = ""
  val someUnderscores = "test_variable_with_underscores"
  val moreUnderscores = "_test_variable_with_underscores\nanother variable_for testing  "
  val simpleCamelCase = "simpleCamelCase"
  val harderCamelCase = "simpleCamelCaseWithWORDSINCAPSNnotCaught"
  val pascalCase = "PascalCase"

  val spainMatchFunction = (m : Match) => {
    m match {
      case Match("rain") => "heavy rain"
      case Match("spain") => "Spain"
      case _ => ""
    }
  }

  val fooMatchFunction = (m : Match) => {
    m match {
      case Match("bar") => "foo"
      case Match("foo") => "bla"
      case _ => ""
    }
  }

  val fooMatchSequence = Seq(("bar", "foo"),("foo", "bla"))

  val replacementSequence = Seq(("rain", "Rain"), ("spain.", "Espana."))

  val scalaMatchFunction = (m : Match) => {
    m match {
      case Match("Int") => "String"
      case Match(".") => " "
      case _ => ""
    }
  }
}
