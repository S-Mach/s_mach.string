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
         Lft11ii;::;ii1tfL:       Author: lance.gatlin@gmail.com
          .L1 1tt1ttt,,Li
            ...1LLLL...
*/
package s_mach.string.impl

import s_mach.string.WordSplitResult

trait AbstractWordSplitResult extends WordSplitResult {
  override def map(
    leadingGlue: String => String,
    word: String => String,
    glue: String => String,
    trailingGlue: String => String
  ) : String = {
    val sb = new StringBuilder
    // Note: required for casting to String => Unit since sb.append returns StringBuilder
    val f1 : String => Unit = { s:String => sb.append(leadingGlue(s)) }
    val f2 : String => Unit = { s:String => sb.append(word(s)) }
    val f3 : String => Unit = { s:String => sb.append(glue(s)) }
    val f4 : String => Unit = { s:String => sb.append(trailingGlue(s)) }
    foreach(
      leadingGlue = f1,
      word = f2,
      glue = f3,
      trailingGlue = f4
    )
    sb.result()
  }

  override def toString = {
    val sb = new StringBuilder
    sb.append("WordSplitResult(")
    foreach(
      leadingGlue = { s:String => sb.append(s"LeadingGlue($s),") },
      word = { s:String => sb.append(s"Word($s),") },
      glue = { s:String => sb.append(s"Glue($s),") },
      trailingGlue = { s:String => sb.append(s"TrailingGlue($s),") }
    )
    sb.dropRight(1).append(")").result()
  }

}
