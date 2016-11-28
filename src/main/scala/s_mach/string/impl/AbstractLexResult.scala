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

import s_mach.string.LexResult

trait AbstractLexResult extends LexResult {
  override def map(
    leadingDelim: String => String,
    token: String => String,
    delimiter: String => String,
    trailingDelim: String => String
  ) : String = {
    val sb = new StringBuilder
    // Note: required for casting to String => Unit since sb.append returns StringBuilder
    val f1 : String => Unit = { s:String => sb.append(leadingDelim(s)) }
    val f2 : String => Unit = { s:String => sb.append(token(s)) }
    val f3 : String => Unit = { s:String => sb.append(delimiter(s)) }
    val f4 : String => Unit = { s:String => sb.append(trailingDelim(s)) }
    foreach(
      leadingDelim = f1,
      token = f2,
      delim = f3,
      trailingDelim = f4
    )
    sb.result()
  }

  override def toString = {
    val sb = new StringBuilder
    sb.append("LexResult(")
    foreach(
      leadingDelim = { s:String => sb.append(s"LeadingDelim($s),") },
      token = { s:String => sb.append(s"Token($s),") },
      delim = { s:String => sb.append(s"Delim($s),") },
      trailingDelim = { s:String => sb.append(s"TrailingDelim($s),") }
    )
    sb.dropRight(1).append(")").result()
  }

}
