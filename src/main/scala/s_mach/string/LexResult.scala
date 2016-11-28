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
package s_mach.string

/**
 * A trait for the lazy result of a lexing
 */
trait LexResult {
  /**
   * Iterate over tokens and delimiter contained in the string
   * @param leadingDelim function called if string contains leading delimiter (if any)
   * @param token function called if string contains a token (if any)
   * @param delim function called if string contains delimiter following a token (if
   *             any)
   * @param trailingDelim function called if string contains trailing delimiter (if
   *                     any)
   */
  def foreach(
    leadingDelim: String => Unit,
    token: String => Unit,
    delim: String => Unit,
    trailingDelim: String => Unit
  ) : Unit

  /**
   * Map over token and delimiter contained in the string
   * @param leadingDelim function called if string contains leading delimiter (if any)
   * @param token function called if string contains a token (if any)
   * @param delim function called if string contains delimiter following a token (if
   *             any)
   * @param trailingDelim function called if string contains trailing delimiter (if
   *                     any)
   * @return a string that is the result of concatenating the returned strings
   *         from all supplied functions
   */
  def map(
    leadingDelim: String => String,
    token: String => String,
    delim: String => String,
    trailingDelim: String => String
  ) : String
}

