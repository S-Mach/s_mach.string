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
 * A trait for the lazy result of a word split
 */
trait WordSplitResult {
  /**
   * Iterate over words and glue contained in the string
   * @param leadingGlue function called if string contains leading glue (if any)
   * @param word function called if string contains a word (if any)
   * @param glue function called if string contains glue following a word (if
   *             any)
   * @param trailingGlue function called if string contains trailing glue (if
   *                     any)
   */
  def foreach(
    leadingGlue: String => Unit,
    word: String => Unit,
    glue: String => Unit,
    trailingGlue: String => Unit
  ) : Unit

  /**
   * Map over word and glue contained in the string
   * @param leadingGlue function called if string contains leading glue (if any)
   * @param word function called if string contains a word (if any)
   * @param glue function called if string contains glue following a word (if
   *             any)
   * @param trailingGlue function called if string contains trailing glue (if
   *                     any)
   * @return a string that is the result of concatenating the returned strings
   *         from all supplied functions
   */
  def map(
    leadingGlue: String => String,
    word: String => String,
    glue: String => String,
    trailingGlue: String => String
  ) : String
}

