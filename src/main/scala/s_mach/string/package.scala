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
package s_mach

import scala.util.matching.Regex
import scala.util.matching.Regex.Match
import s_mach.string.impl.{PrintGridImpl, StringOps}

/**
 * Note1 : ensures recursive replacements cannot occur. Once a section of the
 * string has been matched, it cannot be matched again later. All replacements
 * occur only after all  matching has finished.
 * */
package object string {

  implicit class S_Mach_String_StringPML(val self: String) extends AnyVal {
    /**
     * Replace string sections by matching regex and replacing with result of
     * match function.
      *
      * @param zomRegex zero or more (regex, match function) tuples
     * @return string with all occurrences of regexes replaced with the string
     *         returned by passing the match generated by the regex to the
     *         paired function. See note1 above.
    */
    @inline def findRegexReplaceMatch(
      zomRegex: Seq[(Regex, Match => String)]
    ) : String = StringOps.findRegexReplaceMatch(self, zomRegex)

    /**
     * Replace string sections by matching regex and replacing with a string
     * literal.
      *
      * @param zomRegex zero or more (regex, replacement string literal) tuples
     * @return string with all occurrences of regex replaced with the paired
     *         string. See note1 above.
     */
    @inline def findRegexReplace(
      zomRegex: Seq[(Regex, String)]
    ) : String = StringOps.findRegexReplace(self, zomRegex)

    /**
     * Replace string sections by matching string literal with a replacement
     * string literal.
      *
      * @param zomFindReplace zero or more (find string literal, replace string
     *                       literal) tuples
     * @param caseSensitive TRUE to for case-sensitive matching FALSE otherwise
     * @return string with all replacements. For each (find,replace) pair, all
     *         occurrences of find that match regions of the string substituted
     *         with replace. See note1 above.
     */
    @inline def findReplace(
      zomFindReplace: Seq[(String, String)],
      caseSensitive: Boolean = true
    ) : String = StringOps.findReplace(self, caseSensitive, zomFindReplace)

    /**
     * Replace tokens in a string by matching string literal and replacing with a
     * replacement string literal.
      *
      * @param zomFindReplace zero or more (find string literal, replace string
     *                       literal) tuples
     * @param caseSensitive TRUE to for case-sensitive matching FALSE otherwise
     * @return string with all replacements. For each (find,replace) pair, all
     *         occurrences of find that match a token exactly are substituted
     *         with replace. See note1 above.
     */
    @inline def findReplaceTokens(
      zomFindReplace: Seq[(String, String)],
      caseSensitive: Boolean = true
    )(implicit lexer: Lexer): String =
      StringOps.findReplaceTokens(self, caseSensitive, zomFindReplace)

    /**
     * Replace string sections by matching from a set of string literals and
     * replacing with a replacement string literal.
      *
      * @param zomFindReplace zero or more (find string literal, replace string
     *                       literal) tuples
     * @param caseSensitive TRUE to for case-sensitive matching FALSE otherwise
     * @return string with all replacements. For each (find*,replace) pair, any
     *         occurrences of any find are substituted with replace. See note1
     *         above. */
    @inline def findAllReplace(
      zomFindReplace: Seq[(Seq[String], String)],
      caseSensitive: Boolean
    ) : String = StringOps.findAllReplace(self, caseSensitive, zomFindReplace)

    /**
     * Replace tokens in string sections by matching from a set of string
     * literals to a replacement string literal.
      *
      * @param zomFindReplace zero or more (find string literal, replace string
     *                       literal) tuples
     * @param caseSensitive TRUE to for case-sensitive matching FALSE otherwise
     * @return string with all replacements. For each (find*,replace) pair, any
     *         occurrences of any find are substituted with replace. See note1
     *         above. */
    @inline def findAllReplaceTokens(
      zomFindReplace: Seq[(Seq[String], String)],
      caseSensitive: Boolean = true
    )(implicit lexer:Lexer) : String =
      StringOps.findAllReplaceTokens(self, caseSensitive, zomFindReplace)

    /**
     * @param suffix string to ensure is at the end of the string
     * @return string with suffix appended if string does not end with suffix
     */
    @inline def ensureSuffix(suffix: String) : String =
      StringOps.ensureSuffix(self, suffix)

    /**
     * In-place transform tokens in the string preserving delimiters
     *
     * @param f function to map tokens
     * @return a string with all tokens replaced with the result of passing the
     *         token to f. all delimiters is preserved */
    @inline def mapTokens(
      f: String => String
    )(implicit
      lexer: Lexer
    ) : String =
      StringOps.mapTokens(self)(f)

    /**
     * Collapse all delimiters to the same string and trim all leading and trailing
     * delimiters
     *
     * @param delimiterSubst string to replace delimiter runs with
     * @return string with all delimiters collapsed to delimiterSubst and all
      *         leading and trailing delimiters trimmed */
    @inline def collapseDelims(
      delimiterSubst: String
    )(implicit
      lexer: Lexer
    ) : String =
      StringOps.collapseDelims(self, delimiterSubst)

    /** @return string with all whitespace collapsed to a single space and all
      *         leading and trailing whitespace trimmed */
    @inline def collapseWhitespace : String = StringOps.collapseWhitespace(self)

    /** @return the first character to uppercase and the remaining characters to
      *         lowercase */
    @inline def toProperCase : String = StringOps.toProperCase(self)

    /** @return each token in proper case with delimiter preserved */
    @inline def toTitleCase(implicit lexer:Lexer) : String =
      StringOps.toTitleCase(self)

    /** @return all tokens camel-cased i.e. camelCase */
    @inline def toCamelCase(implicit lexer:Lexer) : String =
      StringOps.toCamelCase(self)

    /** @return all tokens camel-cased i.e. CamelCase */
    @inline def toPascalCase(implicit lexer:Lexer) : String =
      StringOps.toPascalCase(self)

    /** @return all tokens camel-cased i.e. camel_case */
    @inline def toSnakeCase(implicit lexer:Lexer) : String =
      StringOps.toSnakeCase(self)

    /** @return all tokens contained in string */
    @inline def toTokens(implicit lexer:Lexer) : Iterator[String] =
      lexer.tokens(self)

    /** @return lazily separate all tokens and delimiters contained in string */
    @inline def lex(implicit
      lexer:Lexer
    ) : LexResult =
      lexer.lex(self)

    /**
     * @param n number of spacers to indent with
     * @param spacer the spacer string to use while indenting
     * @return string with all lines indented by n occurrences of s */
    @inline def indent(n: Int, spacer: String = " ") : String =
      StringOps.indent(self, n, spacer)

    /** @return None if string length is 0 OR Some(String) if length > 0 */
    @inline def toOption : Option[String] = StringOps.toOption(self)

    /** @return None if String fails to convert to Double OR Some(Double) if
      *         string can be converted to a valid Double value */
    @inline def toDoubleOpt : Option[Double] = StringOps.toDoubleOpt(self)

    /** @return None if String fails to convert to Long OR Some(Long) if string
      *         can be converted to a valid Long value */
    @inline def toLongOpt : Option[Long] = StringOps.toLongOpt(self)

    /** @return None if String fails to convert to Int OR Some(Int) if string
      *         can be converted to a valid Int value */
    @inline def toIntOpt : Option[Int] = StringOps.toIntOpt(self)

    /**
     * Parse a string to some type
      *
      * @param f function to parse string to type
     * @return None if String fails to convert to A OR Some(A) if string can be
      *         converted to a valid A value */
    @inline def convert[A](f: String => A) : Option[A] =
      StringOps.convert(self, f)

  }

  implicit class S_Mach_String_GridPML(val self: IndexedSeq[IndexedSeq[String]]) extends AnyVal {
    /**
      * Print a NxM formatted grid of strings padding cells with whitespace as
      * needed to left align cells
      *
      * @return formatted grid string
      */
    def printGrid : String = PrintGridImpl.printGrid(self)
  }
}
