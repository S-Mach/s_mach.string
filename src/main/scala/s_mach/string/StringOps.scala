package s_mach.string

import scala.util.matching.Regex
import scala.util.matching.Regex.Match

object StringOps {

  /** @return string with suffix appended if string does not end with suffix */
  def ensureSuffix(s: String, suffix: String) : String = {
    if(s.endsWith(suffix)) s else s + suffix
  }

  /*/** @return string with all occurrences of regex replaced with the paired string returned by supplying the match generated by the regex. Ensures recursive replacements cannot occur. */
  def findReplaceRegex(
    s: String,
    zomRegex: (Regex, Match => String)*
  ) : String = ???

  /** @return string with all occurrences of regex replaced with the paired string. Ensures recursive replacements cannot occur. */
  def findReplaceRegex(
    s: String,
    zomRegex: (Regex, String)*
  ) : String = ???

  /** @return string with all replacements. For each (find,replace) pair, all occurrences of find are substituted with replace. Ensures recursive replacements cannot occur. */
  def findReplace(
    s: String,
    caseSensitive: Boolean = true,
    fr: (String, String)*
  ) : String = ???

  /** @return string with all replacements. For each (find,replace) pair, all occurrences of find as a word are substituted with replace. Ensures recursive replacements cannot occur. */
  def findReplaceWords(
    s: String,
    caseSensitive: Boolean = true,
    fr: (String, String)*
  )(implicit splitter:WordSplitter) : String = ???*/

  /*/** @return string with all replacements. For each (find*,replace) pair, all occurrences of find are substituted with replace. Ensures recursive replacements cannot occur. */
  def findAllReplace(
    s: String,
    caseSensitive: Boolean = true,
    fr: (String*, String)*
  ) : String = ???

  /** @return string with all replacements. For each (find*,replace) pair, all occurrences of find as a word are substituted with replace. Ensures recursive replacements cannot occur. */
  def findAllReplaceWords(
    s: String,
    caseSensitive: Boolean = true,
    fr: (String*, String)*
  )(implicit splitter:WordSplitter) : String = ???*/

  /** @return string with all whitespace collapsed to a single space and all leading and trailing whitespace trimmed */
  def collapseWhitespace(s: String) : String = ???

  /** @return the first character to uppercase and the remaining characters to lowercase */
  def toProperCase(s: String) : String = s.capitalize

  /** @return all whitespace collapsed, each word in proper case */
  def toTitleCase(s: String)(implicit words:WordSplitter) : String = ???

  /** @return all words camel-cased i.e. camelCase */
  def toCamelCase(s: String)(implicit words:WordSplitter) : String = ???

  /** @return all words camel-cased i.e. CamelCase */
  def toPascalCase(s: String)(implicit words:WordSplitter) : String = ???

  /** @return all words camel-cased i.e. camel_case */
  def toSnakeCase(s: String)(implicit words:WordSplitter) : String = ???

  /** @return string with all lines indented by n occurrences of s */
  def indent(s: String, n: Int, spacer: String = " ") : String = {
    //s.split("\n").map {case line => spacer * n + line}.mkString("\n")
    s.linesWithSeparators.map {case line => spacer * n + line}.mkString
  }

  /** @return None if string length is 0 OR Some(String) if length > 0 */
  def toOption(s: String) : Option[String] = {
    if(s.length == 0) None else Some(s)
  }

  /** @return None if String fails to convert to Double OR Some(Double) if string can be converted to a valid Double value */
  def toDoubleOpt(s: String) : Option[Double] = convert[Double](s, java.lang.Double.parseDouble)

  /** @return None if String fails to convert to Long OR Some(Long) if string can be converted to a valid Long value */
  def toLongOpt(s: String) : Option[Long] = convert[Long](s, java.lang.Long.parseLong)

  /** @return None if String fails to convert to Int OR Some(Int) if string can be converted to a valid Int value */
  def toIntOpt(s: String) : Option[Int] = convert[Int](s, java.lang.Integer.parseInt)

  /** @return None if String fails to convert to A OR Some(A) if string can be converted to a valid A value */
  def convert[A](s: String, f: String => A) : Option[A] = {
    try {
      Some(f(s))
    } catch {
      case e : Exception => None
    }
  }

  /** @return all words contained in string */
  def toWords(s: String)(implicit splitter:WordSplitter) : Iterator[String] = splitter.split(s)
}
