package s_mach.string

import s_mach.string.WordSplitter.Whitespace

import scala.collection.mutable._
import scala.util.matching.Regex
import scala.util.matching.Regex.Match

object StringOps {

  /** @return string with suffix appended if string does not end with suffix */
  def ensureSuffix(s: String, suffix: String) : String = {
    if(s.endsWith(suffix)) s else s + suffix
  }

  /** @return string with all occurrences of regex replaced with
    *         the paired string returned by supplying the match generated by the regex.
    *         Ensures recursive replacements cannot occur.
    */
  def findRegexReplaceMatch(s: String, zomRegex: collection.immutable.Seq[(Regex, Match => String)]) : String = {
    import scala.language.implicitConversions
    implicit def match2tuple(m : Match) : (Int, Int) = (m.start, m.end)
    def overlaps(set :scala.collection.mutable.Set[(Int,Int)], region : (Int, Int)) : Boolean = {
      set.exists { case (begin, end) =>
        region._1 > begin && region._1 < end ||
          region._2 > begin && region._2 < end
      }
    }
    def matchLength(m : Match) = m.end - m.start

    val matchedRegions = HashSet[(Int,Int)]()
    val willReplace = ArrayBuffer[(Match,  Match => String)]()
    for((regex, matcher) <- zomRegex) {
      regex.findAllMatchIn(s).foreach{ match_ =>
        if(!overlaps(matchedRegions, match_)) {
          matchedRegions += match_
          willReplace.append((match_, matcher))
        }
      }
    }
    var strList = s.toList
    var offset = 0
    willReplace.foreach { case (match_, matcher) =>
      strList = strList.patch(offset+match_.start, matcher(match_), matchLength(match_) )
      offset += matcher(match_).length - matchLength(match_)
    }
    strList.mkString
  }

  /** @return string with all occurrences of regex replaced with the paired string. Ensures recursive replacements cannot occur. */
  def findRegexReplace(
    s: String,
    zomRegex: Seq[(Regex, String)]
  ) : String = ???

  /** @return string with all replacements. For each (find,replace) pair, all occurrences of find are substituted with replace. Ensures recursive replacements cannot occur. */
  def findReplace(
    s: String,
    caseSensitive: Boolean,
    fr: Seq[(String, String)]
  ) : String = ???

  /** @return string with all replacements. For each (find,replace) pair, all occurrences of find as a word are substituted with replace. Ensures recursive replacements cannot occur. */
  def findReplaceWords(
    s: String,
    caseSensitive: Boolean,
    fr: Seq[(String, String)]
  )(implicit splitter:WordSplitter) : String = {
    ???
  }

  /** @return string with all replacements. For each (find*,replace) pair, all occurrences of find are substituted with replace. Ensures recursive replacements cannot occur. */
  def findAllReplace(
    s: String,
    caseSensitive: Boolean,
    fr: Seq[(Seq[String], String)]
  ) : String = ???

  /** @return string with all replacements. For each (find*,replace) pair, all occurrences of find as a word are substituted with replace. Ensures recursive replacements cannot occur. */
  def findAllReplaceWords(
    s: String,
    caseSensitive: Boolean,
    fr: Seq[(Seq[String], String)]
  )(implicit splitter:WordSplitter) : String = ???

  /** @return string with all whitespace collapsed to a single space and all leading and trailing whitespace trimmed */
  def collapseWhitespace(s: String) : String = Whitespace.split(s).mkString(" ").trim

  /** @return the first character to uppercase and the remaining characters to lowercase */
  def toProperCase(s: String) : String = {
    s.length match {
      case long if long > 1 => s.head.toUpper + s.tail.toLowerCase
      case oneChar if oneChar == 1 => s.head.toTitleCase.toString
      case _ => ""
    }
  }

  /**
   * Helper method as the other transforms essentially follow the same pattern
   * @param s string to transform
   * @param transform String => String transform that we apply on each split string
   * @param concatenator parameter to mkString. defaults to empty string
   * @param words implicit splitter
   * @return transformed sequence of words
   */
  private def mapWithTransform(s : String, transform: String => String, concatenator: String = "")(implicit words:WordSplitter) : String = {
    words.split(s).map(transform).mkString(concatenator)
  }

  /** @return all whitespace collapsed, each word in proper case */
  def toTitleCase(s: String)(implicit words:WordSplitter) : String = {
    mapWithTransform(s, toProperCase, " ")
  }


  /** @return all words camel-cased i.e. camelCase */
  def toCamelCase(s: String)(implicit words:WordSplitter) : String = {
    val iter = words.split(s)
    val prefix = iter.next().toLowerCase
    prefix + iter.map(toProperCase).mkString
  }

  /** @return all words camel-cased i.e. CamelCase */
  def toPascalCase(s: String)(implicit words:WordSplitter) : String = {
    mapWithTransform(s, toProperCase)
  }

  /** @return all words camel-cased i.e. camel_case */
  def toSnakeCase(s: String)(implicit  words:WordSplitter) : String = {
    mapWithTransform(s, _.toLowerCase, "_")
  }

  /** @return string with all lines indented by n occurrences of s */
  def indent(s: String, n: Int, spacer: String = " ") : String = {
    val builder : StringBuilder = StringBuilder.newBuilder
    val indent = spacer * n
    s.linesWithSeparators.foreach {
      line => builder.append(indent + line)
    }
    builder.toString()
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
    }
    catch {
      case e : Exception  => None
    }
  }

  /** @return all words contained in string */
  def toWords(s: String)(implicit splitter:WordSplitter) : Iterator[String] = splitter.split(s)
}
