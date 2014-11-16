package s_mach.string

import scala.util.matching.Regex

trait WordSplitter {

  /**
   * Splits a string based on some boundary condition
   * @param s string to split
   * @return Iterator of the elements created from the split
   */
  def split(s: String) : Iterator[String]

  /**
   * Splits a string based on some boundary condition and also preserves
   * the characters that were between the boundaries.
   * ([content], [glue])
   * @param s string to split
   * @return Iterator of the split and the intermediate characters
   */
  def splitWithGlue(s : String) : Iterator[(String, String)]
  /**
   * This method handles an optional prefix on a split
   * @param optPrefix optional prefix
   * @param str string to split
   * @param regex regex to split by.
   * @return iterator of words that match the regex and the optional prefix prepended
   */
  protected def splitterAccumulate(optPrefix : Option[String], str : String, regex : Regex) : Iterator[String] = {
    if(str.nonEmpty) {
      val accum = List.newBuilder[String]
      optPrefix.foreach( accum.+= )
      regex.findAllIn(str).foreach(accum.+=)
      accum.result().iterator
    } else {
      Iterator("")
    }
  }

  /**
   * Helper method for the glue-aware split methods
   * @param glueDetector Magic regex that splits strings at boundaries of a particular patter
   * @param glue the regular splitting regex that these objects use
   * @param s string to split
   * @return An Iterator[(String, String)] with (word, glue) elements
   */
  protected def glueSplit(glueDetector : Regex, glue : Regex, s : String) : Iterator[(String, String)] = {
    glue.findFirstMatchIn(s) match {
      case Some(beginGlue) =>
        if (beginGlue.start == 0) {
          Iterator(("", beginGlue.toString())) ++
            glueDetector.split(s).sliding(2, 2).map { a => if (a.length == 2) (a(1), a(0)) else (a(0), "")} //voodoo
        } else {
          glueDetector.split(s).sliding(2, 2).map { a => if (a.length == 2) (a(0), a(1)) else (a(0), "")}
        }
      case None => glue.split(s).map(a => (a, "")).iterator
    }
  }
}

class WhitespaceWordSplitter extends WordSplitter {
  import WordSplitter._

  override def split(s: String): Iterator[String] = whiteSpace.split(s).iterator

  override def splitWithGlue(s: String): Iterator[(String, String)] = {
    glueSplit(magicWhiteSpace, whiteSpace, s)
  }
}

class WhitespaceOrUnderscoreWordSplitter extends WordSplitter {
  import WordSplitter._

  override def splitWithGlue(s: String): Iterator[(String, String)] = {
    glueSplit(magicWhitespaceOrUnderscores, whiteSpaceOrUnderscores, s)
  }

  override def split(s: String): Iterator[String] = whiteSpaceOrUnderscores.split(s).iterator
}

class CamelCaseWordSplitter extends WordSplitter {
  import WordSplitter.{allLowerPrefix, capitalizedWord}

  override def splitWithGlue(s: String): Iterator[(String, String)] = ???

  override def split(s: String): Iterator[String] = {
    splitterAccumulate(allLowerPrefix.findFirstIn(s), s, capitalizedWord)
  }
}

class PascalCaseWordSplitter extends WordSplitter {
  import WordSplitter.capitalizedWord

  override def splitWithGlue(s: String): Iterator[(String, String)] = ???

  override def split(s: String): Iterator[String] = {
    splitterAccumulate(None, s, capitalizedWord)
  }
}

object WordSplitter {

  val magicWhiteSpace = "(?<=\\S)(?=\\s)|(?<=\\s)(?=\\S)".r
  val magicWhitespaceOrUnderscores= "(?<=[^_])(?=(_|\\s))|(?<=(\\s|_))(?=[^_\\s])".r
  val whiteSpace = """\s+""".r
  val whiteSpaceOrUnderscores = """(\s|_)+""".r
  val allLowerPrefix = """[a-z_]+""".r
  val capitalizedWord = """([A-Z]+[a-z_0-9]*)""".r

  object Whitespace extends WhitespaceWordSplitter
  object WhitespaceOrUnderscore extends WhitespaceOrUnderscoreWordSplitter
  object CamelCase extends CamelCaseWordSplitter
  object PascalCase extends PascalCaseWordSplitter
}