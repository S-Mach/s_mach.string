package s_mach.string

import scala.collection.mutable.ArrayBuffer
import scala.util.matching.Regex

trait WordSplitter {

  val whiteSpace = """\s+""".r
  val whiteSpaceOrUnderscores = """(\s|_)+""".r
  val allLowerPrefix = """[a-z_]+""".r
  val capitalizedWord = """([A-Z]+[a-z_0-9]*)""".r

  def split(s: String) : Iterator[String]

  /**
   * This method handles an optional prefix on a split
   * @param optPrefix optional prefix
   * @param str string to split
   * @param regex regex to split by.
   * @return iterator of words that match the regex and the optional prefix prepended
   */
  def splitterAccumulate(optPrefix : Option[String], str : String, regex : Regex) : Iterator[String] = {
    //TODO There's probably a less ugly way to do this
    val accum = ArrayBuffer(optPrefix.getOrElse(""))
    regex.findAllIn(str).foreach {
      w => accum.append(w)
    }
    accum.iterator
  }

}



class WhitespaceWordSplitter extends WordSplitter {
  override def split(s: String): Iterator[String] = whiteSpace.split(s).iterator
}

class WhitespaceOrUnderscoreWordSplitter extends WordSplitter {
  override def split(s: String): Iterator[String] = whiteSpaceOrUnderscores.split(s).iterator
}

class CamelCaseWordSplitter extends WordSplitter {
  override def split(s: String): Iterator[String] = {
    splitterAccumulate(allLowerPrefix.findFirstIn(s), s, capitalizedWord)
  }
}

class PascalCaseWordSplitter extends WordSplitter {
  override def split(s: String): Iterator[String] = {
    splitterAccumulate(None, s, capitalizedWord)
  }
}

object WordSplitter {
  object Whitespace extends WhitespaceWordSplitter
  object WhitespaceOrUnderscore extends WhitespaceOrUnderscoreWordSplitter
  object CamelCase extends CamelCaseWordSplitter
  object PascalCase extends PascalCaseWordSplitter
}