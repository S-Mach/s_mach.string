package s_mach.string

import scala.collection.mutable.ArrayBuffer
import scala.util.matching.Regex

trait WordSplitter {

  def split(s: String) : Iterator[String]

  /**
   * This method handles an optional prefix on a split
   * @param optPrefix optional prefix
   * @param str string to split
   * @param regex regex to split by.
   * @return iterator of words that match the regex and the optional prefix prepended
   */
  protected def splitterAccumulate(optPrefix : Option[String], str : String, regex : Regex) : Iterator[String] = {
    //TODO There's probably a less ugly way to do this
    val accum = ArrayBuffer[String]()
    optPrefix.collect{ case str => accum.append(str) }
    regex.findAllIn(str).foreach {
      w => accum.append(w)
    }
    if(accum.isEmpty) accum.append("")
    accum.iterator
  }
}

class WhitespaceWordSplitter extends WordSplitter {
  import WordSplitter.whiteSpace
  override def split(s: String): Iterator[String] = whiteSpace.split(s).iterator
}

class WhitespaceOrUnderscoreWordSplitter extends WordSplitter {
  import WordSplitter.whiteSpaceOrUnderscores
  override def split(s: String): Iterator[String] = whiteSpaceOrUnderscores.split(s).iterator
}

class CamelCaseWordSplitter extends WordSplitter {
  import WordSplitter.{allLowerPrefix, capitalizedWord}
  override def split(s: String): Iterator[String] = {
    splitterAccumulate(allLowerPrefix.findFirstIn(s), s, capitalizedWord)
  }
}

class PascalCaseWordSplitter extends WordSplitter {
  import WordSplitter.{capitalizedWord}
  override def split(s: String): Iterator[String] = {
    splitterAccumulate(None, s, capitalizedWord)
  }
}

object WordSplitter {

  val whiteSpace = """\s+""".r
  val whiteSpaceOrUnderscores = """(\s|_)+""".r
  val allLowerPrefix = """[a-z_]+""".r
  val capitalizedWord = """([A-Z]+[a-z_0-9]*)""".r

  object Whitespace extends WhitespaceWordSplitter
  object WhitespaceOrUnderscore extends WhitespaceOrUnderscoreWordSplitter
  object CamelCase extends CamelCaseWordSplitter
  object PascalCase extends PascalCaseWordSplitter
}