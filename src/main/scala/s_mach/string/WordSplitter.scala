package s_mach.string

import scala.util.matching.Regex

trait WordSplitter {
  def split(s: String) : Iterator[String]
}


class WhitespaceWordSplitter extends WordSplitter {
  // TODO: since this will be widely used library, use pre-compiled regex here
  // TODO: use \s instead of '  ' to match all whitespace (tabs, new lines)
  override def split(s: String): Iterator[String] = s.split("( )+").iterator
}

class WhitespaceOrUnderscoreWordSplitter extends WordSplitter {
  // TODO: since this will be widely used library, use pre-compiled regex here
  // TODO: use \s instead of '  ' to match all whitespace (tabs, new lines)
  // TODO: I think regex should be "(\s|_)+"
  override def split(s: String): Iterator[String] = s.split("(( )+)|(_)+").filter(!_.isEmpty).iterator
}

class CamelCaseWordSplitter extends WordSplitter {

  override def split(s: String): Iterator[String] = {

    val accumulator = List[String]()
    // TODO: move these to a companion object so that they don't have to be compiled every call
    val prefix = "[a-z_]+".r
    val word = "([A-Z]+[a-z_0-9]*)".r
    prefix.findPrefixOf(s).getOrElse("") :: accumulator
    // TODO: your list is immutable but this expression returns unit so this isn't doing what you want
    // TODO: why not just call word.findAllMatchIn(s).map { ... } ? and avoid computing results unless they are needed?
    for(matchedWord <- word.findAllMatchIn(s)) matchedWord :: accumulator
    accumulator.iterator
  }
}

class PascalCaseWordSplitter extends WordSplitter {
  override def split(s: String): Iterator[String] = ???
}

object WordSplitter {
  object Whitespace extends WhitespaceWordSplitter
  object WhitespaceOrUnderscore extends WhitespaceOrUnderscoreWordSplitter
  object CamelCase extends CamelCaseWordSplitter
  object PascalCase extends PascalCaseWordSplitter
}