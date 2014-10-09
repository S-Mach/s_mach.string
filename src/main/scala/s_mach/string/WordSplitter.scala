package s_mach.string

trait WordSplitter {
  def split(s: String) : Iterator[String]
}

class WhitespaceWordSplitter extends WordSplitter {
  override def split(s: String): Iterator[String] = s.split("( )+|(\\n)").iterator
}

class WhitespaceOrUnderscoreWordSplitter extends WordSplitter {
  override def split(s: String): Iterator[String] = s.split("(( )+)|(_)+|(\\n)").filter(!_.isEmpty).iterator
}

class CamelCaseWordSplitter extends WordSplitter {

  override def split(s: String): Iterator[String] = { ???
    /*
    val accumulator = List[String]()
    val prefix = "[a-z_]+".r
    val word = "([A-Z]+[a-z_0-9]*)".r
    prefix.findPrefixOf(s).getOrElse("") :: accumulator
    for(matchedWord <- word.findAllMatchIn(s)) matchedWord :: accumulator
    accumulator.iterator
    */
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