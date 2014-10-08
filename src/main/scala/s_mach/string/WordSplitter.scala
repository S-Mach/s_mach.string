package s_mach.string

trait WordSplitter {
  def split(s: String) : Iterator[String]
}

class WhitespaceWordSplitter extends WordSplitter {
  override def split(s: String): Iterator[String] = s.split("( )+").iterator
}

class WhitespaceOrUnderscoreWordSplitter extends WordSplitter {
  override def split(s: String): Iterator[String] = s.split("(( )+)|(_)+").filter(!_.isEmpty).iterator
}

class CamelCaseWordSplitter extends WordSplitter {
  override def split(s: String): Iterator[String] = ???
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