package s_mach.string

import org.scalatest.{FlatSpec, Matchers}


class PrintGridTest extends FlatSpec with Matchers {
  "Grid.printGrid" should "print a formatted grid" in {
    IndexedSeq(
      IndexedSeq("a a","bb","c","d"),
      IndexedSeq("d","eee","f","g"),
      IndexedSeq("hh","i","jjjj","k")
    ).printGrid shouldBe
"""
a a bb  c    d
d   eee f    g
hh  i   jjjj k
""".trim + "\n"
  }
}
