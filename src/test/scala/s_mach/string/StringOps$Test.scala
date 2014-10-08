package s_mach.string

import StringOps._

import org.scalatest.{Matchers, FlatSpec}

/**
 * Created by Gustavo on 10/7/14.
 */
class StringOps$Test extends FlatSpec with Matchers{

  "indent()" should "add an indentation format" in {
    val initString =
      """
        |int a = 10;
        |hello!
      """.stripMargin
    indent(initString,4," ") should be
      """
        |    int a = 10;
        |    hello!
      """.stripMargin
    indent(initString,4," ") should not be
    """
      |int a = 10;
      |hello!
    """.stripMargin
  }
}
