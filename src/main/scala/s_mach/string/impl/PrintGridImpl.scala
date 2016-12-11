/*
                    ,i::,
               :;;;;;;;
              ;:,,::;.
            1ft1;::;1tL
              t1;::;1,
               :;::;               _____       __  ___              __
          fCLff ;:: tfLLC         / ___/      /  |/  /____ _ _____ / /_
         CLft11 :,, i1tffLi       \__ \ ____ / /|_/ // __ `// ___// __ \
         1t1i   .;;   .1tf       ___/ //___// /  / // /_/ // /__ / / / /
       CLt1i    :,:    .1tfL.   /____/     /_/  /_/ \__,_/ \___//_/ /_/
       Lft1,:;:       , 1tfL:
       ;it1i ,,,:::;;;::1tti      s_mach.string
         .t1i .,::;;; ;1tt        Copyright (c) 2016 S-Mach, Inc.
         Lft11ii;::;ii1tfL:       Author: lance.gatlin@gmail.com
          .L1 1tt1ttt,,Li
            ...1LLLL...
*/
package s_mach.string.impl

object PrintGridImpl {
  // maybe make these configurable?
  val colSep = " "
  val rowSep = "\n"

  def printGrid[A](
    data: IndexedSeq[IndexedSeq[String]]
  ) : String = {
    val rowCount = data.size
    val colSepLen = colSep.length
    if(data.nonEmpty) {
      val colCount = data.head.size
      val maxColWidths = (0 until colCount).map { colIdx =>
        (0 until rowCount).iterator.map(rowIdx => data(rowIdx)(colIdx).length).max
      }
      val sb = new StringBuilder(512)
      data.foreach { row =>
        row.init.zipWithIndex.foreach { case (cell,colIdx) =>
          val maxColWidth = maxColWidths(colIdx)
          sb.append(cell)
          val sepsNeeded = ((maxColWidth - cell.length) / colSepLen) + 1
          for(_ <- 0 until sepsNeeded) {
            sb.append(colSep)
          }
        }
        sb.append(row.last)
        sb.append(rowSep)
      }
      sb.result()
    } else {
      ""
    }
  }
}
