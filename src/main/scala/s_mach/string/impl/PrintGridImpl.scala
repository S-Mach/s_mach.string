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
  private def cell(data: IndexedSeq[IndexedSeq[String]], rowIdx: Int, colIdx: Int) : String =
    if(rowIdx < data.size) {
      val row = data(rowIdx)
      if(colIdx < row.size) {
        row(colIdx)
      } else {
        ""
      }
    } else {
      ""
    }

  private def appendRow(
    sb: StringBuilder,
    maxColWidths: IndexedSeq[Int],
    colDelim: Char,
    colDelimCount: Int,
    rowDelim: String,
    row: IndexedSeq[String]
  ) : Unit = {
    // all but last column
    row.init.zipWithIndex.foreach { case (cell,colIdx) =>
      val maxColWidth = maxColWidths(colIdx)
      sb.append(cell)
      val sepsNeeded = (maxColWidth - cell.length) + colDelimCount
      for(_ <- 0 until sepsNeeded) {
        sb.append(colDelim)
      }
    }
    // last column
    sb.append(row.last)
    ()
  }

  def printGrid[A](
    data: IndexedSeq[IndexedSeq[String]],
    colDelim: Char,
    colDelimCount: Int,
    rowDelim: String
  ) : String = {
    val rowCount = data.size
    if(data.nonEmpty) {
      val maxColCount = data.head.size
      val maxColWidths = (0 until maxColCount).map { colIdx =>
        (0 until rowCount).iterator.map { rowIdx =>
          cell(data,rowIdx,colIdx).length
        }.max
      }
      val sb = new StringBuilder(512)
      // all but last row
      data.init.foreach { row =>
        appendRow(sb, maxColWidths, colDelim, colDelimCount, rowDelim, row)
        sb.append(rowDelim)
        ()
      }
      // last row
      appendRow(sb, maxColWidths, colDelim, colDelimCount, rowDelim, data.last)
      sb.result()
    } else {
      ""
    }
  }
}
