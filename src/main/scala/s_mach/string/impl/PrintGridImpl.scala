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
