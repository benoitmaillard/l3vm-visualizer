package tutorial.webapp

abstract class SpatialMemoryRepresentation(width: Int, height: Int) {
  def addressToSquare(address: Int): (Int, Int)
  def squareToAddress(row: Int, col: Int): Int
}

class ScanMemoryRepresentation(width: Int, height: Int)
    extends SpatialMemoryRepresentation(width, height) {
  override def addressToSquare(address: Int): (Int, Int) = {
    val row = address / height
    val posInRow = address % width
    val col = if row % 2 == 0 then posInRow else width - posInRow
    (col, row)
  }
  override def squareToAddress(row: Int, col: Int): Int = {
    val posInRow = if row % 2 == 0 then col else width - col
    row * width + posInRow
  }
}