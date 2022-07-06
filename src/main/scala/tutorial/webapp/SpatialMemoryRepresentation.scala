package tutorial.webapp

abstract class SpatialMemoryRepresentation(val width: Int, val height: Int) {
  def addressToSquare(address: Int): GridSquare
  def squareToAddress(square: GridSquare): Int
}

class ScanMemoryRepresentation(width: Int, height: Int)
    extends SpatialMemoryRepresentation(width, height) {
  override def addressToSquare(address: Int): GridSquare = {
    val row = address / width
    val posInRow = address % width
    val col = if row % 2 == 0 then posInRow else width - 1 - posInRow
    GridSquare(row, col)
  }
  override def squareToAddress(square: GridSquare): Int = {
    val posInRow = if square.row % 2 == 0 then square.col else width - 1 - square.col
    square.row * width + posInRow
  }
}