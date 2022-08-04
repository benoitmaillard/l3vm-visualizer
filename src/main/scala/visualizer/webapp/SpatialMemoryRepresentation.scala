package visualizer.webapp

/** Mapping from an address to a position on a discrete grid and vice-versa
 * 
 * @param width
 * @param height
 * 
 */
abstract class SpatialMemoryRepresentation(val width: Int, val height: Int) {
  /** Transforms an address into a position
   * 
   * @param address address of the word
   * @return position on the grid
  */
  def addressToSquare(address: Int): GridSquare

  /** Transforms a position on the grid into an address 
   * 
   * @param square position on the grid
   * @return address of the word
  */
  def squareToAddress(square: GridSquare): Int
}


/** Representation where words are placed on the grid from left to right on
 *  even rows (starting at row 0) and from right to left on odd rows
 */
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