package visualizer.webapp

/** Rectangle that forms a subset of a discrete grid 
 * 
 * @param minRow y coordinate of top-left corner
 * @param minCol x coordinate of top-left corner
 * @param height
 * @param width
*/
case class GridRectangle(minRow: Int, minCol: Int, height: Int, width: Int) {

  /** Checks if a square is inside the rectangle 
   * 
   * @param square
  */
  def contains(square: GridSquare) =
    square.row >= minRow
      && square.col >= minCol
      && square.row < minRow + height
      && square.col < minCol + width
}

/** Square on a discrete grid. It is the small unit of space in such a grid
 * 
 * @param row
 * @param col
*/
case class GridSquare(row: Int, col: Int) {
  /** Finds the adjacent square in the given direction
   * 
   * @param o orientation of the adjacent square
   */
  def adjacent(o: Orientation) =
    GridSquare(row + o.rowDiff, col + o.colDiff)

  /** Finds all the adjacent squares */
  def allAdjacent: Seq[(GridSquare, Orientation)] =
    Orientation.values.map(o => (adjacent(o), o))
}

/** Orientations

 * @param rowDiff offset in y coordinate
 * @param colDiff offset in x coordinate
*/
enum Orientation(val rowDiff: Int, val colDiff: Int):
  case North extends Orientation(-1, 0)
  case NorthEast extends Orientation(-1, 1)
  case East extends Orientation(0, 1)
  case SouthEast extends Orientation(1, 1)
  case South extends Orientation(1, 0)
  case SouthWest extends Orientation(1, -1)
  case West extends Orientation(0, -1)
  case NorthWest extends Orientation(-1, -1)

/** Color 
 * 
 * @param r red component
 * @param g green component
 * @param b blue component
 * @param a alpha component (transparency)
*/
case class Color(r: Float, g: Float, b: Float, a: Float) {
  /** Component-wise addition */
  def +(that: Color) = 
    Color(r + that.r, g + that.g, b + that.b, a + that.a)

  /** Multiplies every component by a scalar */
  def *(f: Float) =
    Color(f * r, f * g, f * b, f * a)

  /** Converts the color to a discrete 32-bits representation */
  def toIntChannels =
    ((r * 255).toInt, (g * 255).toInt, (b * 255).toInt, (a * 255).toInt)
}

object Color {
  /** Creates a color palette by interpolating between two colors
   * 
   * @param from
   * @param to 
   * @param n number of colors in the palette
   * 
   */
  def range(from: Color, to: Color, n: Int): Seq[Color] = 
    (0 until n).map {i =>
      val factor = i.toFloat / (n-1)
      Color(
        (from.r + (to.r - from.r) * factor),
        (from.g + (to.g - from.g) * factor),
        (from.b + (to.b - from.b) * factor),
        from.a + (to.a - from.a) * factor
      )
    }
}