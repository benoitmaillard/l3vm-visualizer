package visualizer.webapp

case class GridRectangle(minRow: Int, minCol: Int, height: Int, width: Int) {
  def contains(square: GridSquare) =
    square.row >= minRow
      && square.col >= minCol
      && square.row < minRow + height
      && square.col < minCol + width
}

case class GridSquare(row: Int, col: Int) {
  def adjacent(o: Orientation) =
    GridSquare(row + o.rowDiff, col + o.colDiff)

  def allAdjacent: Seq[(GridSquare, Orientation)] =
    Orientation.values.map(o => (adjacent(o), o))
}

enum Orientation(val rowDiff: Int, val colDiff: Int):
  case North extends Orientation(-1, 0)
  case NorthEast extends Orientation(-1, 1)
  case East extends Orientation(0, 1)
  case SouthEast extends Orientation(1, 1)
  case South extends Orientation(1, 0)
  case SouthWest extends Orientation(1, -1)
  case West extends Orientation(0, -1)
  case NorthWest extends Orientation(-1, -1)

case class Color(r: Int, g: Int, b: Int, a: Float) {
  def +(that: Color) = 
    Color(r + that.r, g + that.g, b + that.b, a + that.a)
  def *(f: Int) =
    Color(f * r, f * g, f * b, f * a)
}

object Color {
  def range(from: Color, to: Color, n: Int): Seq[Color] = 
    (0 until n).map {i =>
      val factor = i.toFloat / (n-1)
      Color(
        (from.r + (to.r - from.r) * factor).toInt,
        (from.g + (to.g - from.g) * factor).toInt,
        (from.b + (to.b - from.b) * factor).toInt,
        from.a + (to.a - from.a) * factor
      )
    }
}