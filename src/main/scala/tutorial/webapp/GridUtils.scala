package tutorial.webapp

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
  case East extends Orientation(0, 1)
  case South extends Orientation(1, 0)
  case West extends Orientation(0, -1)
