package companies

import scala.util.Random


class Engine {

  abstract class Cell(val position: Position)

  case class AliveCell(override val position: Position) extends Cell(position)

  case class DeadCell(override val position: Position) extends Cell(position)

  type BoardData[T] = Array[Array[T]]

  case class Position(x: Int, y: Int)

  def intToCell(value: Int, position: Position): Cell  = value match {
    case 1 => new AliveCell(position)
    case _ => new DeadCell(position)
  }

  def cellToInt(cell: Cell): Int = cell match {
    case cell: AliveCell => 1
    case _ => 0
  }

  case class Board(data: BoardData[Cell]) {
    def cell(p: Position): Option[Cell] =
      data.lift(p.y).flatMap(_.lift(p.x))

    def neighbors(position: Position): Array[Option[Cell]] =
      Array((-1, -1), (0, -1), (1, -1), (-1, 0), (1, 0), (-1, 1), (0, 1), (1, 1))
        .map(p => new Position(position.x + p._1, position.y + p._2))
        .map(cell)

    def toArray: BoardData[Int] = data.map(_.map(cellToInt))

    def map(fn: Cell => Cell): Board =
      new Board(data.map(_.map(fn)))
  }

  def createBoard(width: Int, height: Int): Board = {
    val cellData: Array[Array[Cell]] = Array.ofDim(height, width)
    for (x <- 0 until width; y <- 0 until height) {
      cellData(y)(x) = intToCell(Random.nextInt(2), new Position(x, y))
    }
    new Board(cellData)
  }

  def applyRules(amount: Int, cell: Cell): Cell = {
    val kill = (cell: Cell) => new DeadCell(cell.position)
    val resurrect = (cell: Cell) => new AliveCell(cell.position)
    cell match {
      case cell: AliveCell =>
        if (amount < 2) kill(cell) //live cell with fewer than two live neighbours dies
        else if (amount == 2 || amount == 3) resurrect(cell) //live cell with two or three live neighbours lives
        else if (amount > 3) kill(cell) //live cell with more than three live neighbours dies
        else cell
      case _ =>
        if (amount == 3) resurrect(cell) //dead cell with exactly three live neighbours becomes a live cell
        else cell
    }
  }

  def isAlive(cell: Option[Cell]): Boolean = cell match {
    case Some(cell: AliveCell) => true
    case _ => false
  }

  def sumOfNeighbors(board: Board) = (cell: Cell) =>
    board
      .neighbors(cell.position)
      .filter(isAlive)
      .length

  def tick(board: Board): Board = {
    val aliveNeighborsNum = sumOfNeighbors(board)
    board.map(
      cell => applyRules(aliveNeighborsNum(cell), cell)
    )
  }
}
