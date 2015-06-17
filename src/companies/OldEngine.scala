package companies

import scala.collection.mutable.ArrayBuffer
import scala.util.Random


class OldEngine {

  type Cell = Int
  type Board = Array[Array[Cell]]

  def applyRules(amount: Int, currentState: Int): Int = {
    if (currentState == 1) {
      if (amount < 2) 0 //live cell with fewer than two live neighbours dies
      else if (amount == 2 || amount == 3) 1 //live cell with two or three live neighbours lives
      else if (amount > 3) 0 //live cell with more than three live neighbours dies
      else 1
    }
    else {
      if (amount == 3) 1 //dead cell with exactly three live neighbours becomes a live cell
      else 0
    }
  }

  def createBoard(width: Int, height: Int): Board = {
    ArrayBuffer.fill(width, height)(Random.nextInt(2)).map(_.toArray).toArray
  }

  def tick(board: Board): Board = {
    board.map(
      row => {
        var colNum = 0
        row.map(
          point => {
            val rowNum = board.indexOf(row)
            val sumOfNeighbors = getSumOfNeighbors(board, rowNum, colNum) - point
            colNum = colNum + 1
            applyRules(sumOfNeighbors, point)
          }
        )
      })
  }

  def getNeighborsCols(board: Board, colNum: Int): Board = {
    if (colNum < 0 || colNum >= board.length) return Array.empty
    if (colNum == 0) return board.map(_.take(colNum + 2))
    if (colNum == board.length - 1) return board.map(_.drop(colNum - 1))
    board.map(_.take(colNum + 2)).map(_.drop(colNum - 1))
  }

  def getSumOfNeighbors(board: Board, row: Int, col: Int): Int = {
    val cols = getNeighborsCols(board, col)
    val rows = getNeighborsRows(cols, row)
    rows.flatten.sum
  }

  def getNeighborsRows(board: Board, rowNum: Int): Board = {
    if (rowNum < -1 || board.isEmpty) Array.empty
    else if (rowNum == 1 || rowNum == 0 || rowNum == -1) getNeighborsRows(board.tail, rowNum - 1) :+ board.head
    else getNeighborsRows(board.tail, rowNum - 1)
  }
}
