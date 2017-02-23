package solver

/**
  * Created by wojciech on 23.02.17.
  */
trait Board {

  def column(x: Int): List[Int]
  def row(y: Int): List[Int]

  def columnFields(x: Int): List[Field]
  def rowFields(y: Int): List[Field]

  def setColumn(x: Int, arg: List[Field]): Unit
  def setRow(y: Int, arg: List[Field]): Unit

  def width: Int
  def height: Int

}
