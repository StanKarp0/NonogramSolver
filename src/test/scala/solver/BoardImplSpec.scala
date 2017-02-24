package solver

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by wojciech on 23.02.17.
  */
class BoardImplSpec extends FlatSpec with Matchers {

  import BoardImplSpec._

  "init BoardImpl geters, no mod" should " " in {
    val impl = new BoardImpl(columns, rows)
    impl.column(0) should contain theSameElementsInOrderAs columns.head
    impl.column(4) should contain theSameElementsInOrderAs columns(4)
    impl.row(0) should contain theSameElementsInOrderAs rows.head
    impl.row(7) should contain theSameElementsInOrderAs rows(7)
    impl.columnFields(4) should contain theSameElementsInOrderAs List.fill(10)(Empty)
    impl.rowFields(4) should contain theSameElementsInOrderAs List.fill(9)(Empty)
    impl.columnFields(8) should contain theSameElementsInOrderAs List.fill(10)(Empty)
    impl.rowFields(9) should contain theSameElementsInOrderAs List.fill(9)(Empty)
  }

  "BoardImpl modify" should " " in {
    val impl = new BoardImpl(columns, rows)
    val col = List.fill(9)(Empty) :+ BlackAny
    val row = List.fill(8)(Empty) :+ BlackAny
    impl.setColumn(8, col)

    impl.columnFields(8) should contain theSameElementsInOrderAs col
    impl.rowFields(9) should contain theSameElementsInOrderAs row

  }

}
object BoardImplSpec {

  import BlockSpec._

  /*
                             | 5|
        | 2| 2|10| 3| 2| 1| 3| 1| 3|
       1|
       3|
       3|
    1| 1|
       6|
    3| 2|
    2| 2|
    1| 2|
    1| 1|
    1| 2|

   */
  val columns = List(
    List(2), List(2), List(10), List(3), List(2), List(1), List(3), List(5,1), List(3)
  )
  val rows = List(
    List(1), List(3), List(3), List(1,1), List(6), List(3,2), List(2,2), List(1,2), List(1,1), List(1,2)
  )

  val result = List(
    convert((2,W),(1,A),(6,W)),
    convert((3,A),(6,W)),
    convert((3,A),(6,W)),
    convert((2,W),(1,A),(4,W),(1,A),(1,W)),
    convert((2,W),(6,A),(1,W)),
    convert((2,W),(3,A),(1,W),(2,A),(1,W)),
    convert((2,W),(2,A),(2,W),(2,A),(1,W)),
    convert((2,W),(1,A),(4,W),(2,A)),
    convert((2,W),(1,A),(5,W),(1,A)),
    convert((2,W),(1,A),(4,W),(2,A))
  )


  val columns2 = List(
    List(1,2,2,1,2),
    List(2,1,1,1,5),
    List(1,1,1,2,2,1),
    List(1,1,4,1,1),
    List(6,1),
    List(4,3,2),
    List(1,1,1,3),
    List(1,1,1,1,6),
    List(3,4,2,2),
    List(3,2,4,1,1),
    List(2,4,3),
    List(2,1,1,2),
    List(2,2,1,2),
    List(4,1,1,3,1),
    List(1,2,1,4)
  )

   val rows2 = List(
     List(3,2,4),
     List(1,1,3,3),
     List(4,2,2),
     List(4,2),
     List(6,5),
     List(1,1,5,2),
     List(2,2,1,1),
     List(1,12),
     List(2,1,1,2),
     List(1,4,2),
     List(4,4,3),
     List(2,1,2),
     List(1,1,5,1),
     List(2,5,2,1),
     List(4,3,3,1)
   )

  val result2 = List(
    convert((3,A),(5,W),(2,A),(1,W),(4,A)),
    convert((1,W),(1,A),(3,W),(1,A),(1,W),(3,A),(1,W),(3,A),(1,W)),
    convert((2,W),(4,A),(2,W),(2,A),(3,W),(2,A)),
    convert((4,W),(4,A),(5,W),(2,A)),
    convert((6,A),(2,W),(5,A),(2,W)),
    convert((1,A),(3,W),(1,A),(1,W),(5,A),(1,W),(2,A),(1,W)),
    convert((1,W),(2,A),(1,W),(2,A),(2,W),(1,A),(5,W),(1,A)),
    convert((1,A),(1,W),(12,A),(1,W)),
    convert((2,A),(1,W),(1,A),(1,W),(1,A),(3,W),(2,A),(4,W)),
    convert((3,W),(1,A),(3,W),(4,A),(1,W),(2,A),(1,W)),
    convert((4,A),(3,W),(4,A),(1,W),(3,A)),
    convert((1,W),(2,A),(4,W),(1,A),(5,W),(2,A)),
    convert((1,W),(1,A),(1,W),(1,A),(2,W),(5,A),(3,W),(1,A)),
    convert((2,A),(2,W),(5,A),(1,W),(2,A),(2,W),(1,A)),
    convert((4,A),(1,W),(3,A),(1,W),(3,A),(1,W),(1,A),(1,W))
  )



}
