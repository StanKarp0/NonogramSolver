package solver

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by wojciech on 22.02.17.
  */
class SimpleDiffSpec extends FlatSpec with Matchers {

  "Simple Diff (3) size = 7" should " 7E " in {
    SimpleDiff(List(3),7) should contain theSameElementsInOrderAs
      List.fill(7)(Empty)
  }

  "Simple Diff (4, 4) size = 12" should " 3E B 4E B 3E " in {
    SimpleDiff(List(4,4),12) should contain theSameElementsInOrderAs
      List.fill(3)(Empty) ::: List(Black(0)) ::: List.fill(4)(Empty) ::: List(Black(1)) ::: List.fill(3)(Empty)
  }

  "Simple Diff (11) size = 12" should " E 10B E " in {
    SimpleDiff(List(11),12) should contain theSameElementsInOrderAs
      List(Empty) ::: List.fill(10)(Black(0)) ::: List(Empty)
  }

  "Simple Diff (1,3,1) size = 12" should " 12E " in {
    SimpleDiff(List(1,3,1),12) should contain theSameElementsInOrderAs
      List.fill(12)(Empty)
  }

  "Simple Diff (1,1,3,1,1) size = 12" should " 5E 2B 5E " in {
    SimpleDiff(List(1,1,3,1,1),12) should contain theSameElementsInOrderAs
      List.fill(5)(Empty) ::: List.fill(2)(Black(2)) ::: List.fill(5)(Empty)
  }

  "Simple Diff (4,1,5) size = 12" should " 4B W 1B W 5B " in {
    SimpleDiff(List(4,1,5),12) should contain theSameElementsInOrderAs
      List.fill(4)(Black(0)) ::: List(White,Black(1),White) ::: List.fill(5)(Black(2))
  }

  "Simple Diff (4,6) size = 12" should " E 3B 2E 5B E " in {
    SimpleDiff(List(4,6),12) should contain theSameElementsInOrderAs
      List(Empty) ::: List.fill(3)(Black(0)) ::: List(Empty,Empty) ::: List.fill(5)(Black(1)) ::: List(Empty)
  }

  "Simple Diff (1, 2, 6) size = 12" should " 3E B 2E 5B E " in {
    SimpleDiff(List(1,2,6),12) should contain theSameElementsInOrderAs
      List.fill(3)(Empty) ::: List(Black(1)) ::: List.fill(2)(Empty) ::: List.fill(5)(Black(2)) ::: List(Empty)
  }

}
