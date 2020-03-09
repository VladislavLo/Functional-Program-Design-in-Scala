package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    i <- arbitrary[Int]
    h <- oneOf[H](empty, genHeap)
  } yield insert(i, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  def checkListOrder(arr: List[Int]): Boolean = arr match {
    case Nil => true
    case h :: t => {
      if (t.forall(e => h <= e)) {
        checkListOrder(t)
      }
      else false
    }
  }

  def heapListMin(heap: H): List[Int] = if (!isEmpty(heap)) {
    findMin(heap) :: heapListMin(deleteMin(heap))
  } else {
    Nil
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("find min") = forAll { (a: Int, b: Int) => {
    val h = insert(a, empty)
    val h2 = insert(b, h)
    findMin(h2) == (if (a < b) a else b)
  }
  }

  property("ins and rm") = forAll { (a: Int) => {
    val h = insert(a, empty)
    deleteMin(h) == empty
  }
  }

  property("heap order") = forAll { (h: H) => {
    checkListOrder(heapListMin(h)) == true
  }
  }

  property("meld and min") = forAll { (h1: H, h2: H) => {
    val min1 = findMin(h1)
    val min2 = findMin(h2)
    val min = if (min1 < min2) min1 else min2
    findMin(meld(h1, h2)) == min
  }
  }

  property("link error") = forAll { (h1: H, h2: H) => {
    val m = meld(h1, h2)
    val meldMin = heapListMin(m)
    val min = (heapListMin(h1) ::: heapListMin(h2)).sorted
    meldMin == min
  }
  }
}
