package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import Math.min

import scala.annotation.tailrec

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    a <- arbitrary[Int]
    h <- oneOf(const(empty), genHeap)
  } yield insert(a, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("Calling findMin on a single element heap returns the element.") =
    forAll { a: Int => findMin(insert(a, empty)) == a }

  property("For any heap, inserting the minimal element results in findMin returning that element.") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("Calling findMin on a two element heap returns the minimum element.") =
    forAll { (a: Int, b: Int) => findMin(insert(a, insert(b, empty))) == min(a, b) }

  property("Inserting an element into an empty heap then calling deleteMin results in an empty heap.") =
    forAll { (a: Int) => isEmpty(deleteMin(insert(a, empty))) }

  property("Given any heap, you should get a sorted sequence of elements when continually finding and deleting minima.") =
    forAll { (h: H) =>
      val s = toLazyList(h, LazyList())
      s == s.sorted
    }

  property("Finding a minimum of the melding of any two heaps should return a minimum of one or the other.") =
    forAll { (h1: H, h2: H) => findMin(meld(h1, h2)) == min(findMin(h1), findMin(h2)) }

  property("Calling findMin after deleteMin returns second largest element") = {
    forAll { (a: Int, b: Int, c: Int) =>
      findMin(deleteMin(insert(a, insert(b, insert(c, empty))))) == List(a, b, c).sorted.drop(1).head
    }
  }

  @tailrec
  private def toLazyList(h: H, s: LazyList[A]): LazyList[A] =
    if (isEmpty(h)) s
    else toLazyList(deleteMin(h), s :+ findMin(h))
}
