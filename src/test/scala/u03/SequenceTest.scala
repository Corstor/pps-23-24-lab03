package u03

import Optionals.Optional.*
import org.junit.*
import org.junit.Assert.*

class SequenceTest:
  import u03.Sequences.*
  import Sequence.*

  val l: Sequence[Int] = Cons(10, Cons(20, Cons(30, Nil())))

  @Test def testSum() =
    assertEquals(0, sum(Nil()))
    assertEquals(60, sum(l))

  @Test def testMap() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), map(l)(_ + 1))
    assertEquals(Cons("10", Cons("20", Cons("30", Nil()))), map(l)(_ + ""))

  @Test def testFilter() =
    assertEquals(Cons(20, Cons(30, Nil())), filter(l)(_ >= 20))
    assertEquals(Cons(10, Cons(30, Nil())), filter(l)(_ != 20))
  
  @Test def testTake() =
    assertEquals(Cons(10, Cons(20, Nil())), take(l)(2))
    assertEquals(Cons(10, Cons(20, Cons(30, Nil()))), take(l)(3))
    assertEquals(Nil(), take(l)(0))
    assertEquals(Nil(), take(Nil())(2))
  
  @Test def testZip() = 
    val l2: Sequence[String] = Cons("10", Cons("20", Cons("30", Nil())))
    val zipped = Cons((10, "10"), Cons((20, "20"), Cons((30, "30"), Nil())))
    assertEquals(zipped, zip(l, l2))
    assertEquals(Nil(), zip(l, Nil()))
    assertEquals(map(zipped)((x, y) => (y, x)), zip(l2, l))

  @Test def testConcat() =
    val l2: Sequence[Int] = Cons(40, Cons(50, Nil()))
    val concatted = Cons(10, Cons(20, Cons(30, Cons(40, Cons(50, Nil())))))
    assertEquals(concatted, concat(l, l2))
    assertEquals(l, concat(Nil(), l))
    assertEquals(Nil(), concat(Nil(), Nil()))
    assertEquals(l, concat(l, Nil()))
    
  @Test def testFlatMap(): Unit = ???
    

  @Test def testMin(): Unit = ???
    
