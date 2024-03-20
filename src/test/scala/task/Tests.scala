package task

import org.junit.*
import org.junit.Assert.*

class Tests:
    //Task 1, 2, svolti da solo
    import task.Sequences.*
    import Sequence.*

    val l: Sequence[Int] = Cons(10, Cons(20, Cons(30, Nil())))

    @Test def testSum() =
        assertEquals(0, sum(Nil()))
        assertEquals(60, sum(l))

    @Test def testMap() =
        assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), map(l)(_ + 1))
        assertEquals(Cons("10", Cons("20", Cons("30", Nil()))), map(l)(_ + ""))
        assertEquals(Nil(), map(Nil[Int]())(_ + 1))

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
        
    @Test def testFlatMap() = 
        assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), flatMap(l)(x => Cons(x + 1, Nil())))
        assertEquals(Cons(11, Cons(12, Cons(21, Cons(22, Cons(31, Cons(32, Nil())))))), flatMap(l)(x => Cons(x + 1, Cons(x+2, Nil()))))
        assertEquals(Nil(), flatMap(l)(v => Nil()))
        assertEquals(l, flatMap(l)(v => Cons(v, Nil())))
        assertEquals(Nil(), flatMap(Nil())(v => Nil()))
        assertEquals(Nil(), flatMap(Nil())(v => Cons(v, Nil())))

    @Test def testMin() = 
        import u03.Optionals.*
        import Optional.*
        assertEquals(Just(10), min(l))
        assertEquals(Empty(), min(Nil()))
        
    @Test def testGetCourses() =
        import task.Persons.* 
        import Person.*
        val persons = Cons(Teacher("Andrea", "pcd"), Cons(Student("Corrado", 2023), Cons(Teacher("Mirko", "pps"), Cons(Teacher("Prova", "pps"), Nil()))))
        assertEquals(Cons("pcd", Cons("pps", Cons("pps", Nil()))), getCourses(persons))

    @Test def testFoldLeft() =
        assertEquals(-60, foldLeft(l)(0)(_ - _))
        assertEquals(0, foldLeft(Nil())(0)(_+_))

    //Task 3, svolto da solo
    import task.Streams.*
    import Stream.*

    @Test def testIterate(): Unit = 
        val str1 = Stream.iterate(0)(_ + 1) // {0,1,2,3,..}
        assertEquals(Cons(0, Cons(1, Cons(2, Cons(3, Nil())))), toList(Stream.take(str1)(4)))

    @Test def testStreamMap(): Unit =
        val str1 = Stream.iterate(0)(_ + 1) // {0,1,2,3,..}
        val str2 = Stream.map(str1)(_ + 1) // {1,2,3,4,..}
        assertEquals(Cons(1, Cons(2, Cons(3, Cons(4, Nil())))), toList(Stream.take(str2)(4)))

    @Test def testStreamFilter(): Unit =
        val str1 = Stream.iterate(0)(_ + 1) // {0,1,2,3,..}
        val str2 = Stream.filter(str1)(x => x % 2 == 1) // {1,3,5,7,..}
        assertEquals(Cons(1, Cons(3, Cons(5, Cons(7, Nil())))), toList(Stream.take(str2)(4)))

    @Test def takeWhile(): Unit = 
        val str1 = Stream.iterate(0)(_ + 1) // {0,1,2,3,..}
        val str2 = Stream.takeWhile(str1)(_ < 5) // {0,1,2,3,4}
        assertEquals(Cons(0, Cons(1, Cons(2, Cons(3, Cons(4, Nil()))))), Stream.toList(str2))
        
    @Test def testFill(): Unit = 
        assertEquals(Cons("a", Cons("a", Cons("a" , Nil()))), Stream.toList(Stream.fill(3)("a")))
        assertEquals(Nil(), Stream.toList(Stream.fill(0)(10)))
        assertEquals(Cons(5, Nil()), Stream.toList(Stream.fill(1)(5)))

//Task 2 - extended method for Sequences, svolto da solo
@main def trySequences() =
  import task.SequencesExtended.*
  import SequenceExtended.*
  
  val seq = Cons(10, Cons(20, Cons(30, Nil())))
  println(seq.filter(_ >= 20).map(_ + 1).sum) // 21+31 = 52
  println(sum(map(filter(seq)(_ >= 20))(_ + 1))) // equally possible
  val seq2 = of(10, -1) // Cons(-1, Cons(-1, Cons(-1, ...)))
  println(seq2.sum) // -10
  println(seq.min) //10
  println(seq.foldLeft(0)(_+_)) //60