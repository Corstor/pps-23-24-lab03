package task

import u03.Optionals.Optional
import u03.Persons.Person

//Task 1, svolto da solo

object Sequences:
  
  enum Sequence[E]:
    case Cons(head: E, tail: Sequence[E])
    case Nil()

  object Sequence:

    def sum(l: Sequence[Int]): Int = l match
      case Cons(h, t) => h + sum(t)
      case _          => 0

    def map[A, B](l: Sequence[A])(mapper: A => B): Sequence[B] = 
      flatMap(l)(v => Cons(mapper(v), Nil()))

    def filter[A](l1: Sequence[A])(pred: A => Boolean): Sequence[A] =
      flatMap(l1)(v => pred(v) match
        case true => Cons(v, Nil())
        case _ => Nil())

    def zip[A, B](first: Sequence[A], second: Sequence[B]): Sequence[(A, B)] = (first, second) match
      case (Cons(h, t), Cons(h2, t2)) => Cons((h, h2), zip(t, t2))
      case _ => Nil()
    

    def take[A](l: Sequence[A])(n: Int): Sequence[A] = l match
      case Cons(h, t) if n > 0 =>  Cons(h, take(t)(n-1))
      case _ => Nil()
    
    def concat[A](l1: Sequence[A], l2: Sequence[A]): Sequence[A] = l1 match
      case Cons(h, t) => Cons(h, concat(t, l2))
      case _ => l2

    def flatMap[A, B](l: Sequence[A])(mapper: A => Sequence[B]): Sequence[B] = l match
      case Cons(h, t) => concat(mapper(h), flatMap(t)(mapper))
      case _ => Nil()

    def min(l: Sequence[Int]): Optional[Int] = 
      @annotation.tailrec
      def minTail(l: Sequence[Int])(min: Int): Int =
        l match
          case Cons(h, t) if h < min => minTail(t)(h)
          case Cons(_, t) => minTail(t)(min)
          case _ => min
      l match
        case Cons(h, t) => Optional.Just(minTail(l)(h))
        case _ => Optional.Empty()
      
    //Task 2, svolto da solo
    def getCourses(l: Sequence[Person]): Sequence[String] = 
      flatMap(l)(p => p match
        case Person.Teacher(_, c) => Cons(c, Nil())
        case _ => Nil()
      )
    
    def foldLeft[A](l: Sequence[A])(v: A)(f: (x: A, y:A) => A): A = l match
      case Cons(h, t) => foldLeft(t)(f(v, h))(f)
      case _ => v


//Task 2 extended method for sequences, svolto da solo
object SequencesExtended:
  
  enum Sequence[E]:
    case Cons(head: E, tail: Sequence[E])
    case Nil()

  object Sequence:
    import u03.Optionals.*
    import u03.Persons.*

    extension (l: Sequence[Int])
      def sum: Int = l match
        case Cons(h, t) => h + t.sum
        case _          => 0

      def min: Optional[Int] = 
        @annotation.tailrec
        def minTail(l: Sequence[Int])(min: Int): Int =
          l match
            case Cons(h, t) if h < min => minTail(t)(h)
            case Cons(_, t) => minTail(t)(min)
            case _ => min
        l match
          case Cons(h, t) => Optional.Just(minTail(l)(h))
          case _ => Optional.Empty()
    
    extension (l: Sequence[Person])
      def getCourses(): Sequence[String] = 
        l.flatMap(p => p match
          case Person.Teacher(_, c) => Cons(c, Nil())
          case _ => Nil()
        )

    extension [A](l: Sequence[A])

      def map[B](mapper: A => B): Sequence[B] = 
        flatMap(v => Cons(mapper(v), Nil()))

      def filter(pred: A => Boolean): Sequence[A] =
        flatMap(v => pred(v) match
          case true => Cons(v, Nil())
          case _ => Nil())

      def zip[B](second: Sequence[B]): Sequence[(A, B)] = (l, second) match
        case (Cons(h, t), Cons(h2, t2)) => Cons((h, h2), t.zip(t2))
        case _ => Nil()
      

      def take(n: Int): Sequence[A] = l match
        case Cons(h, t) if n > 0 =>  Cons(h, t.take(n-1))
        case _ => Nil()
      
      def concat(l2: Sequence[A]): Sequence[A] = l match
        case Cons(h, t) => Cons(h, t.concat(l2))
        case _ => l2

      def flatMap[B](mapper: A => Sequence[B]): Sequence[B] = l match
        case Cons(h, t) => mapper(h).concat(t.flatMap(mapper))
        case _ => Nil()
      
      def foldLeft(v: A)(f: (x: A, y:A) => A): A = l match
        case Cons(h, t) => t.foldLeft(f(v, h))(f)
        case _ => v

    def of[A](n: Int, a: A): Sequence[A] =
      if (n == 0) then Nil[A]() else Cons(a, of(n - 1, a))



object Streams :

  import Sequences.*

  enum Stream[A]:
    private case Empty()
    private case Cons(head: () => A, tail: () => Stream[A])

  object Stream:

    def empty[A](): Stream[A] = Empty()

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)

    def toList[A](stream: Stream[A]): Sequence[A] = stream match
      case Cons(h, t) => Sequence.Cons(h(), toList(t()))
      case _ => Sequence.Nil()

    def map[A, B](stream: Stream[A])(f: A => B): Stream[B] = stream match
      case Cons(head, tail) => cons(f(head()), map(tail())(f))
      case _ => Empty()

    def filter[A](stream: Stream[A])(pred: A => Boolean): Stream[A] = stream match
      case Cons(head, tail) if (pred(head())) => cons(head(), filter(tail())(pred))
      case Cons(head, tail) => filter(tail())(pred)
      case _ => Empty()

    def take[A](stream: Stream[A])(n: Int): Stream[A] = (stream, n) match
      case (Cons(head, tail), n) if n > 0 => cons(head(), take(tail())(n - 1))
      case _ => Empty()

    def iterate[A](init: => A)(next: A => A): Stream[A] =
      cons(init, iterate(next(init))(next))

    // Task 3, svolto da solo

    def takeWhile[A](stream: Stream[A])(pred: A => Boolean): Stream[A] = stream match
      case Cons(head, tail) if pred(head()) => cons(head(), takeWhile(tail())(pred))
      case _ => Empty()

    def fill[A](n: Int)(elem: A): Stream[A] = 
      take(iterate(elem)(x => x))(n)

    def getPellStream(): Stream[Int] =
      def _getPellStream(v1: Int)(v2:Int): Stream[Int] = v1 match
        case 0 => cons(v1, cons(v2, _getPellStream(v2)(v1)))
        case _ => 
          val newPellNumber = 2 * v1 + v2
          cons(newPellNumber, _getPellStream(newPellNumber)(v1))

      _getPellStream(0)(1)