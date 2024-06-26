package u03

import u03.Optionals.Optional
import u03.Persons.Person

object Sequences: // Essentially, generic linkedlists
  
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

    // Lab 03
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
      
    def getCourses(l: Sequence[Person]): Sequence[String] = 
      flatMap(l)(p => p match
        case Person.Teacher(_, c) => Cons(c, Nil())
        case _ => Nil()
      )
    
    def foldLeft[A](l: Sequence[A])(v: A)(f: (x: A, y:A) => A): A = l match
      case Cons(h, t) => foldLeft(t)(f(v, h))(f)
      case _ => v
    
    
        
@main def trySequences =
  import Sequences.* 
  val l = Sequence.Cons(10, Sequence.Cons(20, Sequence.Cons(30, Sequence.Nil())))
  println(Sequence.sum(l)) // 60

  import Sequence.*

  println(sum(map(filter(l)(_ >= 20))(_ + 1))) // 21+31 = 52