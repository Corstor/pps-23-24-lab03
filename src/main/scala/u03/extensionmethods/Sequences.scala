package u03.extensionmethods

object Sequences:
  
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

    extension [A](l: Sequence[A])

      def map[B](mapper: A => B): Sequence[B] = 
      flatMap(v => Cons(mapper(v), Nil()))

      def filter(pred: A => Boolean): Sequence[A] =
        flatMap(v => pred(v) match
          case true => Cons(v, Nil())
          case _ => Nil())

      // Lab 03
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
        
      def getCourses(): Sequence[String] = 
        flatMap(p => p match
          case Person.Teacher(_, c) => Cons(c, Nil())
          case _ => Nil()
        )
      
      def foldLeft(v: A)(f: (x: A, y:A) => A): A = l match
        case Cons(h, t) => t.foldLeft(f(v, h))(f)
        case _ => v

    def of[A](n: Int, a: A): Sequence[A] =
      if (n == 0) then Nil[A]() else Cons(a, of(n - 1, a))

@main def trySequences() =
  import Sequences.*
  import Sequence.*
  
  val seq = Cons(10, Cons(20, Cons(30, Nil())))
  println(seq.filter(_ >= 20).map(_ + 1).sum) // 21+31 = 52
  println(sum(map(filter(seq)(_ >= 20))(_ + 1))) // equally possible
  val seq2 = of(10, -1) // Cons(-1, Cons(-1, Cons(-1, ...)))
  println(seq2.sum) // -10
  println(seq.min) //10
  println(seq.foldLeft(0)(_+_)) //60