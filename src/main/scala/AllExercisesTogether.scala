// Tutti gli esercizi sono stati svolti da solo.

// Task 1.
object Sequences: // Essentially, generic linkedlists
    enum Sequence[E]:
        case Cons(head: E, tail: Sequence[E])
        case Nil()

    object Sequence:
        def sum(l: Sequence[Int]): Int = l match
            case Cons(h, t) => h + sum(t)
            case _          => 0

        def map[A, B](l: Sequence[A])(mapper: A => B): Sequence[B] = l match
            case Cons(h, t) => Cons(mapper(h), map(t)(mapper))
            case Nil()      => Nil()

        def filter[A](l1: Sequence[A])(pred: A => Boolean): Sequence[A] = l1 match
            case Cons(h, t) if pred(h) => Cons(h, filter(t)(pred))
            case Cons(_, t)            => filter(t)(pred)
            case Nil()                 => Nil()

        def zip[A, B](first: Sequence[A], second: Sequence[B]): Sequence[(A, B)] = (first, second) match
            case(Cons(h1, t1), Cons(h2, t2)) => Cons((h1, h2), zip(t1, t2))
            case(_, Nil()) | (Nil(), _) => Nil()
        
        def take[A](l: Sequence[A])(n: Int): Sequence[A] = l match
            case Cons(h, t) if n > 0 => Cons(h, take(t)(n - 1))
            case Cons(_, _) if n == 0 => Nil()
            case Nil() => Nil() 
        
        def concat[A](l1: Sequence[A], l2: Sequence[A]): Sequence[A] = (l1, l2) match
            case (Cons(h1, t1), _) => Cons(h1, concat(t1, l2))
            case (Nil(), Cons(h2, t2)) => Cons(h2, concat(t2, Nil()))
            case(_, Nil()) => Nil()
        
        def flatMap[A, B](l: Sequence[A])(mapper: A => Sequence[B]): Sequence[B] = l match
            case Cons(h, t) => concat(mapper(h), flatMap(t)(mapper))
            case Nil() => Nil()

        def mapImplementedByFlatMap[A, B](l: Sequence[A])(mapper: A => B): Sequence[B] = flatMap(l)(a => Cons(mapper(a), Nil()))
    
        def filterImplementedByFlatMap[A](l1: Sequence[A])(pred: A => Boolean): Sequence[A] = 
            val mapper = (v: A) => v match
                case v if pred(v) => Cons(v, Nil())
                case _ => Nil()
            flatMap(l1)(mapper)
        
        def min(l: Sequence[Int]): Optional[Int] = l match
            case Cons(h, t) if h < orElse(min(t), Int.MaxValue) => Optional.Just(h)
            case Cons(h, t) if h > orElse(min(t), Int.MaxValue) => min(t)
            case Nil() => Optional.Empty()

// Task 2.
enum Person:
    case Student(name: String, year: Int)
    case Teacher(name: String, course: String)

object Person:
    def isStudent(p: Person): Boolean = p match
        case Student(_, _) => true
        case _ => false

    def getCourse(p: Person): String = p match
        case Teacher(_, c) => c
        case _ => ""
    
    def getAllCourses(people: Sequence[Person]): Sequence[String] = Sequence.map(Sequence.filter(people)(!isStudent(_)))(getCourse(_))

object Sequences: // Essentially, generic linkedlists
    enum Sequence[E]:
        case Cons(head: E, tail: Sequence[E])
        case Nil() 
    object Sequence:
        def foldLeft[A, B](list: Sequence[A])(default: B)(f: (B, A) => B): B = list match
            case Nil() => default
            case Cons(h, t) => foldLeft(t)(f(default, h))(f)
        extension (l: Sequence[Int]) def sumAsExtensionMethod: Int = l match
            case Cons(h, t) => h + t.sumAsExtensionMethod
            case _          => 0

        extension [A, B](l: Sequence[A]) def mapAsExtensionMethod(mapper: A => B): Sequence[B] = l match
            case Cons(h, t) => Cons(mapper(h), t.mapAsExtensionMethod(mapper))
            case Nil()      => Nil()

        extension [A](l1: Sequence[A]) def filterAsExtensionMethod(pred: A => Boolean): Sequence[A] = l1 match
            case Cons(h, t) if pred(h) => Cons(h, t.filterAsExtensionMethod(pred))
            case Cons(_, t)            => t.filterAsExtensionMethod(pred)
            case Nil()                 => Nil()

        extension [A, B](first: Sequence[A]) def zipAsExtensionMethod(second: Sequence[B]): Sequence[(A, B)] = (first, second) match
            case(Cons(h1, t1), Cons(h2, t2)) => Cons((h1, h2), t1.zipAsExtensionMethod(t2))
            case(_, Nil()) | (Nil(), _) => Nil()
        
        extension [A](l: Sequence[A]) def takeAsExtensionMethod(n: Int): Sequence[A] = l match
            case Cons(h, t) if n > 0 => Cons(h, t.takeAsExtensionMethod(n - 1))
            case Cons(_, _) if n == 0 => Nil()
            case Nil() => Nil() 
        
        extension [A](l1: Sequence[A]) def concatAsExtensionMethod(l2: Sequence[A]): Sequence[A] = (l1, l2) match
            case (Cons(h1, t1), _) => Cons(h1, t1.concatAsExtensionMethod(l2))
            case (Nil(), Cons(h2, t2)) => Cons(h2, t2.concatAsExtensionMethod(Nil()))
            case(_, Nil()) => Nil()
        
        extension [A, B](l: Sequence[A]) def flatMapAsExtensionMethod(mapper: A => Sequence[B]): Sequence[B] = l match
            case Cons(h, t) => concat(mapper(h), t.flatMapAsExtensionMethod(mapper))
            case Nil() => Nil()

        extension [A, B](l: Sequence[A]) def mapImplementedByFlatMapAsExtensionMethod(mapper: A => B): Sequence[B] = l.flatMapAsExtensionMethod(a => Cons(mapper(a), Nil()))
    
        extension [A](l1: Sequence[A]) def filterImplementedByFlatMapAsExtensionMethod(pred: A => Boolean): Sequence[A] = 
            val mapper = (v: A) => v match
                case v if pred(v) => Cons(v, Nil())
                case _ => Nil()
            l1.flatMapAsExtensionMethod(mapper)
        
        extension (l: Sequence[Int]) def minAsExtensionMethod: Optional[Int] = l match
            case Cons(h, t) if h < orElse(t.minAsExtensionMethod, Int.MaxValue) => Optional.Just(h)
            case Cons(h, t) if h > orElse(t.minAsExtensionMethod, Int.MaxValue) => t.minAsExtensionMethod
            case Nil() => Optional.Empty()

// Task 3.
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

    def takeWhile[A](stream: Stream[A])(pred: A => Boolean): Stream[A] = stream match
      case Cons(head, tail) if pred(head()) => cons(head(), takeWhile(tail())(pred))
      case _ => Empty()

    def fill[A](n: Int)(k: A): Stream[A] = n match
      case base if n == 0 => Empty()
      case recursive if n > 0  => Cons(() => k, () => fill(n - 1)(k))

    def mapperForPellNumbers(x: Int): Int = x match
      case 0 => 0
      case 1 => 1
      case _ => 2 * mapperForPellNumbers(x - 1) + mapperForPellNumbers(x - 2)
    
    def pellNumbers: Stream[Int] = map(iterate(0)(_ + 1))(mapperForPellNumbers(_))

  end Stream