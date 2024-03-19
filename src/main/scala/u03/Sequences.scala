package u03

import u02.AnonymousFunctions.l
import u03.Optionals.Optional
import u03.Optionals.Optional.orElse

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

        // Lab 03
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

        def foldLeft[A, B](list: Sequence[A])(default: B)(f: (B, A) => B): B = list match
            case Nil() => default
            case Cons(h, t) => foldLeft(t)(f(default, h))(f)

        // All the aboveFunctions as extension methods for the sequence type.
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
        

@main def trySequences =
    import Sequences.* 
    val l = Sequence.Cons(10, Sequence.Cons(20, Sequence.Cons(30, Sequence.Nil())))
    println(Sequence.sum(l)) // 30

    import Sequence.*

    println(sum(map(filter(l)(_ >= 20))(_ + 1))) // 21+31 = 52
