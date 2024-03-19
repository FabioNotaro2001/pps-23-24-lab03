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
        assertEquals(Cons(10, Cons(20, Cons(30, Nil()))), take(l)(5))   // Added test.
        assertEquals(Nil(), take(l)(0))
        assertEquals(Nil(), take(Nil())(2))
    
    @Test def testZip() = 
        val l2: Sequence[String] = Cons("10", Cons("20", Cons("30", Nil())))
        assertEquals(Cons((10, "10"), Cons((20, "20"), Cons((30, "30"), Nil()))), zip(l, l2))
        assertEquals(Nil(), zip(l, Nil()))
        assertEquals(Nil(), zip(Nil(), l2))
        assertEquals(Nil(), zip(Nil(), Nil()))

    @Test def testConcat() =
        val l2: Sequence[Int] = Cons(40, Cons(50, Nil()))
        assertEquals(Cons(10, Cons(20, Cons(30, Cons(40, Cons(50, Nil()))))), concat(l, l2))
        assertEquals(Cons(40, Cons(50, Nil())), concat(Nil(), l2))

    @Test def testFlatMap() =
        assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), flatMap(l)(v => Cons(v + 1, Nil())))
        assertEquals(Nil(), flatMap(Nil())(v => Cons(v, Nil())))

    @Test def testMapImplementedByFlatMap() =
       assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), mapImplementedByFlatMap(l)(_ + 1))
       assertEquals(Cons("10", Cons("20", Cons("30", Nil()))), mapImplementedByFlatMap(l)(_ + ""))

    @Test def testFilterImplementedByFlatMap() =
       assertEquals(Cons(20, Cons(30, Nil())), filterImplementedByFlatMap(l)(_ >= 20))
       assertEquals(Cons(10, Cons(30, Nil())), filterImplementedByFlatMap(l)(_ != 20))

    @Test def testMin() =
        assertEquals(Just(10), min(l))
        assertEquals(Just(1), min(Cons(1, Nil())))
        assertEquals(Empty(), min(Nil()))
        assertEquals(Just(10), min(Cons(100, Cons(10, Cons(50, Nil())))))

    @Test def testFoldLeft() =
        assertEquals(-16, foldLeft(Cons (3 , Cons (7 , Cons (1 , Cons (5 , Nil () ) ) ) ))(0)(_-_))
        assertEquals("Ciao mondo", foldLeft(Cons("Ciao", Cons(" mondo", Nil())))("")(_+_))

    @Test def testExtensionMethod() =
        assertEquals(60, l.sumAsExtensionMethod)
        assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), l.mapAsExtensionMethod(_ + 1))
        assertEquals(Cons(20, Cons(30, Nil())), l.filterAsExtensionMethod(_ >= 20))
        assertEquals(Cons(10, Cons(20, Nil())), l.takeAsExtensionMethod(2))
        assertEquals(Cons((10, "10"), Cons((20, "20"), Cons((30, "30"), Nil()))), l.zipAsExtensionMethod(Cons("10", Cons("20", Cons("30", Nil())))))
        assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), l.flatMapAsExtensionMethod(v => Cons(v + 1, Nil())))
        assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), l.mapImplementedByFlatMapAsExtensionMethod(_ + 1))
        assertEquals(Cons(20, Cons(30, Nil())), l.filterImplementedByFlatMapAsExtensionMethod(_ >= 20))
        assertEquals(Just(10), l.minAsExtensionMethod)