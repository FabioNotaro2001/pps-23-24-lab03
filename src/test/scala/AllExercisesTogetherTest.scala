// Task 1.
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

// Task 2.
class PersonTest:
    import Person.*
    import Sequence.*
    

    @Test def testIsStudent() =
        assertTrue(isStudent(Student("Fabio Notaro", 2001)))
        assertFalse(isStudent(Teacher("MirkoViroli", "PPS")))

    @Test def testgetCourse() =
        assertEquals("PPS", getCourse(Teacher("MirkoViroli", "PPS")))

    @Test def testGetAllCourses() =
        val people: Sequence[Person] = Cons(Student("Fabio Notaro", 2001), Cons(Teacher("Mirko Viroli", "PPS"), Cons(Teacher("Gianluca Aguzzi", "PPS"), Nil())))
        assertEquals(Cons("PPS", Cons("PPS", Nil())), getAllCourses(people))

class SequenceTest:
    import u03.Sequences.*    
    import Sequence.*

    val l: Sequence[Int] = Cons(10, Cons(20, Cons(30, Nil())))
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

// Task 3.
class StreamTest:
  @Test def testIterate(): Unit = 
    val str1 = Stream.iterate(0)(_ + 1) // {0,1,2,3,..}
    assertEquals(Cons(0, Cons(1, Cons(2, Cons(3, Nil())))), toList(Stream.take(str1)(4)))

  @Test def testMap(): Unit =
    val str1 = Stream.iterate(0)(_ + 1) // {0,1,2,3,..}
    val str2 = Stream.map(str1)(_ + 1) // {1,2,3,4,..}
    assertEquals(Cons(1, Cons(2, Cons(3, Cons(4, Nil())))), toList(Stream.take(str2)(4)))

  @Test def testFilter(): Unit =
    val str1 = Stream.iterate(0)(_ + 1) // {0,1,2,3,..}
    val str2 = Stream.filter(str1)(x => x % 2 == 1) // {1,3,5,7,..}
    assertEquals(Cons(1, Cons(3, Cons(5, Cons(7, Nil())))), toList(Stream.take(str2)(4)))

  @Test def takeWhile(): Unit = 
    val str1 = Stream.iterate(0)(_ + 1) // {0,1,2,3,..}
    val str2 = Stream.takeWhile(str1)(_ < 5) // {0,1,2,3,4}
    assertEquals(Cons(0, Cons(1, Cons(2, Cons(3, Cons(4, Nil()))))), Stream.toList(str2))

  @Test def fill(): Unit = 
    val s = Stream.fill(3)("a")
    assertEquals(Cons("a", Cons("a", Cons("a", Nil()))), Stream.toList(s))

  @Test def pellNumbers(): Unit = 
    val s = Stream.pellNumbers
    assertEquals(Cons(0, Cons(1, Cons(2, Cons(5, Cons(12, Nil()))))), Stream.toList(Stream.take(s)(5)))
