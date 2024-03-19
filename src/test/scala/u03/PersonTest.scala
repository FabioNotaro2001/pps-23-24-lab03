package u03

import Optionals.Optional.*
import org.junit.*
import org.junit.Assert.*
import u03.Sequences.Sequence

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

    
