package u03

import u03.AdditionalSequencesAlgorithms.filter
import u03.Sequences.Sequence

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
    
