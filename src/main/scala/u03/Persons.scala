package u03

object Persons:
    enum Person:
        case Student(name: String, year: Int)
        case Teacher(name: String, course: String)
