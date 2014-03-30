import scala.util.Try

object Utils {

  def nameCheck[A](name: String)(check: String => Try[A]): Try[A] = {
    val names = name.split(" ")
    val middleNames = if (names.length > 2) names.init.tail.toList else Nil

    def shortName = check(names.head + " " + names.last)
    def initalledName = check(names.head + middleNames.map(_.head).mkString(" ") + names.last)
    def initalledDottedName = check(names.head + middleNames.map(_.head).mkString(". ") + names.last)

    val fullNameCheck = check(name)

    if (fullNameCheck.isSuccess) fullNameCheck
    else if (!middleNames.isEmpty) {
      shortName orElse initalledName orElse initalledDottedName
    }
    else fullNameCheck // if all else fails
  }

}
