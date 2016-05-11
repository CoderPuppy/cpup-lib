package cpup.lib.arguments

import scala.reflect.runtime.{universe => ru}

import cpup.lib.arguments.parsing.ArgData
import cpup.lib.conversion.Convert

case class Arg[T](name: String, usage: String = "", default: Option[T] = None, parse: Arg.Parse[T])(implicit val tt: ru.TypeTag[T]) {
	if(!Arg.validFlagName(name)) throw new InvalidFlagNameException(s"invalid flag name: $name")
}

object Arg {
	case class Var[T](name: String, usage: String = "", necessary: Boolean = false, parse: Arg.Parse[T])(implicit val tt: ru.TypeTag[T]) extends SingleName
	case class Opt[T](names: Set[String], usage: String = "", default: T, typ: Option[Int] = None, parse: Arg.Parse[T])(implicit val tt: ru.TypeTag[T])
	case class Flag(names: Set[String], usage: String = "", default: Boolean = false, typ: Option[Int] = None)

	def validFlagName(str: String) = !(str.startsWith("+") || str.startsWith("-") || str.contains("="))

	type Parse[T] = (ArgData*) => Either[T, String]
}
