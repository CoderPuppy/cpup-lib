package cpup.lib.arguments

import cpup.lib.arguments.parsing.ArgData

import scala.reflect.runtime.{universe => ru}

trait Arg[T] {
	def names: Set[String]
	def desc: String
	def tt: ru.TypeTag[T]
}

object Arg {
	trait Positional[T] extends Arg[T] {
		def name: String
		def names = Set(name)
		def parse: Arg.Parse[T]
	}
	case class Single[T](
		name: String,
		desc: String = "",
		default: Option[T] = None,
		parse: Arg.Parse[T]
	)(implicit val tt: ru.TypeTag[T]) extends Positional[T]
	case class Var[T](
		name: String,
		desc: String = "",
		min: Int = 0,
		max: Option[Int] = None,
		raw: Boolean = false,
		parse: Arg.Parse[T]
	)(implicit val tt: ru.TypeTag[T]) extends Positional[T]

	case class Opt[T](
		names: Set[String],
		desc: String = "",
		default: T,
		parse: Arg.Parse[T],
		typ: Option[Int] = None
	)(implicit val tt: ru.TypeTag[T]) extends Arg[T]
	case class Flag(
		names: Set[String],
		desc: String = "",
		default: Boolean = false,
		typ: Option[Int] = None
	) extends Arg[Boolean] {
		override def tt = implicitly[ru.TypeTag[Boolean]]
	}

	def validFlagName(str: String) = !(str.startsWith("+") || str.startsWith("-") || str.contains("="))

	trait Parse[+T] {
		def parse(args: ArgData*): Either[T, String]
		def map[R](fn: T => R) = {
			val outer = this
			new Parse[R] {
				override def parse(args: ArgData*) = outer.parse(args: _*).left.map(fn)
			}
		}
		def flatMap[R](fn: T => Either[R, String]) = {
			val outer = this
			new Arg.Parse[R] {
				override def parse(args: ArgData*) = outer.parse(args: _*).left.flatMap(fn)
			}
		}
	}
	object Parse {
		object raw extends Parse[ArgData.List] {
			override def parse(args: ArgData*) = Left(ArgData.List(args: _*))
		}
	}

	trait PrettyPrint[-T] {
		def prettyPrint(v: T): String
	}
	object PrettyPrint {
		object string extends PrettyPrint[String] {
			override def prettyPrint(v: String) = v
		}
	}
}
