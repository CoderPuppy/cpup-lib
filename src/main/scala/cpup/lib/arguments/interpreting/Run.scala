package cpup.lib.arguments.interpreting

import scala.reflect.runtime.{universe => ru}

import cpup.lib.arguments.Arg

trait Run {
	def command(name: String, handle: (Run) => Any)

	def arg[T](name: String, usage: String = "", default: Option[T] = None)(implicit tt: ru.TypeTag[T], parse: Arg.Parse[T]): T
	def vararg[T](name: String, usage: String = "", necessary: Boolean = false)(implicit tt: ru.TypeTag[T], parse: Arg.Parse[T]): Seq[T]

	def opt[T](names: Set[String], usage: String = "", default: T, typ: Option[Int] = None)(implicit tt: ru.TypeTag[T], parse: Arg.Parse[T]): T
	def flag(names: Set[String], usage: String = "", default: Boolean = false, typ: Option[Int] = None): Boolean

	def run(fn: => Any)
}
