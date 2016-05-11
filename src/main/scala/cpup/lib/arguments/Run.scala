package cpup.lib.arguments

import scala.reflect.runtime.{universe => ru}

trait Run {
	def command(name: String, handle: (Run) => Any)

	def arg[T](arg: Arg[T]): T
	def vararg[T](arg: Arg.Var[T]): Seq[T]

	def opt[T](opt: Arg.Opt[T]): T
	def flag(flag: Arg.Flag): Boolean

	def run(fn: => Any)
	def output(msg: String, data: Any*)
	def error(msg: String, data: Any*)
}
