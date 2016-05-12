package cpup.lib.arguments

import scala.language.higherKinds
import scala.reflect.runtime.{universe => ru}

trait Run {
	def command(name: String, handle: (Run) => Any)

	type P[T] <: Run.Param[T]

	def arg[T](arg: Arg.Single[T]): P[T]
	def vararg[T](arg: Arg.Var[T]): P[Seq[T]]

	def opt[T](opt: Arg.Opt[T]): P[T]
	def flag(flag: Arg.Flag): P[Boolean]

	def run(fn: => Any)
	def out: Output
}

object Run {
	trait Param[+T] {
		def v: T
		def _v: Option[T]
	}
}
