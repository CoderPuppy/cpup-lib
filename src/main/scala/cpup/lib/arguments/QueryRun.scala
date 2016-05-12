package cpup.lib.arguments

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.reflect.runtime.{universe => ru}

import cpup.lib.arguments.Arg.Parse

class QueryRun extends Run {
	val commands = mutable.Map[String, QueryRun]()
	val runs = mutable.Set[() => Any]()
	val argsL = ListBuffer[Arg.Positional[_]]()
	val argsM = mutable.Map[String, (Int, Arg.Positional[_])]()
	val  opts = mutable.Map[(Option[Int], String), Arg.Opt[_]]()
	val flags = mutable.Map[(Option[Int], String), Arg.Flag]()

	trait P[+T] extends Run.Param[T]
	object P extends P[Nothing] {
		def v = throw new RuntimeException("trying to get the value of a query argument (probably outside the run block)")
		def _v = None
	}

	override def command(name: String, handle: (Run) => Any) {
		if(commands.contains(name)) throw new RuntimeException(s"there's already a command named: $name")
		val run = new QueryRun
		handle(run)
		commands(name) = run
	}

	override def arg[T](arg: Arg.Single[T]) = {
		argsM.get(arg.name) match {
			case None =>
			case _ => throw new RuntimeException("arg already gotten")
		}
		argsM(arg.name) = (argsL.length, arg)
		argsL += arg
		P
	}

	override def vararg[T](vararg: Arg.Var[T]) = {
		argsM.get(vararg.name) match {
			case None =>
			case _ => throw new RuntimeException(s"arg already gotten: ${vararg.name}")
		}
		argsM(vararg.name) = (argsL.length, vararg)
		argsL += vararg
		P
	}

	override def opt[T](opt: Arg.Opt[T]) = {
		opt.names.foreach((name) => {
			(opts.get((opt.typ, name)), opts.get((None, name))) match {
				case (Some(`opt`), None) =>
				case (Some(`opt`), Some(`opt`)) =>
				case (None, None) =>
				case _ => throw new RuntimeException("opt already registered with different values")
			}
			opts((opt.typ, name)) = opt
		})
		P
	}

	override def flag(flag: Arg.Flag) = {
		flag.names.foreach((name) => {
			(flags.get((flag.typ, name)), flags.get((None, name))) match {
				case (Some(`flag`), None) =>
				case (Some(`flag`), Some(`flag`)) =>
				case (None, None) =>
				case _ => throw new RuntimeException("flag already registered with different values")
			}
			flags((flag.typ, name)) = flag
		})
		P
	}

	override def run(fn: => Any) {
		runs += (() => fn)
	}

	def out = new Output {
		override def write(msg: String, data: Any*) {}
		override def error(msg: String, data: Any*) {}
	}
}
