package cpup.lib.arguments

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.reflect.runtime.{universe => ru}

import cpup.lib.arguments.Arg.Parse

class QueryRun extends Run {
	val commands = mutable.Map[String, QueryRun]()
	val runs = mutable.Set[() => Any]()
	val argsL = ListBuffer[Arg[_]]()
	val argsM = mutable.Map[String, (Int, Arg[_])]()
	var vararg: Option[Arg.Var[_]] = None
	val  opts = mutable.Map[(Option[Int], String), Arg.Opt[_]]()
	val flags = mutable.Map[(Option[Int], String), Arg.Flag]()

	override def command(name: String, handle: (Run) => Any) {
		if(commands.contains(name)) throw new RuntimeException(s"there's already a command named: $name")
		val run = new QueryRun
		handle(run)
		commands(name) = run
	}

	override def arg[T](arg: Arg[T]): T = {
		argsM.get(arg.name) match {
			case None =>
			case _ => throw new RuntimeException("arg already gotten")
		}
		argsM(arg.name) = (argsL.length, arg)
		argsL += arg
		arg.default.getOrElse(null.asInstanceOf[T])
	}

	override def vararg[T](_vararg: Arg.Var[T]): Seq[T] = {
		vararg match {
			case Some(`_vararg`) =>
			case None => vararg = Some(_vararg)
			case _ => throw new RuntimeException("vararg is already registered with different data")
		}
		List()
	}

	override def opt[T](opt: Arg.Opt[T]): T = {
		opt.names.foreach((name) => {
			(opts.get((opt.typ, name)), opts.get((None, name))) match {
				case (Some(`opt`), None) =>
				case (Some(`opt`), Some(`opt`)) =>
				case (None, None) =>
				case _ => throw new RuntimeException("opt already registered with different values")
			}
			opts((opt.typ, name)) = opt
		})
		opt.default
	}

	override def flag(flag: Arg.Flag): Boolean = {
		flag.names.foreach((name) => {
			(flags.get((flag.typ, name)), flags.get((None, name))) match {
				case (Some(`flag`), None) =>
				case (Some(`flag`), Some(`flag`)) =>
				case (None, None) =>
				case _ => throw new RuntimeException("flag already registered with different values")
			}
			flags((flag.typ, name)) = flag
		})
		flag.default
	}

	override def run(fn: => Any) {
		runs += (() => fn)
	}

	override def output(msg: String, data: Any*) {}
	override def error(msg: String, data: Any*) {}
}
