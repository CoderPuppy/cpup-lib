package cpup.lib.arguments.interpreting

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.reflect.runtime.{universe => ru}

import cpup.lib.arguments.Arg
import cpup.lib.arguments.Arg.Parse

class QueryRun extends Run {
	val commands = mutable.Map[String, QueryRun]()
	val runs = mutable.Set[() => Any]()
	val argsL = ListBuffer[(String,       ru.TypeTag[Any], Option[Any], String, Arg.Parse[Any])]()
	val argsM = mutable.Map[String, (Int, ru.TypeTag[Any], Option[Any], String, Arg.Parse[Any])]()
	var vararg: Option[(String, String, Boolean, ru.TypeTag[Any], Arg.Parse[Any])] = None
	val  opts = mutable.Map[(Option[Int], String), (Set[String], String,     Any, Option[Int], ru.TypeTag[Any], Arg.Parse[Any])]()
	val flags = mutable.Map[(Option[Int], String), (Set[String], String, Boolean, Option[Int])]()

	override def command(name: String, handle: (Run) => Any) {
		if(commands.contains(name)) throw new RuntimeException(s"there's already a command named: $name")
		val run = new QueryRun
		handle(run)
		commands(name) = run
	}

	override def arg[T](name: String, usage: String, _default: Option[T] = None)(implicit _tt: ru.TypeTag[T], _parse: Arg.Parse[T]): T = {
		val tt = _tt.asInstanceOf[ru.TypeTag[Any]]
		val default = _default.asInstanceOf[Option[Any]]
		val parse = _parse.asInstanceOf[Arg.Parse[Any]]
		argsM.get(name) match {
			case None =>
			case _ => throw new RuntimeException("arg already gotten with different values")
		}
		argsM(name) = (argsL.length, tt, default, usage, parse)
		argsL += ((name, tt, default, usage, parse))
		_default.getOrElse(null.asInstanceOf[T])
	}

	override def vararg[T](name: String, usage: String, necessary: Boolean)(implicit tt: ru.TypeTag[T], parse: Parse[T]): Seq[T] = {
		val data = (name, usage, necessary, tt.asInstanceOf[ru.TypeTag[Any]], parse.asInstanceOf[Arg.Parse[Any]])
		vararg match {
			case Some(`data`) =>
			case None => vararg = Some(data)
			case _ => throw new RuntimeException("vararg is already registered with different data")
		}
		List()
	}

	override def opt[T](names: Set[String], usage: String, default: T, typ: Option[Int])(implicit tt: ru.TypeTag[T], parse: Arg.Parse[T]): T = {
		val data = (names, usage, default, typ, tt.asInstanceOf[ru.TypeTag[Any]], parse.asInstanceOf[Arg.Parse[Any]])
		names.foreach((name) => {
			if(!Arg.validFlagName(name)) throw new InvalidFlagNameException(s"invalid flag name: $name")
			(opts.get((typ, name)), opts.get((None, name))) match {
				case (Some(`data`), None) =>
				case (Some(`data`), Some(`data`)) =>
				case (None, None) =>
				case _ => throw new RuntimeException("opt already registered with different values")
			}
			opts((typ, name)) = data
		})
		default
	}

	override def flag(names: Set[String], usage: String, default: Boolean, typ: Option[Int]): Boolean = {
		val data = (names, usage, default, typ)
		names.foreach((name) => {
			if(!Arg.validFlagName(name)) throw new InvalidFlagNameException(s"invalid flag name: $name")
			(flags.get((typ, name)), flags.get((None, name))) match {
				case (Some(`data`), None) =>
				case (Some(`data`), Some(`data`)) =>
				case (None, None) =>
				case _ => throw new RuntimeException("flag already registered with different values")
			}
			flags((typ, name)) = data
		})
		default
	}

	override def run(fn: => Any) {
		runs += (() => fn)
	}
}
