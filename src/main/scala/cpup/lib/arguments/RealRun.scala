package cpup.lib.arguments

import scala.collection.mutable.ListBuffer
import scala.reflect.runtime.{universe => ru}
import cpup.lib.arguments.Arg.{Parse, Positional}
import cpup.lib.arguments.parsing.{ArgData, Argument}

import scala.collection.mutable
import scala.util.control.Breaks

class RealRun(_args: Seq[Argument], val out: Output) extends Run {
	val args = mutable.ListBuffer.empty[Argument]
	args ++= _args
	def argsI = args.view.zipWithIndex

	def pullArg[R](fn: Argument => Option[R]): Option[R] = {
		argsI.flatMap { arg =>
			fn(arg._1).map((_, arg._2))
		}.headOption match {
			case Some((r, i)) =>
				args.remove(i)
				Some(r)

			case None =>
				None
		}
	}

	sealed trait P[+T] extends Run.Param[T]
	object P {
		case class Defined[T](v: T) extends P[T] {
			override def _v = Some(v)
		}
		class Deferred[T] extends P[T] {
			var _v: Option[T] = None
			def v = _v.getOrElse(throw new RuntimeException("trying to get value before parsing is done"))
		}
	}

	val commands = mutable.Map.empty[String, Run => Any]
	override def command(name: String, handle: (Run) => Any) {
		commands(name) = handle
	}

	override def arg[T](arg: Arg.Single[T]) = {
		val firstArg = pullArg {
			case Argument.Positional(data @ _*) => Some(data)
			case _ => None
		}

		val parseRes = firstArg match {
			case Some(data) => arg.parse.parse(data: _*)
			case None => Right("not enough arguments")
		}

		P.Defined(parseRes match {
			case Left(r) => r
			case Right(err) =>
				arg.default match {
					case Some(r) => r
					case None => throw new RuntimeException(err)
				}
		})
	}
	override def vararg[T](vararg: Arg.Var[T]) = {
		val builder = List.newBuilder[T]
		var length = 0
		Breaks.breakable {
			while(vararg.max.forall(length < _)) {
				val firstArg = if(vararg.raw) {
					pullArg { arg => Some(arg.data) }
				} else {
					pullArg {
						case Argument.Positional(data @ _*) => Some(data)
						case _ => None
					}
				}

				val parseRes = firstArg match {
					case Some(data) => vararg.parse.parse(data: _*)
					case None => Right("not enough arguments")
				}

				parseRes match {
					case Left(r) =>
						builder += r
						length += 1

					case Right(err) =>
						if(length < vararg.min)
							throw new RuntimeException(s"not enough arguments (last errored with: $err")
						else
							Breaks.break
				}
			}
		}
		P.Defined(builder.result)
	}

	val runs = new ListBuffer[() => Any]
	override def run(fn: => Any) = runs += (() => fn)

	def run {
		if(commands.isEmpty) {
			for(deferred <- deferreds) deferred(args)
		} else {
			pullArg {
				case a: Argument.Positional => Some(a)
				case _ => None
			} match {
				case Some(arg) =>
					val deferredArgs = args.take(args.indexOf(arg))
					for(deferred <- deferreds) deferred(deferredArgs)

					val cmdName = ArgData.List(arg.data: _*).reify(ArgData.Role.Arg)
					val cmd = commands.getOrElse(cmdName, throw new RuntimeException(s"unknown command: $cmdName"))
					val run = new RealRun(args.drop(args.indexOf(arg) + 1), out)
					cmd(run)
					this.run {
						run.run
					}

				case None =>
					throw new RuntimeException("no command")
			}
		}

		runs.foreach(_.apply)
	}

	val deferreds = mutable.Set.empty[Seq[Argument] => Any]
	override def opt[T](opt: Arg.Opt[T]) = {
		val matcher = Argument.Flag.OptMatch(opt.names, opt.typ)
		val p = new P.Deferred[T]
		deferreds += { args =>
			p._v = Some(args.flatMap {
				case matcher(d) => Some(d)
				case _ => None
			}.headOption.flatMap { a => a }.map { d =>
				opt.parse.parse(d: _*) match {
					case Left(d) => d
					case Right(err) => throw new InvalidArgumentException(opt.names.toList.sortBy(-_.length).head, err)
				}
			}.getOrElse(opt.default))
		}
		p
	}
	override def flag(flag: Arg.Flag) = {
		val matcher = Argument.Flag.FlagMatch(flag.names, flag.typ)
		val p = new P.Deferred[Boolean]
		deferreds += { args =>
			p._v = Some(args.flatMap {
				case matcher(d) => Some(d)
				case _ => None
			}.headOption.getOrElse(flag.default))
		}
		p
	}
}
