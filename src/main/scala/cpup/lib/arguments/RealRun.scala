package cpup.lib.arguments

import scala.collection.mutable.ListBuffer
import scala.reflect.runtime.{universe => ru}

import cpup.lib.arguments.Arg.Parse
import cpup.lib.arguments.parsing.{ArgData, Argument}

class RealRun(_args: Seq[Argument], out: Output) extends Run {
	val args = _args.view.zipWithIndex.toList

	override def command(name: String, handle: (Run) => Any) {
		args.find { case (Argument.Positional(ArgData.Single(`name`)), _) => true case _ => false } match {
			case Some((_, i)) => {
				val run = new RealRun(args.drop(i + 1).view.map(_._1), out)
				handle(run)
				this.run({ run.run })
			}
			case None =>
		}
	}

	var remaining = _args.toList.flatMap { case a: Argument.Positional => Some(a) case _ => None }
	override def arg[T](arg: Arg[T]): T = {
		val opts = remaining.view.map(a => arg.parse.parse(a.data: _*)).zipWithIndex
		opts.find(_._1.isLeft).map(a => (a._1.left.get, a._2)) match {
			case Some((v, i)) => {
				val (before, _ :: after) = remaining.splitAt(i)
				remaining = before ++ after
				v
			}
			case None => arg.default match {
				case Some(v) => v
				case None => throw new ArgumentMissingException(arg.name, remaining.zip(opts).map(a => (a._1.data, a._2._1.right.get)))
			}
		}
	}
	override def vararg[T](vararg: Arg.Var[T]): Seq[T] = {
		if(remaining.isEmpty && vararg.necessary) throw new ArgumentMissingException(s"${vararg.name}[]", List())
		remaining.zipWithIndex.map(a => vararg.parse.parse(a._1.data: _*) match {
			case Left(v) => {
				remaining = List()
				v
			}
			case Right(err) => throw new InvalidArgumentException(s"${vararg.name}[${a._2}]", err)
		})
	}

	val flags = _args.view.flatMap { case a: Argument.Flag => Some(a) case _ => None }.reverse.toList
	override def opt[T](opt: Arg.Opt[T]): T = {
		val matcher = Argument.Flag.OptMatch(opt.names, opt.typ)
		flags.view.flatMap {
			case matcher(d) => Some(d)
			case _ => None
		}.find(d => true).flatMap(_.asInstanceOf[Option[Seq[ArgData]]]).map(d => {
			opt.parse.parse(d: _*) match {
				case Left(d) => d
				case Right(err) => throw new InvalidArgumentException(opt.names.toList.sortBy(-_.length).head, err)
			}
		}).getOrElse(opt.default)
	}
	override def flag(flag: Arg.Flag) = {
		val matcher = Argument.Flag.FlagMatch(flag.names, flag.typ)
		flags.view.flatMap {
			case matcher(d) => Some(d)
			case _ => None
		}.find((_) => true).getOrElse(flag.default)
	}

	val runs = new ListBuffer[() => Any]
	override def run(fn: => Any) = runs += (() => fn)

	def run = runs.foreach(_.apply)

	override def output(msg: String, data: Any*) = out.write(msg, data: _*)
	override def error(msg: String, data: Any*) = out.error(msg, data: _*)
}
