package cpup.lib.arguments.interpreting

import scala.collection.mutable.ListBuffer
import scala.reflect.runtime.{universe => ru}

import cpup.lib.arguments.Arg
import cpup.lib.arguments.Arg.Parse
import cpup.lib.arguments.parsing.{ArgData, Argument}

class RealRun(_args: Seq[Argument]) extends Run {
	val args = _args.view.zipWithIndex.toList

	override def command(name: String, handle: (Run) => Any) {
		args.find { case (Argument.Positional(ArgData.Single(`name`)), _) => true case _ => false } match {
			case Some((_, i)) => {
				val run = new RealRun(args.drop(i + 1).view.map(_._1))
				handle(run)
				this.run({ run.run })
			}
			case None =>
		}
	}

	var remaining = _args.toList.flatMap { case a: Argument.Positional => Some(a) case _ => None }
	override def arg[T](name: String, usage: String, default: Option[T])(implicit tt: ru.TypeTag[T], parse: Arg.Parse[T]): T = {
		if(remaining.isEmpty) {
			default match {
				case Some(v) => v
				case None => throw new ArgumentMissingException(s"$name is missing")
			}
		} else {
			val a = remaining.head
			remaining = remaining.drop(1)
			parse.parse(a.data: _*) match {
				case Left(v) => v
				case Right(err) => throw new InvalidArgumentException(s"$name is invalid: $err")
			}
		}
	}
	override def vararg[T](name: String, usage: String, necessary: Boolean)(implicit tt: ru.TypeTag[T], parse: Parse[T]): Seq[T] = {
		if(remaining.isEmpty && necessary) throw new ArgumentMissingException(s"$name[] is missing")
		remaining.zipWithIndex.map(a => parse.parse(a._1.data: _*) match {
			case Left(v) => v
			case Right(err) => throw new InvalidArgumentException(s"$name[${a._2} is invalid: $err")
		})
	}

	val flags = _args.view.flatMap { case a: Argument.Flag => Some(a) case _ => None }.reverse.toList
	override def opt[T](names: Set[String], usage: String, default: T, typ: Option[Int])(implicit tt: ru.TypeTag[T], parse: Parse[T]): T = {
		names.foreach(name => if(!Arg.validFlagName(name)) throw new InvalidFlagNameException(s"invalid flag name: $name"))
		val matcher = Argument.Flag.OptMatch(names, typ)
		flags.view.flatMap {
			case matcher(d) => Some(d)
			case _ => None
		}.find(d => true).flatMap(_.asInstanceOf[Option[Seq[ArgData]]]).map(d => {
			parse.parse(d: _*) match {
				case Left(d) => d
				case Right(err) => throw new InvalidArgumentException(s"${names.toList.sortBy(-_.length).head} is invalid: $err")
			}
		}).getOrElse(default)
	}
	override def flag(names: Set[String], usage: String, default: Boolean, typ: Option[Int]) = {
		names.foreach(name => if(!Arg.validFlagName(name)) throw new InvalidFlagNameException(s"invalid flag name: $name"))
		val matcher = Argument.Flag.FlagMatch(names, typ)
		flags.view.flatMap {
			case matcher(d) => Some(d)
			case _ => None
		}.find((_) => true).getOrElse(default)
	}

	val runs = new ListBuffer[() => Any]
	override def run(fn: => Any) = runs += (() => fn)

	def run = runs.foreach(_.apply)
}
