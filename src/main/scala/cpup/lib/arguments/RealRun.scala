package cpup.lib.arguments

import scala.collection.mutable.ListBuffer
import scala.reflect.runtime.{universe => ru}
import cpup.lib.arguments.Arg.{Parse, Positional}
import cpup.lib.arguments.parsing.{ArgData, Argument}

import scala.collection.mutable
import scala.util.control.Breaks

class RealRun(val _args: Seq[Argument], val out: Output) extends Run {
	val args = _args.view.zipWithIndex.toList
	val positional = _args.view.flatMap {
		case p: Argument.Positional => Some(p)
		case _ => None
	}.toList

	protected val params = mutable.ListBuffer.empty[P.Positional[Any, Any]]

	sealed trait P[+T] extends Run.Param[T]
	object P {
		case class Flag(v: Boolean) extends P[Boolean] {
			def _v = Some(v)
		}
		case class Opt[T](v: T) extends P[T] {
			def _v = Some(v)
		}
		trait Positional[T, IT] extends P[T] {
			def arg: Arg.Positional[IT]

			var _v: Option[T] = None
			def v = _v.getOrElse(throw new RuntimeException("getting value of argument before final parsing (probably outside the run block)"))
		}
		case class Single[T](arg: Arg.Single[T]) extends Positional[T, T]
		case class Var[T](arg: Arg.Var[T]) extends Positional[Seq[T], T] {
			protected[arguments] def prepend(v: T) {
				_v = Some(v +: _v.getOrElse(Seq.empty))
			}

			protected[arguments] def append(v: T) {
				_v = Some(_v.getOrElse(Seq.empty) :+ v)
			}
		}
	}

	val commands = mutable.Map.empty[String, Run => Any]
	override def command(name: String, handle: (Run) => Any) {
		commands(name) = handle
	}

	override def arg[T](arg: Arg.Single[T]) = {
		val p = new P.Single[T](arg)
		params += p.asInstanceOf[P.Positional[Any, Any]]
		p
	}
	override def vararg[T](vararg: Arg.Var[T]) = {
		val p = new P.Var(vararg)
		params += p.asInstanceOf[P.Positional[Any, Any]]
		p
	}

	val runs = new ListBuffer[() => Any]
	override def run(fn: => Any) = runs += (() => fn)

	def run {
		val uncontestedInit = params.takeWhile { p => p.arg match {
			case a: Arg.Single[_] => a.default.isEmpty
			case _ => false
		} }

		println('uncontestedInit, uncontestedInit)

		if(positional.size < uncontestedInit.size)
			throw new RuntimeException("not enough arguments")

		for((param, arg) <- uncontestedInit.zip(positional)) {
			param._v = Some(param.arg.parse.parse(arg.data: _*) match {
				case Left(v) => v
				case Right(err) => throw new RuntimeException(s"Error parsing initial uncontested params: $err")
			})
		}

		if(commands.isEmpty) {
			val uncontestedTail = params.view.drop(uncontestedInit.size).reverse.takeWhile {
				case p: P.Single[_] => p.arg.default.isEmpty
				case _ => false
			}.reverse
			println('uncontestedTail, uncontestedTail)

			if(positional.size - uncontestedInit.size < uncontestedTail.size)
				throw new RuntimeException("not enough arguments")

			for((param, arg) <- uncontestedTail.zip(positional.drop(positional.size - uncontestedTail.size))) {
				param._v = Some(param.arg.parse.parse(arg.data: _*) match {
					case Left(v) => v
					case Right(err) => throw new RuntimeException(s"Error parsing tailing uncontested params: $err")
				})
			}

			{
				val contestedParams = params.slice(uncontestedInit.size, params.size - uncontestedTail.size)
				val contestedArgs = positional.slice(uncontestedInit.size, positional.size - uncontestedTail.size)

				println('contestedParams, contestedParams)
				println('contestedArgs, contestedArgs)

				if(contestedParams.exists {
					case p: P.Single[_] => p.arg.default.isEmpty
					case _ => false
				}) {
					val initial = contestedParams.view.takeWhile {
						case p: P.Single[_] if p.arg.default.isEmpty => false
						case _ => true
					}.toList
					val tailing = contestedParams.view.drop(initial.size).reverse.takeWhile {
						case p: P.Single[_] if p.arg.default.isEmpty => false
						case _ => true
					}.toList

					val coreParams = Some(contestedParams.slice(initial.size, contestedParams.size - tailing.size))
					println('coreParams, coreParams)

					println('initial, initial)
					println('tailing, tailing)

					???
				} else {
					var args = contestedArgs
					for(param <- contestedParams) {
						println('param, param)
						param.asInstanceOf[Any] match {
							case s: P.Single[Any] =>
								s._v = Some(args.headOption.flatMap { a =>
									param.arg.parse.parse(a.data: _*).left.toOption
								}.getOrElse(s.arg.default.get))
								args = args.drop(1)

							case pv: P.Var[Any] =>
								pv._v = Some(pv._v.getOrElse(Seq.empty))
								var errO: Option[String] = None
								Breaks.breakable {
									while (true) {
										args.headOption.flatMap { a =>
											param.arg.parse.parse(a.data: _*) match {
												case Left(v) => Some(v)
												case Right(e) =>
													errO = Some(e)
													None
											}
										} match {
											case Some(v) => pv.asInstanceOf[P.Var[Any]].append(v)
											case None => Breaks.break
										}
										args = args.drop(1)
									}
								}
								if(pv._v.getOrElse(Seq.empty).size < pv.arg.min) {
									errO match {
										case Some(err) =>
											throw new RuntimeException(s"not enough arguments for ${pv.arg.name} (last parse errored with $err)")
										case None =>
											throw new RuntimeException(s"not enough arguments for ${pv.arg.name}")
									}
								}
						}
					}
				}
			}
		} else {
			if(params.exists { p => p.arg match {
				case a: Arg.Single[_] if a.default.isDefined => true
				case _: Arg.Var[_] => true
				case _ => false
			} }) {
				throw new RuntimeException("optional and variable positional arguments aren't allowed with subcommands")
			}

			println('commands, commands)

			val cmdArg = positional.lift(uncontestedInit.size).getOrElse(throw new RuntimeException("no subcommand"))
			val cmdName = cmdArg.data.map(_.reify(ArgData.Role.Arg)).mkString("")
			val cmd = commands.getOrElse(cmdName, throw new RuntimeException(s"unknown subcommand: $cmdName"))

			val cmdArgs = _args.drop(_args.indexOf(cmdArg) + 1)

			val run = new RealRun(cmdArgs, out)
			cmd(run)
			runs += (() => {
				run.run
			})
		}

		runs.foreach(_.apply)
	}

	val flags = _args.flatMap { case a: Argument.Flag => Some(a) case _ => None }
	override def opt[T](opt: Arg.Opt[T]) = {
		val matcher = Argument.Flag.OptMatch(opt.names, opt.typ)
		println(flags.view.flatMap {
			case matcher(d) => Some(d)
			case _ => None
		}.toList)
		P.Opt(flags.view.flatMap {
			case matcher(d) => Some(d)
			case _ => None
		}.headOption.flatMap { a => a }.map { d =>
			opt.parse.parse(d: _*) match {
				case Left(d) => d
				case Right(err) => throw new InvalidArgumentException(opt.names.toList.sortBy(-_.length).head, err)
			}
		}.getOrElse(opt.default))
	}
	override def flag(flag: Arg.Flag) = {
		val matcher = Argument.Flag.FlagMatch(flag.names, flag.typ)
		P.Flag(flags.view.flatMap {
			case matcher(d) => Some(d)
			case _ => None
		}.headOption.getOrElse(flag.default))
	}
}
