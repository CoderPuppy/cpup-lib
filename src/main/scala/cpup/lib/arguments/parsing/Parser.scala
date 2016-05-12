package cpup.lib.arguments.parsing

import cpup.lib.arguments.parsing.ArgData.Single

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Parser {
	def parseLow(s: String): Either[ArgData.List, (Int, String)] = {
		val low = new Low
		for(char <- s) {
			low.handle(char)
		}
		if(low.brackets.nonEmpty)
			return Right(s.length - 1, "unclosed brackets")
		if(low.inArg)
			low.path.out
		if(low.path.size > 1)
			return Right(s.length - 1, s"ending with ${low.path.size} levels of nesting")
		Left(low.root)
	}

	class Path {
		val stack = new mutable.ArrayStack[ArgData.List]
		def size = stack.size
		def root = stack.last

		def tip = stack.head
		def tip_=(neww: ArgData.List) {
			var update = neww
			stack.pop
			for((ArgData.List(list @ _*), i) <- stack.view.zipWithIndex) {
				val newL = ArgData.List(list.init :+ update: _*)
				stack(i) = newL
				update = newL
			}
			stack.push(neww)
		}

		def in {
			tip.data.lastOption match {
				case Some(l: ArgData.List) =>
					stack.push(l)
				case Some(d) =>
					throw new RuntimeException(s"cannot go into $d")
				case None =>
					throw new RuntimeException("cannot go in from an empty list")
			}
		}

		def out {
			stack.pop
		}

		def <<(data: ArgData) {
			tip = ArgData.List(((tip.data.lastOption, data) match {
				case (Some(ArgData.Single(existing)), ArgData.Single(neww)) =>
					tip.data.init :+ ArgData.Single(existing + neww)

				case (_, _) =>
					tip.data :+ data
			}): _*)
		}

		stack.push(ArgData.List())
	}

	class Low {
		val path = new Path
		def root = path.root

		def inArg = path.size % 2 == 0

		def ensureArg {
			if(!inArg) {
				path << ArgData.List()
				path.in
			}
			assert(inArg)
		}

		var escape: Boolean = false

		val brackets = new mutable.Stack[Int]
		var bracketTempO: Option[Int] = None
		var bracketOpening: Boolean = false

		def handle(char: Char): Unit = {
			if(escape) {
				ensureArg
				path << ArgData.Single(char.toString)
				escape = false
			} else bracketTempO match {
				case Some(bracketTemp) =>
					char match {
						case '=' =>
							bracketTempO = Some(bracketTemp + 1)

						case '[' if bracketOpening =>
							ensureArg
							path << ArgData.List()
							path.in
							bracketTempO = None
							brackets.push(bracketTemp)

						case ']' if !bracketOpening && brackets.head == bracketTemp =>
							if(inArg) path.out
							path.out
							bracketTempO = None
							brackets.pop

						case _ =>
							ensureArg
							path << ArgData.Single((if(bracketOpening) "[" else "]") + ("=" * bracketTemp) + char)
					}

				case None =>
					char match {
						case '\\' =>
							escape = true

						case '[' =>
							bracketTempO = Some(0)
							bracketOpening = true

						case ']' if brackets.nonEmpty =>
							bracketTempO = Some(0)
							bracketOpening = false

						case ' ' =>
							if(inArg) path.out
							path << ArgData.Space

						case _ =>
							ensureArg
							path << ArgData.Single(char.toString)
					}
			}
		}
	}

	def parseHigh(data: Seq[ArgData]) = {
		data
			.flatMap {
				case l: ArgData.List => Some(l.data)
				case s: ArgData.Single => Some(Seq(s))
				case ArgData.Space => None
			}
			.map {
				case List(ArgData.Single(Argument.Flag.pattern(typ, data))) => Argument.Flag(typ, ArgData.Single(data))
				case ArgData.Single(Argument.Flag.pattern(typ, data)) :: tl => Argument.Flag(typ, (if(data.isEmpty) {
					tl
				} else {
					ArgData.Single(data) :: tl
				}): _*)
				case l => Argument.Positional(l: _*)
			}
	}
}
