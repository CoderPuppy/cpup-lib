package cpup.lib.arguments.parsing

import scala.collection.mutable.ArrayBuffer

object Parser {
	def parse(argv: String): Either[Seq[Argument], (Int, String)] = {
		val path = new ArrayBuffer[ArgData.List]
		path += new ArgData.List

		var brackets = List[Int]()
		var bracketsOpening = false
		var bracketsTemp: Option[Int] = None

		val data = new StringBuilder

		var escape = false

		val backlog = new StringBuilder

		for((char, i) <- argv.view.zipWithIndex) {
			backlog += char
			if(path.length == (brackets.length + 1) * 2 - 1 && char != ' ' && bracketsTemp.isEmpty && char != ']') {
				val l = new ArgData.List
				add(path, l)
				path += l
			}
			if(escape) {
				data += char
			} else if(bracketsTemp.isDefined) {
				char match {
					case '=' => bracketsTemp = bracketsTemp.map(_ + 1)
					case '[' if bracketsOpening => {
						if(data.nonEmpty) {
							add(path, ArgData.Single(data.toString))
							data.clear
						}
						brackets ::= bracketsTemp.get
						bracketsTemp = None
						val l = new ArgData.List
						add(path, l)
						path += l
					}
					case ']' if !bracketsOpening && brackets.head == bracketsTemp.get => {
						if(path.length == (brackets.length + 1) * 2) {
							path.remove(path.length - 1)
						}
						brackets = brackets.tail
						bracketsTemp = None
						path.remove(path.length - 1)
					}
					case _ if bracketsOpening => {
						data += '['
						data ++= "=" * bracketsTemp.get
						data += char
						bracketsTemp = None
					}
					case _ if !bracketsOpening => {
						if(data.isEmpty) {
							val l = new ArgData.List
							add(path, l)
							path += l
						}
						data += ']'
						data ++= "=" * bracketsTemp.get
						data += char
						bracketsTemp = None
					}
				}
			} else {
				char match {
					case '[' => {
						bracketsTemp = Some(0)
						bracketsOpening = true
					}
					case ']' if !brackets.isEmpty => {
						bracketsTemp = Some(0)
						bracketsOpening = false
					}
					case ' ' => {
						if(data.nonEmpty) {
							add(path, ArgData.Single(data.toString))
							data.clear
						}
						if(path.length == (brackets.length + 1) * 2) {
							path.remove(path.length - 1)
						}
						add(path, ArgData.Space)
					}
					case '\\' => escape = true
					case _ => {
						data += char
					}
				}
			}
		}
		if(brackets.nonEmpty) return Right((argv.length - 1, "unclosed brackets"))
		if(data.nonEmpty) {
			add(path, ArgData.Single(data.toString))
			data.clear
		}
		if(path.length == (brackets.length + 1) * 2) {
			path.remove(path.length - 1)
		}
		if(path.length > 1) return Right((argv.length - 1, "invalid state (ending with nesting)"))
//		println(path.head)
		Left(
			path.head.data
				.flatMap { case l: ArgData.List => Some(l.data) case ArgData.Space => None }
				.map {
					case List(ArgData.Single(Argument.Flag.pattern(typ, data))) => Argument.Flag(typ, ArgData.Single(data))
					case ArgData.Single(Argument.Flag.pattern(typ, data)) :: tl => Argument.Flag(typ, (if(data.isEmpty) {
						tl
					} else {
						ArgData.Single(data) :: tl
					}): _*)
					case l => Argument.Positional(l: _*)
				}
		)
	}

	def add(path: ArrayBuffer[ArgData.List], v: ArgData) = {
		for((l, i) <- path.view.zipWithIndex.reverse) {
			path(i) = if(i == path.length - 1) {
				ArgData.List(l.data ++ List(v): _*)
			} else {
				ArgData.List((path(i + 1) :: l.data.reverse.tail.toList).reverse: _*)
			}
		}
		v
	}
}
