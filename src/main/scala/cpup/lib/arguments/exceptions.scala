package cpup.lib.arguments

import cpup.lib.arguments.parsing.ArgData

class ArgumentMissingException(name: String, candidates: Seq[(Seq[ArgData], String)]) extends Exception(s"$name is missing (candidates: ${
	candidates.map(a => s"${a._1.mkString("")} (${a._2})")
}")
class InvalidArgumentException(arg: String, err: String) extends Exception(s"$arg is invalid: $err")
class InvalidFlagNameException(message: String) extends Exception(message)
