package cpup.lib.arguments

import scala.language.higherKinds

trait MultipleNames[S[T] <: Seq[T]] {
	for(name <- names if !Arg.validFlagName(name)) throw new InvalidFlagNameException(s"invalid flag name: $name")
	def names: S[String]
}
