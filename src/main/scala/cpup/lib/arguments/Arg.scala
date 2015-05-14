package cpup.lib.arguments

import cpup.lib.arguments.parsing.ArgData
import cpup.lib.conversion.Convert

object Arg {
	def validFlagName(str: String) = !(str.startsWith("+") || str.startsWith("-") || str.contains("="))

	trait Parse[T] { def parse(arg: ArgData*): Either[T, String] }
	implicit def transitiveParse[F, T](implicit p: Parse[F], c: Convert[F, T]): Parse[T] = new Parse[T] {
		override def parse(arg: ArgData*) = p.parse(arg: _*).left.map(c.convert)
	}

	implicit object StringParse extends Parse[String] {
		override def parse(arg: ArgData*) = Left(arg.mkString(" "))
	}

	trait PrettyPrint[T] { def prettyPrint(v: T): String }
	implicit def transitivePrettyPrint[F, T](implicit c: Convert[F, T], p: PrettyPrint[T]): PrettyPrint[F] = new PrettyPrint[F] {
		override def prettyPrint(v: F) = p.prettyPrint(c.convert(v))
	}

	implicit object  StringPrettyPrint extends PrettyPrint[String] { override def prettyPrint(v: String) = v }
	implicit object DefaultPrettyPrint extends PrettyPrint[   Any] { override def prettyPrint(v:    Any) = v.toString }
}
