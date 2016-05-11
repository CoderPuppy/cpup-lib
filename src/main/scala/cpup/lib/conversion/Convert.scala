package cpup.lib.conversion

import scala.language.implicitConversions

trait Convert[-F, +T] {
	def convert(from: F): T
}

object Convert {
	implicit def transitive[O, I, F](implicit a: Convert[O, I], b: Convert[I, F]): Convert[O, F] = new Convert[O, F] {
		override def convert(from: O): F = b.convert(a.convert(from))
	}
	implicit def distributiveSeq[F, T](implicit c: Convert[F, T]): Convert[Seq[F], Seq[T]] = new Convert[Seq[F], Seq[T]] {
		override def convert(from: Seq[F]): Seq[T] = from.map(c.convert)
	}
	implicit def optionify[T]: Convert[T, Option[T]] = new Convert[T, Option[T]] {
		override def convert(from: T): Option[T] = Some(from)
	}
	implicit def distributiveOpt[F, T](implicit c: Convert[F, T]): Convert[Option[F], Option[T]] = new Convert[Option[F], Option[T]] {
		override def convert(from: Option[F]) = from.map(c.convert)
	}

	implicit object Str2Sym extends Convert[String, Symbol] {
		override def convert(from: String) = Symbol(from)
	}
}
