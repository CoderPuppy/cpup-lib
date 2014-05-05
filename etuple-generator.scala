val end = 22

def createETuple(parts: Seq[String], before: String = "", after: String = ""): String = if(parts.length == 1) {
	s"${before}ETuple1(${parts.head})$after"
} else {
	createETuple(parts.tail, s"${before}ETuple${parts.length}(", s", ${parts.head})$after")
}

def createTuple(parts: Seq[String]) = (if(parts.length == 1) { "Tuple1" } else { "" }) + s"(${parts.mkString(", ")})"

println(s"""trait Rewrapper[SW[_], RW[_]] {
	def rewrap[T](s: SW[T]): RW[T]
}

class ETupleBuilder[W[_]] {
	trait ETuple[T] {
		def value: W[T]
		def get = value
		def rest: Option[ETuple[_]]
		def tuple: Product
		def arity: Int

		def ~[NT](next: W[NT]): ETuple[NT]

		def realArity: Int = realArity(1)
		def realArity(acc: Int): Int = rest match {
			case Some(rest) =>
				rest.realArity(acc + 1)

			case None =>
				acc
		}
	}""")

for(i <- 1 to end) {
	println(s"""
case class ETuple$i[${(1 to i).map("T" + _).mkString(", ")}](${
		if(i > 1) {
			s"header: ETuple${i - 1}[${(1 until i).map("T" + _).mkString(", ")}], "
		} else {
			""
		}
	}value: W[T$i]) extends ETuple[T$i] {
	${
		if(i > 1) {
			"def rest = Some(header)"
		} else {
			"def rest = None"
		}
	}
	def tuple = ${
		createTuple((i - 1 to 1 by -1).map((n) => {
			("header." * n) + "value"
		}) :+ "value")
	}
	def mapAll[R](f: (${
		(1 to i).map("W[T" + _ + "]").mkString(", ")
	}) => R) = f(${
		((i - 1 to 1 by -1).map((n) => {
			("header." * n) + "value"
		}) :+ "value").mkString(", ")
	})
	def arity = $i
	def ~[NT](next: W[NT]) = ${
		if(i < end) {
			s"ETuple${i + 1}(this, next)"
		} else {
			s"throw new RuntimeException(${'"'}Cannot go more than $end${'"'})"
		}
	}
	def map[RW[_]](mapper: Rewrapper[W, RW])(implicit rBuilder: ETupleBuilder[RW]) = rBuilder(${
		((i - 1 to 1 by -1).map((n) => {
			("header." * n) + "value"
		}) :+ "value").map((str) => {
			s"mapper.rewrap($str)"
		}).mkString(", ")
	})
}

def apply[${(1 to i).map("T" + _).mkString(", ")}](${(1 to i).map(i => s"v$i: W[T$i]").mkString(", ")}) = ${
	createETuple((1 to i).map("v" + _))
}""".split("\n").map("\t" + _).mkString("\n"))
}

println(s"""}""")