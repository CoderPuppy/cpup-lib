package cpup.lib.arguments.parsing

trait ArgData

object ArgData {
	case class List(data: ArgData*) extends ArgData {
		override def toString = s"[==[${data.mkString("")}]==]"
	}
	case class Single(data: String) extends ArgData {
		override def toString = data
	}
	object Space extends ArgData {
		override def toString = " "
	}
}
