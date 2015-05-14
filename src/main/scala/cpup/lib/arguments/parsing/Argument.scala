package cpup.lib.arguments.parsing

trait Argument {

}

object Argument {
	case class Flag(typ: String, data: ArgData*) extends Argument

	object Flag {
		val pattern = "^(\\++|-+)([\\w\\W]+)$".r
		val onPattern  = "^\\++$".r
		val offPattern = "^-+$".r

		case class FlagMatch(names: Set[String], typ: Option[Int]) {
			def unapply(a: Argument.Flag): Option[Boolean] = {
				typ match {
					case Some(typ) if a.typ.length == typ =>
					case None =>
					case _ => return None
				}
				val arg = a.data.toList match {
					case List(ArgData.Single(arg)) => arg
					case _ => return None
				}
				val name = names.find(arg.startsWith) match {
					case Some(name) => name
					case None => return None
				}
				Some(a.typ.contains("+"))
			}
		}
		case class OptMatch(names: Set[String], typ: Option[Int]) {
			def unapply(a: Argument.Flag): Option[Option[Seq[ArgData]]] = {
				typ match {
					case Some(typ) if a.typ.length == typ =>
					case None =>
					case _ => return None
				}
				val hasData = a.typ.contains("+")
				val (arg, tl) = a.data.toList match {
					case ArgData.Single(arg) :: tl => (arg, tl)
					case _ => return None
				}
				if(!hasData && tl.nonEmpty) return None
				val name = (if(hasData) names.find(name => arg.startsWith(name + "=")) else names.find(arg.startsWith)) match {
					case Some(name) => name
					case None => return None
				}
				Some(if(hasData) {
					val rem = arg.substring(name.length + 1)
					Some(if(rem.isEmpty) {
						tl
					} else {
						ArgData.Single(rem) :: tl
					})
				} else {
					None
				})
			}
		}
	}

	case class Positional(data: ArgData*) extends Argument
}
