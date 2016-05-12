package cpup.lib.arguments.parsing

trait ArgData {
	def prettyPrint: String
	def reify(role: ArgData.Role): String
	def flatten: String
}

object ArgData {
	case class List(data: ArgData*) extends ArgData {
		override def toString = s"List(${data.mkString(", ")})"

		override def prettyPrint = s"[==[${data.map(_.prettyPrint).mkString("")}]==]"

		override def reify(ctx: Role) = ctx match {
			case ArgData.Role.ArgPart => s"[=[${data.map(_.reify(ArgData.Role.Arg)).mkString}]=]"
			case ArgData.Role.Arg => data.map(_.reify(ArgData.Role.ArgPart)).mkString
		}

		override def flatten = data.map(_.flatten).mkString
	}
	case class Single(data: String) extends ArgData {
		override def prettyPrint = data
		override def reify(role: Role) = data
		override def flatten = data
	}
	case object Space extends ArgData {
		override def prettyPrint = " "
		override def reify(role: Role) = " "
		override def flatten = " "
	}

	sealed trait Role
	object Role {
		case object Arg extends Role
		case object ArgPart extends Role
	}
}
