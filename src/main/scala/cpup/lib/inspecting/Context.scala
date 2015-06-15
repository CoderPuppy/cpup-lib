package cpup.lib.inspecting

import scala.reflect.runtime.{universe => ru}

class Context protected[inspecting](val facets: Map[(ru.TypeTag[_], Symbol), Any]) {
	def this() = this(Map.empty[(ru.TypeTag[_], Symbol), Any])

	def withF[F](v: F, id: Symbol = 'default)(implicit tt: ru.TypeTag[F]) = new Context(facets + ((tt, id) -> v))

	def get[F](id: Symbol = 'default)(implicit tt: ru.TypeTag[F]) = facets.get((tt, id)).map(_.asInstanceOf[F])
	def apply[F](id: Symbol = 'default)(implicit tt: ru.TypeTag[F]) = get[F](id)
}
