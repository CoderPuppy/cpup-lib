package cpup.lib.module

trait TModule[T <: AnyRef] {
	def inst: T
	def spec: ModuleSpec[_ <: T]
	final val id = spec.id
	final val config = spec.config
	final val parent = spec.parent
	final val logger = spec.logger
	final val typ = spec.typ

	override def toString = s"#Module<${spec.id}>"
}
