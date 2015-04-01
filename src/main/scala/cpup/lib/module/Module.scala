package cpup.lib.module

class Module[T <: AnyRef](final val spec: ModuleSpec[_ <: T], final val inst: T) extends TModule[T] {

}
