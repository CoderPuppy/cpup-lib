package cpup.lib.module

import com.typesafe.config.ConfigFactory

object RootModule extends TRootModule with TModule[TRootModule] {
	ModuleLoader.modulesByInst(this) = this
	override final val inst = this
	override lazy val spec = new ModuleSpec[TRootModule](ModuleLoader.moduleType[TRootModule](classOf[TRootModule]), null) {
		override def parent = this
		override def config = ConfigFactory.load
		override def id = ""
	}
}
@ModuleID(id = "cpup-root-module")
abstract class TRootModule {}
