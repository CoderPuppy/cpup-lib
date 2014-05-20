package cpup.lib.module

class MultiModule[I](val impls: Module[_ <: I]*) extends Module[I] {
	protected var _impl: Option[Module[I]] = None
	def impl = _impl

	override def get = impl.get.get

	override def canLoad = impls.foldLeft(super.canLoad)(_ && _.canLoad)

	override def load {
		super.load
		logger.info("Loading")
		_impl = impls.find((impl) => {
			val canLoad = impl.canLoad

			logger.info("Trying {}", impl.name)
			for(msg <- canLoad.messages) {
				impl.logger.info("-- {}", msg: Any)
			}
			canLoad.toBoolean && (try {
				impl.load
				impl.logger.info("Loaded")
				true
			} catch {
				case t: Throwable =>
					impl.logger.info("-- Threw while loading", t)
					false
			})
		}).asInstanceOf[Option[Module[I]]]
		logger.info("Loaded")
	}
}