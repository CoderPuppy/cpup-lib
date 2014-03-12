package cpup.lib

object Util {
	def checkNull[T, R](obj: T, fn: (T) => R, default: R): R = {
		if(obj == null) {
			default
		} else {
			fn(obj)
		}
	}
}