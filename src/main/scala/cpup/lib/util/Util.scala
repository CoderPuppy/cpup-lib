package cpup.lib.util

import scala.collection.mutable.ListBuffer

object Util {
	def checkNull[R](obj: R, default: R): R = {
		if(obj == null) {
			default
		} else {
			obj
		}
	}

	def ancestors(_cla: Class[_]) = {
		var cla = _cla
		val ancestors = new ListBuffer[Class[_]]
		while(cla != null) {
			ancestors += cla
			cla = cla.getSuperclass
		}
		ancestors.toList
	}

	def wrap[N](n: N, min: N, max: N)(implicit numeric: Numeric[N]): N = {
		val range = numeric.plus(numeric.minus(max, min), numeric.one)
		if(numeric.gt(n, max))
			wrap(numeric.minus(n, range), min, max)
		else if(numeric.lt(n, min))
			wrap(numeric.plus(n, range), min, max)
		else
			n
	}
}
