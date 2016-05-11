package cpup.lib.collections

trait TwoWayMap[A, B] {
	def get(key: A): Option[B]
	def unapply(v: B): Option[A]

	def +(kv: (A, B)): TwoWayMap[A, B]
	def -(key: A): TwoWayMap[A, B]
	def iterator: Iterator[(A, B)]

	def apply(key: A): B = get(key).get
	def contains(key: A): Boolean = get(key).isEmpty
}

object TwoWayMap {
	class Basic[A, B](val to: Map[A, B], val from: Map[B, A]) extends TwoWayMap[A, B] {
		override def get(key: A): Option[B] = to.get(key)

		override def unapply(key: B): Option[A] = from.get(key)

		override def +(kv: (A, B)): TwoWayMap[A, B] = if(from.contains(kv._2)) {
			throw new Error("value already registered")
		} else {
			new Basic(to + kv, (to.get(kv._1) match {
				case Some(v) => from - v
				case None => from
			}) + ((kv._2, kv._1)))
		}

		override def -(key: A): TwoWayMap[A, B] = new Basic(to - key, to.get(key) match {
			case Some(v) => from - v
			case None => from
		})

		override def iterator: Iterator[(A, B)] = to.iterator
	}

	def apply[A, B](to: Map[A, B] = Map[A, B](), from: Map[B, A] = Map[B, A]()): TwoWayMap[A, B] = new Basic[A, B](to, from)
	def from[A, B](m: Map[A, B]) = m.foldLeft(TwoWayMap[A, B]())((a, kv) => {
		a + kv
	})
}
