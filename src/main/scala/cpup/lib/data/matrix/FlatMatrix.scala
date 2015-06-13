package cpup.lib.data.matrix

import scala.reflect.ClassTag
import scala.reflect.runtime.{universe => ru}

class FlatMatrix[N](val width: Int, val height: Int, data_ : Seq[N])(implicit val numeric: Numeric[N], tt: ru.TypeTag[N]) extends Matrix[N] {
	assert(data_.length == width * height, "wrong array size")

	implicit val classTag = ClassTag[N](ru.runtimeMirror(getClass.getClassLoader).runtimeClass(tt.tpe))
	protected val _data = data_.toArray
	def data = _data.toList
	val typeTag = tt
	def this(width: Int, height: Int)(implicit numeric: Numeric[N], tt: ru.TypeTag[N]) = {
		this(width, height, {
			implicit val classTag = ClassTag[N](ru.runtimeMirror(getClass.getClassLoader).runtimeClass(tt.tpe))
			Array.fill[N](width * height) { numeric.zero }
		})
	}

	override def apply(x: Int, y: Int) = _data(y * width + x)

	override def set(x: Int, y: Int, v: N) = {
		val data_ = Array.fill[N](width * height) { numeric.zero }
		_data.copyToArray(data_)
		data_(y * width + x) = v
		new FlatMatrix[N](width, height, data_)
	}

	override def canBuild = FlatMatrix.CanBuild
}

object FlatMatrix {
	def create[N](width: Int, data: N*)(implicit numeric: Numeric[N], tt: ru.TypeTag[N]) = {
		assert(data.length % width == 0)
		new FlatMatrix[N](width, data.length / width, data)
	}

	object CanBuild extends Matrix.CanBuild {
		override def build[N](width: Int, height: Int)(implicit numeric: Numeric[N], tt: ru.TypeTag[N]) = new Builder[N](width, height)
	}
	class Builder[N](val width: Int, val height: Int)(implicit val numeric: Numeric[N], tt: ru.TypeTag[N]) extends Matrix.Builder[N] {
		implicit val classTag = ClassTag[N](ru.runtimeMirror(getClass.getClassLoader).runtimeClass(tt.tpe))
		val data = Array.fill[N](width * height) { numeric.zero }

		override def update(x: Int, y: Int, v: N) = data(y * width + x) = v

		override def result = new FlatMatrix[N](width, height, data.clone)
	}
}
