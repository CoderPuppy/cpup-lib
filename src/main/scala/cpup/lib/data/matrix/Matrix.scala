package cpup.lib.data.matrix

import scala.language.higherKinds
import scala.reflect.runtime.{universe => ru}

import cpup.lib.util.Util

trait Matrix[N] {
	def numeric: Numeric[N]
	def canBuild: Matrix.CanBuild
	def typeTag: ru.TypeTag[N]
	def width: Int
	def height: Int
	def apply(x: Int, y: Int): N
	def set(x: Int, y: Int, v: N): Matrix[N]
}

object Matrix {
	trait CanBuild {
		def build[N_](width: Int, height: Int)(implicit numeric: Numeric[N_], tt: ru.TypeTag[N_]): Builder[N_]
	}
	trait Builder[N] {
		def update(x: Int, y: Int, v: N)
		def result: Matrix[N]
	}

	implicit class Rich[N](val matrix: Matrix[N]) {
		implicit val numeric = matrix.numeric
		implicit val canBuild = matrix.canBuild
		implicit val typeTag = matrix.typeTag

		protected[matrix] def build(width: Int, height: Int)(fn: (Int, Int) => N) = {
			val builder = canBuild.build[N](width, height)
			for {
				x <- 0 until width
				y <- 0 until height
			} builder(x, y) = fn(x, y)
			builder.result
		}

		def *(other: N) = build(matrix.width, matrix.height) { (x, y) => numeric.times(matrix(x, y), other) }
		def +(other: N) = build(matrix.width, matrix.height) { (x, y) => numeric. plus(matrix(x, y), other) }
		def -(other: N) = build(matrix.width, matrix.height) { (x, y) => numeric.minus(matrix(x, y), other) }

		def *(other: Matrix[N]) = {
			assert(matrix.width == other.height, "mismatched dimensions")
			build(other.width, matrix.height) { (x, y) =>
				(0 until matrix.width).map(k => numeric.times(matrix(k, y), other(x, k))).sum
			}
		}

		def +(other: Matrix[N]) = {
			assert((matrix.width, matrix.height) == (other.width, other.height), "mismatched dimensions")
			build(matrix.width, matrix.height) { (x, y) => numeric.plus(matrix(x, y), other(x, y)) }
		}

		def -(other: Matrix[N]) = {
			assert((matrix.width, matrix.height) == (other.width, other.height), "mismatched dimensions")
			build(matrix.width, matrix.height) { (x, y) => numeric.minus(matrix(x, y), other(x, y)) }
		}

		def transpose = build(matrix.height, matrix.width) { (x, y) => matrix(y, x) }

		def unary_- = build(matrix.width, matrix.height) { (x, y) => numeric.negate(matrix(x, y)) }

		def determinate: N = {
			assert(matrix.width == matrix.height, "matrix must be square")
			if(matrix.width == 1 && matrix.height == 1) matrix(0, 0) else {
				var res = numeric.zero
				for(x <- 0 until matrix.width) {
					val tmp = numeric.times(matrix(x, 0), sub(x + 1, 1, matrix.width - 1, matrix.height - 1).determinate)
					if(x % 2 == 0)
						res = numeric.plus(res, tmp)
					else
						res = numeric.minus(res, tmp)
				}
				res
			}
		}

		def sub(x: Int, y: Int, width: Int, height: Int) = {
			assert(width >= 1, "must be at least 1 wide")
			assert(width <= matrix.width, s"must not be wider than ${matrix.width}")
			assert(height >= 1, "must be at least 1 high")
			assert(height <= matrix.height, s"must not be taller than ${matrix.height}")
			build(width, height) { (_x, _y) =>
				matrix(Util.wrap(x + _x, 0, matrix.width - 1), Util.wrap(y + _y, 0, matrix.height - 1))
			}
		}

		def tablify = {
			val colWidths = (0 until matrix.width).map { x =>
				(0 until matrix.height)
					.map(y => matrix(x, y).toString.length)
					.sorted.reverse.head
			}
			var res = "+" + colWidths.map("-" * _).mkString("+") + "+"
			for(y <- 0 until matrix.height) {
				res += "\n|" + colWidths.view.zipWithIndex.map { e =>
					val str = matrix(e._2, y).toString
					str + " " * (e._1 - str.length)
				}.mkString("|") + "|"
				res += "\n+" + colWidths.map("-" * _).mkString("+") + "+"
			}
			res
		}
	}
}
