package cpup.lib.inspecting

import java.lang.{String => JStr}
import java.util.UUID

import scala.collection.mutable
import scala.ref.WeakReference
import scala.reflect.runtime.{universe => ru}
import scala.{Byte => SByte, Double => SDouble, Float => SFloat, Int => SInt, List => SList, Long => SLong, Short => SShort}

import cpup.lib.reflect.ReflectUtil

class Registry {
	protected val _insts = new mutable.Map[(JStr, List[Data]), (AnyRef, ru.Type)] {
		val map = mutable.Map.empty[(JStr, List[Data]), (WeakReference[AnyRef], ru.Type)]
		override def get(key: (JStr, SList[Data])) = map.get(key) match {
			case Some((v, typ)) => v.get match {
				case Some(v) => Some((v, typ))
				case None => {
					map -= key
					None
				}
			}
			case None => None
		}

		override def +=(kv: ((JStr, SList[Data]), (AnyRef, ru.Type))): this.type = {
			map += kv._1 -> (new WeakReference[AnyRef](kv._2._1), kv._2._2)
			this
		}

		override def -=(key: (JStr, SList[Data])): this.type = {
			map -= key
			this
		}

		override def iterator = map.iterator.flatMap { kv => kv._2._1.get match {
			case Some(v) => Some((kv._1, (v, kv._2._2)))
			case None => {
				map -= kv._1
				None
			}
		} }
	}
	protected val _getter = new mutable.HashMap[JStr, mutable.ListBuffer[Seq[Any] => Option[Data]]] {
		def addBinding(typ: JStr, fn: Seq[Any] => Option[Data]) = {
			getOrElseUpdate(typ, new mutable.ListBuffer[Seq[Any] => Option[Data]]) += fn
			this
		}
		def removeBinding(typ: JStr, fn: Seq[Any] => Option[Data]) = {
			getOrElseUpdate(typ, new mutable.ListBuffer[Seq[Any] => Option[Data]]) -= fn
			this
		}
	}

	def register[T <: AnyRef](inst: T, tpe: ru.Type, typ: JStr, id: Data*) = {
		_insts((typ, id.toList)) = (inst, tpe)
		this
	}
	def register [T <: AnyRef](inst: T, typ: JStr, id: Data*)(implicit tt: ru.TypeTag[T]): Registry = register[T](inst, tt.tpe, typ, id: _*)
	def register_[T <: AnyRef](inst: T, typ: JStr, id: Data*): Registry = register[T](inst, ReflectUtil.tpe(inst), typ, id: _*)

	def unregister(typ: JStr, id: Data*) = {
		_insts.remove((typ, id.toList))
		this
	}

	def register(typ: JStr, fn: Seq[Any] => Option[Data]) = {
		_getter.addBinding(typ, fn)
		this
	}

	def unregister(typ: JStr, fn: Seq[Any] => Option[Data]) = {
		_getter.removeBinding(typ, fn)
		this
	}

	def get(typ: JStr, id: Data*) = {
		_insts.get((typ, id.toList)) match {
			case Some(inst) => Left(inspect(inst._1, new Object, inst._2, ru.typeOf[Object]))
			case None => {
				val args = id.map(_.unpack)
				_getter.get(typ).flatMap(fns => {
					fns.view.flatMap(fn => fn(args)).find((v) => true)
				}) match {
					case Some(d) => Left(d)
					case None => Right("unknown")
				}
			}
		}
	}

	protected val _inspectors = mutable.Map[(ru.Type, ru.Type), mutable.Set[(Any, Any) => Option[Data.Table]]]()

	def register[O, C](fn: (O, C) => Option[Data.Table], oTpe: ru.Type, cTpe: ru.Type) {
		_inspectors.getOrElseUpdate((oTpe, cTpe), mutable.Set[(Any, Any) => Option[Data.Table]]()) += fn.asInstanceOf[(Any, Any) => Option[Data.Table]]
	}

	def register [O, C](fn: (O, C) => Option[Data.Table])(implicit oTT: ru.TypeTag[O], cTT: ru.TypeTag[C]): Unit = register[O, C](fn, oTT.tpe, cTT.tpe)

	def inspect[O, C](obj: O, ctx: C, oTpe: ru.Type, cTpe: ru.Type) = {
		var tbl = Data.Table()
		for {
			((oTpe_, cTpe_), fns) <- _inspectors if oTpe <:< oTpe_ && cTpe <:< cTpe_
			fn <- fns
			res <- fn(obj, ctx)
		} tbl ++= res
		tbl
	}

	def inspect [O, C](obj: O, ctx: C)(implicit oTT: ru.TypeTag[O], cTT: ru.TypeTag[C]): Data.Table = inspect[O, C](obj, ctx, oTT.tpe, cTT.tpe)
	def inspect_[O, C](obj: O, ctx: C): Data.Table = inspect[O, C](obj, ctx, ReflectUtil.tpe(obj), ReflectUtil.tpe(ctx))

	trait IDed {
		def typ: String

		var uuid = UUID.randomUUID
		_register

		protected[inspecting] def _register = register_(this, typ, Data.String(uuid.toString))
		protected[inspecting] def _unregister = unregister(typ, Data.String(uuid.toString))

		def changeUUID(newUUID: UUID) {
			_unregister
			uuid = newUUID
			_register
		}

		def link = Data.Link(typ, Data.String(uuid.toString))
	}

	sealed trait Data {
		def follow: Either[Data, JStr] = Left(this)
		def unpack: Any
	}

	object Data {
		case class Table(private val kvs: (JStr, Data)*) extends Data with Map[JStr, Data] {
			kvs.map(_._1).foldLeft(Set[JStr]())((s, k) => {
				if(s.contains(k))
					throw new RuntimeException(s"Duplicate key: $k}")
				s + k
			})

			def +(kv: (JStr, Data)) = Table(kvs.filter(_._1 != kv._1) ++ SList(kv): _*)
			def ++(other: Table): Table = this ++ other.kvs
			def ++(other: Seq[(JStr, Data)]) = other.foldLeft(this)(_ + _)

			override def follow: Either[Data, JStr] = {
				Left(Table(kvs.map(kv => {
					(kv._1, kv._2.follow match {
						case Left(d) => d
						case Right(err) => return Right(err)
					})
				}): _*))
			}

			override def unpack = Map(kvs.map(kv => (kv._1, kv._2.unpack)): _*)

			override def toString = s"{ ${kvs.map(kv => s"${kv._1} = ${kv._2}; ").mkString}}"

			override def +[B1 >: Data](kv: (JStr, B1)): Map[JStr, B1] = kv._2 match {
				case v: Data => this + (kv._1 -> v)
				case _ => ???
			}

			override def get(key: JStr): Option[Data] = kvs.find(_._1 == key).map(_._2)
			override def iterator = kvs.iterator
			override def -(key: JStr): Map[JStr, Data] = Table(kvs.filterNot(_._1 == key): _*)
		}

		case class List(private val els: Data*) extends Data with Seq[Data] {
			override def follow: Either[Data, JStr] = Left(List(els.map(_.follow match {
				case Left(d) => d
				case Right(err) => return Right(err)
			}): _*))
			override def unpack = els.map(_.unpack)

			override def toString = s"[ ${els.map(_.toString + "; ").mkString}]"

			override def apply(idx: SInt) = els(idx)
			override def length = els.length
			override def iterator = els.iterator
		}

		case class Link(typ: JStr, id: Data*) extends Data {
			override def follow = get(typ, id: _*).left.flatMap(_.follow)

			override def unpack = ???

			override def toString = s"$typ: ${id.mkString(", ")}"
		}

		case object Nil extends Data {
			override def unpack = null

			override def toString = "nil"
		}

		sealed trait Primitive extends Data {
			override def toString = unpack.toString
		}
		case class String(unpack:    JStr) extends Data with Primitive
		case class    Int(unpack:    SInt) extends Data with Primitive
		case class   Byte(unpack:   SByte) extends Data with Primitive
		case class  Short(unpack:  SShort) extends Data with Primitive
		case class   Long(unpack:   SLong) extends Data with Primitive
		case class Double(unpack: SDouble) extends Data with Primitive
		case class  Float(unpack:  SFloat) extends Data with Primitive
	}
}
