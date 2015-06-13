package cpup.lib.events

import java.lang.reflect.InvocationTargetException

import scala.collection.mutable
import scala.reflect.ClassTag
import scala.reflect.runtime.{universe => ru}

import cpup.lib.reflect.ReflectUtil

trait EventBus {
	private val handlers = new mutable.HashMap[ru.Type, mutable.Set[(ru.MethodMirror, Int)]]

	def register[T](obj: T, tpe: ru.Type) {
		for((m, tpe, prior) <- EventBus.handlers[T](obj, tpe)) {
			handlers.getOrElseUpdate(tpe, new mutable.HashSet[(ru.MethodMirror, Int)]) += ((m, prior))
		}
	}

	def register [T](obj: T)(implicit tt: ru.TypeTag[T]) { register[T](obj,          tt.tpe     ) }
	def register_[T](obj: T)                             { register[T](obj, ReflectUtil.tpe(obj)) }

	def unregister[T](obj: T, tpe: ru.Type) {
		for((m, tpe, prior) <- EventBus.handlers[T](obj, tpe)) {
			handlers.get(tpe).foreach(_ -= ((m, prior)))
		}
	}

	def unregister [T](obj: T)(implicit tt: ru.TypeTag[T]) { unregister[T](obj,          tt.tpe     ) }
	def unregister_[T](obj: T)                             { unregister[T](obj, ReflectUtil.tpe(obj)) }

	def emit[T](e: T, tpe: ru.Type) {
		try {
			for((handler, anno) <- handlers.filter(tpe <:< _._1).flatMap(_._2).toList.sortBy(-_._2)) {
				if(handler.symbol.paramLists.length == 2)
					handler.apply(e, tpe)
				else
					handler.apply(e)
			}
		} catch {
			case StopEvent =>
			case ex: InvocationTargetException if ex.getCause == StopEvent =>
		}
	}

	def emit [T](obj: T)(implicit tt: ru.TypeTag[T]) { emit[T](obj,          tt.tpe     ) }
	def emit_[T](obj: T)                             { emit[T](obj, ReflectUtil.tpe(obj)) }

	@EventHandler
	def forward(e: Any)(tpe: ru.Type) {
		emit(e, tpe)
	}
}

object EventBus {
	class Base extends EventBus

	def validParams(paramLists: List[List[ru.Symbol]]) = {
		paramLists.length >= 1 && paramLists(0).length == 1 &&
		(if(paramLists.length == 2) {
			paramLists(1).length == 1 &&
			paramLists(1)(0).typeSignature == ru.typeOf[ru.Type]
		} else {
			paramLists.length == 1
		})
	}

	def handlers[T](tpe: ru.Type) = {
		val ehTpe = ru.typeOf[EventHandler]
		(for {
			member <- tpe.members if member.isMethod
			sym = member.asMethod if validParams(sym.paramLists)
			anno <- member.annotations.find(_.tree.tpe == ehTpe)
			paramTpe = sym.paramLists.head.head.typeSignature
		} yield (
			sym, paramTpe,
			anno.tree.children.tail
				.flatMap { case t: ru.AssignOrNamedArg => Some(t) case _ => None }
				.filter(_.lhs match { case t: ru.Ident => t.name.toString == "priority" case _ => false })
				.map(_.rhs)
				.flatMap { case t: ru.Literal => Some(t.value.value) case _ => None }
				.flatMap { case n: Int => Some(n) case _ => None }
				.find(c => true)
				.getOrElse(0)
		)).toList
	}

	def handlers[T](obj: T, tpe: ru.Type): List[(ru.MethodMirror, ru.Type, Int)] = {
		val m = ru.runtimeMirror(obj.getClass.getClassLoader)
		implicit val ct = ClassTag[T](m.runtimeClass(tpe))
		val im = m.reflect(obj)
		handlers[T](tpe).map(e => (im.reflectMethod(e._1), e._2, e._3))
	}
}
