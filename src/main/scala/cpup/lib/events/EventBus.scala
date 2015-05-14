package cpup.lib.events

import java.lang.reflect.InvocationTargetException

import scala.collection.mutable
import scala.reflect.ClassTag
import scala.reflect.runtime.{universe => ru}

trait EventBus {
	private val handlers = new mutable.HashMap[ru.Type, mutable.Set[(ru.MethodMirror, Int)]]

	def register[T](obj: T)(implicit tt: ru.TypeTag[T]) {
		for((m, tpe, prior) <- EventBus.handlers[T](obj)) {
			handlers.getOrElseUpdate(tpe, new mutable.HashSet[(ru.MethodMirror, Int)]) += ((m, prior))
		}
	}

	def unregister[T](obj: T)(implicit tt: ru.TypeTag[T]) {
		for((m, tpe, prior) <- EventBus.handlers[T](obj)) {
			handlers.get(tpe).foreach(_ -= ((m, prior)))
		}
	}

	def emit[T](e: T)(implicit tt: ru.TypeTag[T]) {
		try {
			for((handler, anno) <- handlers.filter(tt.tpe <:< _._1).flatMap(_._2).toList.sortBy(-_._2)) {
				if(handler.symbol.paramLists.length == 2)
					handler.apply(e, tt)
				else
					handler.apply(e)
			}
		} catch {
			case StopEvent =>
			case ex: InvocationTargetException if ex.getCause == StopEvent =>
		}
	}
}

object EventBus {
	class Base extends EventBus

	def validParams(paramLists: List[List[ru.Symbol]]) = {
		paramLists.length >= 1 && paramLists(0).length == 1 &&
		(if(paramLists.length == 2) {
			paramLists(1).length == 1 &&
			paramLists(1)(0).typeSignature.typeConstructor == ru.typeOf[ru.TypeTag[_]].typeConstructor
		} else {
			paramLists.length == 1
		})
	}

	def handlers[T](implicit tt: ru.TypeTag[T]) = {
		val ehTpe = ru.typeOf[EventHandler]
		for {
			claSym <- tt.tpe.baseClasses
			tpe = claSym.info
			decl <- tpe.decls if decl.isMethod
			sym = decl.asMethod if validParams(sym.paramLists)
			anno <- decl.annotations.find(_.tree.tpe == ehTpe)
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
		)
	}

	def handlers[T](obj: T)(implicit tt: ru.TypeTag[T]): List[(ru.MethodMirror, ru.Type, Int)] = {
		val tpe = tt.tpe
		val m = ru.runtimeMirror(obj.getClass.getClassLoader)
		implicit val ct = ClassTag[T](m.runtimeClass(tpe))
		val im = m.reflect(obj)
		handlers[T](tt).map(e => (im.reflectMethod(e._1), e._2, e._3))
	}
}
