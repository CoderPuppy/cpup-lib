package cpup.lib.module

import java.lang.annotation.Annotation

import com.typesafe.config.Config
import org.slf4j.Logger

import scala.collection.mutable

object ModuleLoader {
	// annotation => reasons to not load
	final val canLoadHandlers = new mutable.HashMap[Class[_ <: Annotation], (_ <: Annotation) => List[String]]
	final val modulesByInst = new mutable.HashMap[AnyRef, TModule[_ <: AnyRef]]
	private final val modulesBySpec = new mutable.HashMap[ModuleSpec[_ <: AnyRef], TModule[_ <: AnyRef]]
	private final val moduleTypes = new mutable.HashMap[Class[_ <: AnyRef], ModuleType[_ <: AnyRef]]

	canLoadHandlers(classOf[CanLoad.ClassAvailable]) = ((a: CanLoad.ClassAvailable) => {
		try {
			val con = a.clazz.getConstructors()(0)
			con.newInstance(Array.ofDim(con.getParameterTypes.length) { null })
			List()
		} catch {
			case e: TypeNotPresentException =>
				List(e.getMessage)
		}
	}).asInstanceOf[(Annotation) => List[String]] // it does compile without this, but intellij doesn't like it

	def moduleType[T <: AnyRef](implicit cla: Class[T]) = {
		moduleTypes.getOrElseUpdate(cla, new ModuleType[T]).asInstanceOf[ModuleType[T]]
	}

	def getImpls[T <: AnyRef](spec: ModuleSpec[_ <: T]): List[ModuleSpec[_ <: T]] = {
		val cla = spec.typ.cla
		val candidates = mutable.Queue[ModuleSpec[_ <: T]](spec)
		var results = mutable.ListBuffer[ModuleSpec[_ <: T]]()
		while(!candidates.isEmpty) {
			val candidate = candidates.dequeue
			candidate.typ.cla.getAnnotation(classOf[MultiModule]) match {
				case null => results += candidate
				case anno => candidates.enqueue(
					anno.clazzs
						.filter(cla.isAssignableFrom)
						.asInstanceOf[Array[Class[T]]]
						.map((cla) => {
							ModuleSpec(ModuleType()(cla), candidate)
						}): _*
				)
			}
		}
		results.toList
	}

	def load[T <: AnyRef](parent: AnyRef)(implicit cla: Class[T]): Either[T, List[ModuleSpec[_ <: T]]] = {
		val parModule = modulesByInst(parent)
		val spec = ModuleSpec[T](ModuleType[T]()(cla), parModule.spec)
		val impls = getImpls[T](spec)
		var reasons = mutable.ListBuffer[ModuleSpec[_ <: T]]()
		for(impl <- impls) {
			if(modulesBySpec.contains(impl)) {
				spec.logger.info("using {}", impl.id)
				return Left(modulesBySpec(impl).inst.asInstanceOf[T])
			} else {
				if(impl.reasons.isEmpty) {
					val inst = impl.typ.cla.getConstructor(classOf[Config], classOf[Logger]).newInstance(impl.config, impl.logger)
					val module = new Module[T](impl, inst)
					modulesByInst(inst) = module
					modulesBySpec(impl) = module
					impl.logger.info("loading for {}", spec.id)
					spec.logger.info("using {}", impl.id)
					return Left(inst)
				} else {
					impl.logger.info("can't load {} because {}", impl.typ.cla.getName: Any, impl.reasons.mkString(", "))
					reasons += impl
				}
			}
		}
		Right(reasons.toList)
	}

	def forceLoad[T <: AnyRef](parent: AnyRef)(implicit cla: Class[T]): T = load[T](parent) match {
		case Left(m) => m
		case Right(impls) => {
			val typ = moduleType[T](cla)
			val parModule = modulesByInst(parent)
			val prefix = s"${parModule.id}/${typ.id}"
			throw new RuntimeException(s"cannot load ${typ.id}, options: ${
				impls.map(impl => s"${
					val rem = impl.id.substring(prefix.length)
					if(rem.length == 0)
						"."
					else
						rem.substring(1)
				} (${
					impl.reasons.mkString(", ")
				})").mkString(" | ")
			}")
		}
	}
}
