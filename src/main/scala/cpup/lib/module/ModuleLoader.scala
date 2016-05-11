package cpup.lib.module

import java.lang.annotation.Annotation

import scala.collection.mutable.ListBuffer
import scala.language.existentials

import com.typesafe.config.Config
import org.slf4j.Logger

import scala.collection.mutable

object ModuleLoader {
	// annotation => reasons to not load
	final val canLoadHandlers = new mutable.HashMap[Class[_ <: Annotation], (ModuleSpec[_], _ <: Annotation) => List[String]]
	final val modulesByInst = new mutable.HashMap[AnyRef, TModule[_ <: AnyRef]]
	private final val modulesBySpec = new mutable.HashMap[ModuleSpec[_ <: AnyRef], TModule[_ <: AnyRef]]
	private final val moduleTypes = new mutable.HashMap[Class[_ <: AnyRef], ModuleType[_ <: AnyRef]]

	canLoadHandlers(classOf[CanLoad.ClassAvailable]) = ((spec: ModuleSpec[_], a: CanLoad.ClassAvailable) => {
		try {
			val con = a.clazz.getDeclaredConstructors()(0)
			con.setAccessible(true)
			con.newInstance(Array.ofDim(con.getParameterTypes.length) { null })
			List()
		} catch {
			case e: TypeNotPresentException =>
				List(e.getMessage)
		}
	}).asInstanceOf[(ModuleSpec[_], Annotation) => List[String]] // it does compile without this, but intellij doesn't like it

	canLoadHandlers(classOf[CanLoad.ModuleTypeAvailable]) = ((spec: ModuleSpec[_], a: CanLoad.ModuleTypeAvailable) => {
		ModuleSpec(moduleType[AnyRef](a.moduleType.asInstanceOf[Class[AnyRef]]), spec.asInstanceOf[ModuleSpec[_ <: AnyRef]]).reasons
	}).asInstanceOf[(ModuleSpec[_], Annotation) => List[String]] // it does compile without this, but intellij doesn't like it

	def providedModule[T <: AnyRef](start: ModuleSpec[_ <: AnyRef], typ: ModuleType[T]): (Option[ModuleSpec[T]], List[ModuleSpec[_ <: T]]) = {
		var spec = start
		val tries = ListBuffer[ModuleSpec[_ <: T]]()
		while(spec.parent != spec) {
			val checkSpec = ModuleSpec(typ, spec)
			tries += checkSpec
			if(modulesBySpec.contains(checkSpec)) {
				return (Some(checkSpec), tries.toList)
			}
			spec = spec.parent
		}
		return (None, tries.toList)
	}

	canLoadHandlers(classOf[CanLoad.ModuleProvided]) = ((_spec: ModuleSpec[_ <: AnyRef], a: CanLoad.ModuleProvided) => {
		val (spec, tries) = providedModule(_spec, moduleType(a.moduleType.asInstanceOf[Class[_ <: AnyRef]]))
		spec.map((v) => List()).getOrElse(tries.flatMap((spec) => {
			spec.id :: spec.reasons.map(" - " + _)
		}))
	}).asInstanceOf[(ModuleSpec[_], Annotation) => List[String]] // it does compile without this, but intellij doesn't like it

	def moduleType[T <: AnyRef](cla: Class[T]) = moduleTypes.getOrElseUpdate(cla, new ModuleType[T](cla)).asInstanceOf[ModuleType[T]]
	def moduleType[T <: AnyRef](implicit manifest: Manifest[T]): ModuleType[T] = moduleType[T](manifest.runtimeClass.asInstanceOf[Class[T]])

	def getImpls[T <: AnyRef](spec: ModuleSpec[_ <: T]): List[ModuleSpec[_ <: T]] = {
		val cla = spec.typ.cla
		val candidates = mutable.Queue[ModuleSpec[_ <: T]](spec)
		var results = mutable.ListBuffer[ModuleSpec[_ <: T]]()
		while(!candidates.isEmpty) {
			val candidate = candidates.dequeue
			candidate.typ.multi match {
				case None => results += candidate
				case Some(clas) => candidates.enqueue(clas.map((cla) => {
					ModuleSpec(ModuleType(cla), candidate)
				}): _*)
			}
		}
		results.toList
	}

	def provided[T <: AnyRef](self: AnyRef)(implicit manifest: Manifest[T]): Either[T, List[ModuleSpec[_ <: T]]] = {
		val (spec, tries) = providedModule[T](modulesByInst(self).spec, moduleType[T])
		spec match {
			case Some(spec) => Left(modulesBySpec(spec).inst.asInstanceOf[T])
			case _ => Right(tries)
		}
	}

	def forceProvided[T <: AnyRef](self: AnyRef)(implicit manifest: Manifest[T]): T = provided[T](self) match {
		case Left(m) => m
		case Right(impls) => {
			val typ = moduleType[T]
			val parModule = modulesByInst(self)
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

	def load[T <: AnyRef](parent: AnyRef, provide: Boolean = false)(implicit manifest: Manifest[T]): Either[T, List[ModuleSpec[_ <: T]]] = {
		val parModule = modulesByInst(parent)
		val spec = ModuleSpec[T](moduleType[T], parModule.spec, provide)
		val impls = getImpls[T](spec)
		var reasons = mutable.ListBuffer[ModuleSpec[_ <: T]]()
		for(impl <- impls) {
			if(modulesBySpec.contains(impl)) {
				spec.logger.info("using {}", impl.id)
				return Left(modulesBySpec(impl).inst.asInstanceOf[T])
			} else {
				spec.logger.info("trying {}", impl.id)
				if(impl.reasons.isEmpty) {
					val inst = impl.constructor.get.newInstance(impl.constructor.get.getParameterTypes.map(c => {
						if(c.isAssignableFrom(classOf[Logger]))
							spec.logger
						else if(c.isAssignableFrom(classOf[Config]))
							spec.config
						else {
							providedModule(impl, moduleType(c.asInstanceOf[Class[_ <: AnyRef]])) match {
								case (Some(spec), _) => modulesBySpec(spec).inst
								case _ => throw new RuntimeException(s"invalid parameter in constructor: ${c.getName}")
							}
						}
					}): _*).asInstanceOf[T]
					val module = new Module[T](impl, inst)
					modulesByInst(inst) = module
					impl.parents.tail.takeWhile(_.typ.multi.isDefined).foreach(modulesBySpec(_) = module)
					modulesBySpec(impl) = module
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

	def forceLoad[T <: AnyRef](parent: AnyRef, provide: Boolean = false)(implicit manifest: Manifest[T]): T = load[T](parent, provide = provide) match {
		case Left(m) => m
		case Right(impls) => {
			val typ = moduleType[T]
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
