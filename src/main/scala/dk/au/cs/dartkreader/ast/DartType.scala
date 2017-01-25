package dk.au.cs.ast

import java.util

trait DartType extends AstNode

case class NoTypeYet() extends DartType

case class InterfaceType(classNode: AClass,
                         typeArguments: java.util.List[DartType])
    extends DartType

case class TypeParameter(var name: String, var bound: DartType)
    extends DartType

object FunctionType {
  def build(positionalParameters: java.util.List[DartType],
            returnType: DartType,
            typeParameters: java.util.List[TypeParameter],
            requiredParameterCount: Int,
            namedParameters: java.util.List[NamedType]): FunctionType = {
    return new FunctionType(
      positionalParameters = positionalParameters,
      returnType = returnType,
      typeParameters = typeParameters,
      requiredParameterCount = requiredParameterCount,
      namedParameters = namedParameters
    )
  }

  def build(positional: java.util.List[DartType], returnType: DartType) =
    new FunctionType(positionalParameters = positional,
                     returnType = returnType)

}

case class FunctionType(positionalParameters: java.util.List[DartType] =
                          new util.ArrayList[DartType](),
                        returnType: DartType,
                        namedParameters: java.util.List[NamedType] =
                          new util.ArrayList[NamedType](),
                        typeParameters: java.util.List[TypeParameter] =
                          new util.ArrayList[TypeParameter](),
                        requiredParameterCount: Int = 0)
    extends DartType

case class NamedType(var name: String, var ttype: DartType) extends DartType

case class VoidType() extends DartType

case class DynamicType() extends DartType

case class InvalidType() extends DartType

case class TypeName(name: String)

case class BottomType() extends DartType

case class TypeParameterType(positional: TypeParameter) extends DartType

object BaseClassKind extends Enumeration {
  val None, Exact, Subclass, Subtype = Value
}
