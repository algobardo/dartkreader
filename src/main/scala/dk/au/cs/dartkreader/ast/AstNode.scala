package dk.au.cs.ast

import java.util

import scala.collection.JavaConversions._
import scala.collection.mutable

trait AstNode {
  def toCustomString(visited: mutable.Set[AstNode], indentation: Int) = {
    if (visited.contains(this)) this.getClass.getName + "(...)"
    else toString
  }
}

object ProcedureKind extends Enumeration {
  val Method, Getter, Setter, Operator, Factory = Value
}

object AsyncMarker extends Enumeration {
  val Sync, SyncStar, Async, AsyncStar, SyncYielding = Value
}

class LabelId(var id: Int)

object LabelId {
  var id = -1

  def next(): LabelId = {
    id += 1
    new LabelId(id)
  }
}

abstract class TreeNode extends AstNode {
  var fileOffset: Int = -1
}

class Supertype(var classNode: AClass,
                var typeArguments: java.util.List[DartType])
    extends AstNode

object ALibrary {
  def build(): ALibrary = new ALibrary(null, null, null)
}

class ALibrary(
    var fileUri: String,
    var importUri: AUri,
    var name: String,
    var classes: java.util.List[AClass] = new util.ArrayList[AClass](),
    var procedures: java.util.List[AProcedure] =
      new util.ArrayList[AProcedure](),
    var fields: java.util.List[AField] = new util.ArrayList[AField](),
    var isExternal: Boolean = false
) extends TreeNode {

  override def toCustomString(visited: mutable.Set[AstNode],
                              indentation: Int) =
    if (visited.contains(this))
      super.toCustomString(visited, indentation)
    else {
      visited += this
      s"ALibrary(fileUri=$fileUri, " +
        s"importUri=$importUri, " +
        s"name=$name, " +
        s"classes=${classes
          .map(c =>
            if (c == null) "NULL"
            else c.toCustomString(visited, indentation + 1))
          .mkString("\n")}, " +
        s"procedures=${procedures.map(
          _.toCustomString(visited, indentation + 1).mkString("\n"))}, " +
        s"fields=${fields.map(
          _.toCustomString(visited, indentation + 1).mkString("\n"))}, " +
        s"isExternal=$isExternal)"
    }
}

object AClass {
  def build() = new AClass()
}

class AClass(var name: String = null,
             var fileUri: String = null,
             var typeParameters: java.util.List[TypeParameter] =
               new util.ArrayList[TypeParameter](),
             var annotations: java.util.List[AExpression] =
               new util.ArrayList[AExpression](),
             var isAbstract: Boolean = false,
             var level: ClassLevel.Value = ClassLevel.Body,
             var supertype: Supertype = null,
             var mixedInType: Supertype = null,
             var implementedTypes: java.util.List[Supertype] =
               new util.ArrayList[Supertype](),
             var fields: java.util.List[AField] = new util.ArrayList[AField](),
             var constructors: java.util.List[AConstructor] =
               new util.ArrayList[AConstructor](),
             var procedures: java.util.List[AProcedure] =
               new util.ArrayList[AProcedure](),
             var flags: Int = 0)
    extends TreeNode {

  override def toCustomString(visited: mutable.Set[AstNode],
                              indentation: Int) =
    if (visited.contains(this))
      super.toCustomString(visited, indentation)
    else {
      visited += this
      s"AClass(" +
        s"name=$name, " +
        s"fileUri=$fileUri, " +
        s"typeParameters=$typeParameters, " +
        s"annotations=${annotations.map(
          _.toCustomString(visited, indentation + 1).mkString("\n"))}, " +
        s"isAbstract=$isAbstract, " +
        s"level=$level, " +
        s"supertype=${if (supertype != null) supertype.classNode.name else "_"}, " +
        s"mixedInType=${if (mixedInType != null) mixedInType.classNode.name
        else "_"}, " +
        s"implementedTypes=${implementedTypes.map(_.classNode.name).mkString(", ")}, " +
        s"fields=${fields.map(_.toCustomString(visited, indentation + 1)).mkString("\n")}, " +
        s"constructors=${constructors.map(_.toCustomString(visited, indentation + 1)).mkString("\n")}, " +
        s"procedures=${procedures.map(_.toCustomString(visited, indentation + 1)).mkString("\n")}, " +
        s"flags=$flags)"
    }
}

abstract class AMember extends TreeNode {
  def name: AName

  def annotations: java.util.List[AExpression]
}

class AField(var name: AName,
             var annotations: java.util.List[AExpression] =
               new util.ArrayList[AExpression](),
             var ttype: DartType = DynamicType(),
             var initializer: AExpression = null,
             var inferredValue: AInferredValue = null,
             var flags: Int = 0,
             var fileUri: String = null,
             var transformerFlags: Int = 0)
    extends AMember

object AField {
  val FlagFinal = 1 << 0
  // Must match serialized bit positions.
  val FlagConst = 1 << 1
  val FlagStatic = 1 << 2

  def isFinal(flag: Int) = (flag & FlagFinal) != 0

  def isConst(flag: Int) = (flag & FlagConst) != 0

  def isStatic(flag: Int) = (flag & FlagStatic) != 0

  def build(name: AName) = new AField(name: AName)
}

object AConstructor {
  def build(function: AFunctionNode): AConstructor = new AConstructor(function)
}

class AConstructor(var function: AFunctionNode,
                   var flags: Int = 0,
                   var transformerFlags: Int = 0,
                   var name: AName = null,
                   var annotations: java.util.List[AExpression] =
                     new util.ArrayList[AExpression](),
                   var initializers: java.util.List[AInitializer] =
                     new util.ArrayList[AInitializer]())
    extends AMember {}

object AProcedure {
  val FlagStatic = 1 << 0;
  // Must match serialized bit positions.
  val FlagAbstract = 1 << 1
  val FlagExternal = 1 << 2
  val FlagConst = 1 << 3;

  // Only for external const factories.

  def build(name: AName, kind: ProcedureKind.Value, function: AFunctionNode) =
    new AProcedure(name, kind, function)
}

class AProcedure(var name: AName,
                 var kind: ProcedureKind.Value,
                 var function: AFunctionNode,
                 var annotations: java.util.List[AExpression] =
                   new util.ArrayList[AExpression](),
                 var flags: Int = 0,
                 var fileUri: String = null,
                 var transformerFlags: Int = 0)
    extends AMember {

  def isStatic = (flags & AProcedure.FlagStatic) != 0

  def isAbstract = (flags & AProcedure.FlagAbstract) != 0

  def isExternal = (flags & AProcedure.FlagExternal) != 0

  def isConst = (flags & AProcedure.FlagConst) != 0

  def isAbstract_=(b: Boolean) = {
    flags =
      if (b) (flags | AProcedure.FlagAbstract)
      else (flags & ~AProcedure.FlagAbstract);
  }

}

class AInvalidInitializer() extends AInitializer

class AInvalidExpression() extends AExpression

class AInvalidStatement() extends AStatement

trait AInitializer extends TreeNode

class FieldInitializer(var field: AMember, var value: AExpression)
    extends AInitializer

class SuperInitializer(var target: AMember, var arguments: AArguments)
    extends AInitializer

class RedirectingInitializer(var target: AMember, var arguments: AArguments)
    extends AInitializer

class ALocalInitializer(var variable: AVariableDeclaration)
    extends AInitializer

class AFunctionNode(
    var body: AStatement,
    var typeParameters: java.util.List[TypeParameter],
    var requiredParameterCount: Int,
    var positionalParameters: java.util.List[AVariableDeclaration],
    var namedParameters: java.util.List[AVariableDeclaration],
    var returnType: DartType,
    var inferredReturnValue: AInferredValue,
    var asyncMarker: AsyncMarker.Value
) extends TreeNode

trait AExpression extends TreeNode

class AVariableGet(var variable: AVariableDeclaration,
                   var promotedType: DartType)
    extends AExpression

class AVariableSet(var variable: AVariableDeclaration, var value: AExpression)
    extends AExpression

class APropertyGet(var receiver: AExpression,
                   var name: AName,
                   var interfaceTarget: AMember)
    extends AExpression

class APropertySet(var receiver: AExpression,
                   var name: AName,
                   var value: AExpression,
                   var interfaceTarget: AMember)
    extends AExpression

class ADirectPropertyGet(var receiver: AExpression, var target: AMember)
    extends AExpression

class ADirectPropertySet(var receiver: AExpression,
                         var target: AMember,
                         var value: AExpression)
    extends AExpression

class ADirectMethodInvocation(
    var receiver: AExpression,
    var target: AMember,
    var arguments: AArguments
) extends AExpression

class ASuperPropertyGet(var name: AName, var interfaceTarget: AMember)
    extends AExpression

class ASuperPropertySet(var name: AName,
                        var value: AExpression,
                        var interfaceTarget: AMember)
    extends AExpression

class AStaticGet(var target: AMember) extends AExpression

class AStaticSet(var target: AMember, var value: AExpression)
    extends AExpression

class AArguments(var positional: java.util.List[AExpression],
                 var types: java.util.List[DartType],
                 var named: java.util.List[ANamedExpression])
    extends TreeNode

class ANamedExpression(var name: String, var value: AExpression)
    extends TreeNode

abstract class AInvocationExpression() extends AExpression

class AMethodInvocation(var receiver: AExpression,
                        var name: AName,
                        var arguments: AArguments,
                        var interfaceTarget: AMember)
    extends AInvocationExpression

class ASuperMethodInvocation(var name: AName,
                             var arguments: AArguments,
                             var interfaceTarget: AMember)
    extends AInvocationExpression

class AStaticInvocation(var target: AMember,
                        var arguments: AArguments,
                        var isConst: Boolean)
    extends AInvocationExpression

class ConstructorInvocation(var target: AMember,
                            var arguments: AArguments,
                            var isConst: Boolean)
    extends AInvocationExpression

class Not(var operand: AExpression) extends AExpression

class ALogicalExpression(var left: AExpression,
                         var op: String,
                         var right: AExpression)
    extends AExpression

class AConditionalExpression(var condition: AExpression,
                             var tthen: AExpression,
                             var otherwise: AExpression,
                             var staticType: DartType)
    extends AExpression

class StringConcatenation(var expressions: java.util.List[AExpression])
    extends AExpression

class IsExpression(var operand: AExpression, var ttype: DartType)
    extends AExpression

class AsExpression(var operand: AExpression, var ttype: DartType)
    extends AExpression

trait ABasicLiteral extends AExpression

class AStringLiteral(var value: String) extends ABasicLiteral

class AIntLiteral(var value: BigInt) extends ABasicLiteral

class ADoubleLiteral(var value: Double) extends ABasicLiteral

class ABooleanLiteral(var value: Boolean) extends ABasicLiteral

class ANullLiteral() extends ABasicLiteral

class ASymbolLiteral(var value: String) extends AExpression

class TypeLiteral(var ttype: DartType) extends AExpression

class ThisExpression() extends AExpression

class Rethrow() extends AExpression

class Throw(var expression: AExpression) extends AExpression

class ListLiteral(var expressions: java.util.List[AExpression],
                  var typeArgument: DartType,
                  var isConst: Boolean)
    extends AExpression

class MapLiteral(var entries: java.util.List[MapEntry],
                 var keyType: DartType,
                 var valueType: DartType,
                 var isConst: Boolean)
    extends AExpression

class MapEntry(
    var key: AExpression,
    var value: AExpression
) extends TreeNode

class AwaitExpression(var operand: AExpression) extends AExpression

class FunctionExpression(var function: AFunctionNode) extends AExpression

class Let(var variable: AVariableDeclaration, var body: AExpression)
    extends AExpression

class BlockExpression(var body: ABlock, var value: AExpression)
    extends AExpression

trait AStatement extends TreeNode

class AExpressionStatement(var expression: AExpression) extends AStatement

class ABlock(var statements: java.util.List[AStatement]) extends AStatement

class AEmptyStatement() extends AStatement

class AssertStatement(var condition: AExpression, var message: AExpression)
    extends AStatement

class LabeledStatement(var body: AStatement) extends AStatement

class BreakStatement(var target: LabeledStatement) extends AStatement

class WhileStatement(var condition: AExpression, var body: AStatement)
    extends AStatement

class DoStatement(var body: AStatement, var condition: AExpression)
    extends AStatement

class ForStatement(var variables: java.util.List[AVariableDeclaration],
                   var condition: AExpression,
                   var updates: java.util.List[AExpression],
                   var body: AStatement)
    extends AStatement

class ForInStatement(var variable: AVariableDeclaration,
                     var iterable: AExpression,
                     var body: AStatement,
                     var isAsync: Boolean)
    extends AStatement

class SwitchStatement(var expression: AExpression,
                      var cases: java.util.List[SwitchCase])
    extends AStatement

object SwitchCase {
  def build(): SwitchCase = new SwitchCase()
}

class SwitchCase(var expressions: java.util.List[AExpression] =
                   new util.ArrayList[AExpression](),
                 var body: AStatement = null,
                 var isDefault: Boolean = false)
    extends TreeNode

class ContinueSwitchStatement(var switchCase: SwitchCase) extends AStatement

class IfStatement(var condition: AExpression,
                  var tthen: AStatement,
                  var otherwise: AStatement)
    extends AStatement

class ReturnStatement(var expression: AExpression) extends AStatement

class TryCatch(var body: AStatement, var catches: java.util.List[ACatch])
    extends AStatement

class ACatch(var exception: AVariableDeclaration,
             var body: AStatement,
             var guard: DartType = DynamicType(),
             var stackTrace: AVariableDeclaration)
    extends TreeNode

class TryFinally(var body: AStatement, var finalizer: AStatement)
    extends AStatement

object YieldStatement {
  val FlagYieldStar = 1 << 0
  val FlagNative = 1 << 1
}

class YieldStatement(var expression: AExpression,
                     var isYieldStar: Boolean,
                     var isNative: Boolean)
    extends AStatement

object AVariableDeclaration {
  val FlagFinal = 1 << 0
  // Must match serialized bit positions.
  val FlagConst = 1 << 1
  val FlagInScope = 1 << 2 // Temporary flag used by verifier.

}

class AVariableDeclaration(
    var name: String,
    var ttype: DartType,
    var inferredValue: AInferredValue,
    var initializer: AExpression,
    ffinal: Boolean,
    cconst: Boolean
) extends AStatement {

  var flags = 0

  isFinal = ffinal
  isConst = cconst

  def isFinal = (flags & AVariableDeclaration.FlagFinal) != 0

  def isConst = (flags & AVariableDeclaration.FlagConst) != 0

  def isFinal_=(b: Boolean) = {
    flags =
      if (b) (flags | AVariableDeclaration.FlagFinal)
      else (flags & ~AVariableDeclaration.FlagFinal);
  }

  def isConst_=(b: Boolean) = {
    flags =
      if (b) (flags | AVariableDeclaration.FlagConst)
      else (flags & ~AVariableDeclaration.FlagConst);
  }

}

class FunctionDeclaration(var variable: AVariableDeclaration,
                          var function: AFunctionNode)
    extends AStatement

class AName(var name: String, var library: ALibrary) extends AstNode

class Program(var libraries: java.util.List[ALibrary],
              var mainMethod: AMember,
              var uriToLineStarts: java.util.HashMap[String, Array[Integer]])
    extends TreeNode {

  override def toCustomString(visited: mutable.Set[AstNode],
                              indentation: Int) =
    s"Program(" +
      s"libraries=${libraries.map(_.toCustomString(visited, indentation + 1))}, " +
      s"mainMethod=${mainMethod.toCustomString(visited, indentation + 1)})"
}

case class AUri(value: String)

class AInferredValue(
    var baseClass: AClass,
    var baseClassKind: BaseClassKind.Value,
    var valueBits: Int
) extends TreeNode
