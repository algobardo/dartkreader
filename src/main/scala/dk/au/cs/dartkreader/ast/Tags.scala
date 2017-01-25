package dk.au.cs.ast

/**
  * Rewrite from kernel ref: d9197c5d
  */
case class ParseError(message: String,
                      byteIndex: Int,
                      filename: String,
                      path: String)
    extends RuntimeException(message)

object AnyTag extends Enumeration {
  val Nothing = Value(0)
  val Something = Value(1)

  val BottomType = Value(89)
  val InvalidType = Value(90)
  val DynamicType = Value(91)
  val VoidType = Value(92)
  val InterfaceType = Value(93)
  val FunctionType = Value(94)
  val TypeParameterType = Value(95)
  val SimpleInterfaceType = Value(96)
  val SimpleFunctionType = Value(97)

  val NormalClass = Value(2)
  val MixinClass = Value(3)

  val Field = Value(4)
  val Constructor = Value(5)
  val Procedure = Value(6)

  val InvalidInitializer = Value(7)
  val FieldInitializer = Value(8)
  val SuperInitializer = Value(9)
  val RedirectingInitializer = Value(10)
  val LocalInitializer = Value(11)

  val DirectPropertyGet = Value(15)
  val DirectPropertySet = Value(16)
  val DirectMethodInvocation = Value(17)
  val ConstStaticInvocation = Value(18)
  val InvalidExpression = Value(19)
  val VariableGet = Value(20)
  val VariableSet = Value(21)
  val PropertyGet = Value(22)
  val PropertySet = Value(23)
  val SuperPropertyGet = Value(24)
  val SuperPropertySet = Value(25)
  val StaticGet = Value(26)
  val StaticSet = Value(27)
  val MethodInvocation = Value(28)
  val SuperMethodInvocation = Value(29)
  val StaticInvocation = Value(30)
  val ConstructorInvocation = Value(31)
  val ConstConstructorInvocation = Value(32)
  val Not = Value(33)
  val LogicalExpression = Value(34)
  val ConditionalExpression = Value(35)
  val StringConcatenation = Value(36)
  val IsExpression = Value(37)
  val AsExpression = Value(38)
  val StringLiteral = Value(39)
  val DoubleLiteral = Value(40)
  val TrueLiteral = Value(41)
  val FalseLiteral = Value(42)
  val NullLiteral = Value(43)
  val SymbolLiteral = Value(44)
  val TypeLiteral = Value(45)
  val ThisExpression = Value(46)
  val Rethrow = Value(47)
  val Throw = Value(48)
  val ListLiteral = Value(49)
  val MapLiteral = Value(50)
  val AwaitExpression = Value(51)
  val FunctionExpression = Value(52)
  val Let = Value(53)
  val BlockExpression = Value(54)
  val PositiveIntLiteral = Value(55)
  val NegativeIntLiteral = Value(56)
  val BigIntLiteral = Value(57)
  val ConstListLiteral = Value(58)
  val ConstMapLiteral = Value(59)

  val NullReference = Value(99)
  val NormalClassReference = Value(100)
  val MixinClassReference = Value(101)

  val LibraryFieldReference = Value(102)
  val ClassFieldReference = Value(103)
  val ClassConstructorReference = Value(104)
  val LibraryProcedureReference = Value(105)
  val ClassProcedureReference = Value(106)

  val SpecializedVariableGet = Value(128)
  val SpecializedVariableSet = Value(136)
  val SpecializedIntLiteral = Value(144)

  val InvalidStatement = Value(60)
  val ExpressionStatement = Value(61)
  val Block = Value(62)
  val EmptyStatement = Value(63)
  val AssertStatement = Value(64)
  val LabeledStatement = Value(65)
  val BreakStatement = Value(66)
  val WhileStatement = Value(67)
  val DoStatement = Value(68)
  val ForStatement = Value(69)
  val ForInStatement = Value(70)
  val SwitchStatement = Value(71)
  val ContinueSwitchStatement = Value(72)
  val IfStatement = Value(73)
  val ReturnStatement = Value(74)
  val TryCatch = Value(75)
  val TryFinally = Value(76)
  val YieldStatement = Value(77)
  val VariableDeclaration = Value(78)
  val FunctionDeclaration = Value(79)
  val AsyncForInStatement = Value(80)
}

object OtherEnum extends Enumeration {
  val SpecializedIntLiteralBias = Value(3)
}

object Masks extends Enumeration {
  val SpecializedTagHighBit = Value(0x80)
  val SpecializedTagMask = Value(0xF8)
  val SpecializedPayloadMask = Value(0x7)
}

object MagicWordTag extends Enumeration {
  val LibraryFile = Value(0x12345678)
  val ProgramFile = Value(0x90ABCDEF)
}

object TransformerFlag extends Enumeration {
  val superCalls = Value(1 << 0)
}

object LogicalOperatorTag extends Enumeration {
  val EE = Value(0)
  val OO = Value(1)
  val QQ = Value(2)
}

object ClassLevel extends Enumeration {
  val Temporary, Type, Hierarchy, Body = Value
}
