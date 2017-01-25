package dk.au.cs.dartkreader.loader;

import scala.math.BigInt;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import dk.au.cs.ast.*;
/**
 * Copy pasted and modified from ast_from_binary.dart in the kernel project at tagget SDK release 1.21.1.
 */
public class BinaryBuilder {
    interface NodeBuilder<T> {
        T build();
    }

    interface TagIndexObjectBuilder<T> {
        T build(int tag);
    }


    interface TagIndexObjectFiller<T> {
        void fill(T n);
    }


    private final BinaryLoader loader;
    private final List<ALibrary> importTable = new ArrayList<>();
    private final List<AVariableDeclaration> variableStack = new ArrayList<>();
    private final List<LabeledStatement> labelStack = new ArrayList<>();
    private int labelStackBase = 0;

    private final List<SwitchCase> switchCaseStack = new ArrayList<>();
    private final List<TypeParameter> typeParameterStack = new ArrayList<>();
    private final String filename;
    private final byte[] _bytes;
    private int _byteIndex = 0;

    private ALibrary _currentLibrary;
    private List<String> _stringTable;
    private List<String> _sourceUriTable;
    private int _transformerFlags = 0;

    public BinaryBuilder(BinaryLoader loader, byte[] _bytes, String filename) {
        this.loader = loader;
        this._bytes = _bytes;
        this.filename = filename;
    }


    private ParseError fail(String message) {
        return new ParseError(message, _byteIndex, filename, "");
    }

    private void setLength(List l, int newSize) {
        for (int i = l.size() - 1; i >= newSize; i--)
            l.remove(i);
        for (int i = l.size(); i < newSize; i++)
            l.add(null);
    }

    private <T> List<T> mkList(int size) {
        ArrayList l = new ArrayList<>(size);
        for (int i = 0; i < size; i++)
            l.add(null);
        return l;
    }

    private int readByte() {
        return _bytes[_byteIndex++] & 0xFF; // From unsigned to signed
    }

    private int readUInt() {
        int bbyte = readByte();
        if ((bbyte & 0x80) == 0) {
            // 0xxxxxxx
            return bbyte;
        } else if ((bbyte & 0x40) == 0) {
            // 10xxxxxx
            return ((bbyte & 0x3F) << 8) | readByte();
        } else {
            // 11xxxxxx
            return ((bbyte & 0x3F) << 24) |
                    (readByte() << 16) |
                    (readByte() << 8) |
                    readByte();
        }
    }

    private int readMagicWord() {
        return (readByte() << 24) |
                (readByte() << 16) |
                (readByte() << 8) |
                readByte();
    }

    private String readStringEntry() {
        int numBytes = readUInt();
        // Utf8Decoder will skip leading BOM characters, but we must preserve them.
        // Collect leading BOMs before passing the bytes onto Utf8Decoder.
        int numByteOrderMarks = 0;
        while (_byteIndex + 2 < _bytes.length &&
                _bytes[_byteIndex] == 0xef &&
                _bytes[_byteIndex + 1] == 0xbb &&
                _bytes[_byteIndex + 2] == 0xbf) {
            ++numByteOrderMarks;
            _byteIndex += 3;
            numBytes -= 3;
        }
        String string = new String(_bytes, _byteIndex, numBytes);
        _byteIndex += numBytes;
        if (numByteOrderMarks > 0) {
            return '\ufeff' * numByteOrderMarks + string;
        }
        return string;
    }

    private void readStringTable() {
        int length = readUInt();
        _stringTable = mkList(length);
        for (int i = 0; i < length; ++i) {
            _stringTable.set(i, readStringEntry());
        }
    }

    private String readUriReference() {
        return _sourceUriTable.get(readUInt());
    }

    private void readSourceUriTable() {
        int length = readUInt();
        _sourceUriTable = mkList(length);
        for (int i = 0; i < length; ++i) {
            _sourceUriTable.set(i, readStringEntry());
        }
    }

    private String readStringReference() {
        return _stringTable.get(readUInt());
    }

    private String readStringOrNullIfEmpty() {
        String string = readStringReference();
        return string.isEmpty() ? null : string;
    }

    private AInferredValue readOptionalInferredValue() {
        if (readAndCheckOptionTag()) {
            AClass baseClass = readClassReference(true);
            BaseClassKind$.Value baseClassKind = BaseClassKind.apply(readByte());
            int valueBits = readByte();
            return new AInferredValue(baseClass, baseClassKind, valueBits);
        }
        return null;
    }

    private boolean readAndCheckOptionTag() {
        AnyTag$.Value tag = AnyTag.apply(readByte());
        if (tag == AnyTag.Nothing()) {
            return false;
        } else if (tag == AnyTag.Something()) {
            return true;
        } else {
            throw fail("Invalid Option tag: " + tag);
        }
    }

    private List<AExpression> readAnnotationList(TreeNode parent) {
        int length = readUInt();
        if (length == 0) return new ArrayList<>();
        List<AExpression> list = mkList(length);
        for (int i = 0; i < length; ++i) {
            list.set(i, readExpression());
        }
        return list;
    }

    private <T> void _fillTreeNodeList(
            List<T> list, NodeBuilder<T> objBuilder, TreeNode parent) {
        int amount = readUInt();
        setLength(list, amount);
        for (int i = 0; i < amount; ++i) {
            T tn = objBuilder.build();
            list.set(i, tn);
        }
    }

    private <T> void _fillNonTreeNodeList(List<T> list, NodeBuilder<T> buildObject) {
        int amount = readUInt();
        setLength(list, amount);
        for (int i = 0; i < amount; ++i) {
            list.set(i, buildObject.build());
        }
    }

    public Program readProgramFile() {
        MagicWordTag$.Value magic = MagicWordTag.apply(readMagicWord());
        if (magic != MagicWordTag.ProgramFile()) {
            throw fail("This is not a binary dart file. \nMagic number was: " + Integer.toHexString(magic.id()));
        }
        readStringTable();

        HashMap<String, Integer[]> uriToLineStarts = readUriToLineStarts();
        int importTableLength = readUInt();
        setLength(importTable, importTableLength);
        for (int i = 0; i < importTableLength; ++i) {
            importTable.set(i, ALibrary.build());
        }
        for (int i = 0; i < importTableLength; ++i) {
            _currentLibrary = importTable.get(i);
            readLibrary();
        }
        AMember mainMethod = readMemberReference(true);
        return new Program(importTable, mainMethod, uriToLineStarts);
    }

    private HashMap<String, Integer[]> readUriToLineStarts() {
        readSourceUriTable();
        int length = _sourceUriTable.size();
        HashMap<String, Integer[]> uriToLineStarts = new HashMap<>();
        for (int i = 0; i < length; ++i) {
            String uri = _sourceUriTable.get(i);
            int lineCount = readUInt();
            Integer[] lineStarts = new Integer[lineCount];
            int previousLineStart = 0;
            for (int j = 0; j < lineCount; ++j) {
                int lineStart = readUInt() + previousLineStart;
                lineStarts[j] = lineStart;
                previousLineStart = lineStart;
            }
            uriToLineStarts.put(uri, lineStarts);
        }
        return uriToLineStarts;
    }

    private <T> void _fillLazilyLoadedList(List<T> list, TagIndexObjectBuilder<T> buildObject) {
        int length = readUInt();
        setLength(list, length);
        for (int i = 0; i < length; ++i) {
            int tag = readByte();
            list.set(i, buildObject.build(tag));
        }
    }

    private ALibrary readLibraryReference() {
        int index = readUInt();
        return importTable.get(index);
    }

    private AClass readClassReference(boolean allowNull) {
        AnyTag$.Value tag = AnyTag.apply(readByte());
        if (tag == AnyTag.NullReference()) {
            if (!allowNull) {
                throw new RuntimeException("Expected a class reference to be valid but was `null`.");
            }
            return null;
        } else {
            ALibrary library = readLibraryReference();
            int index = readUInt();
            return loader.getClassReference(library, tag.id(), index);
        }
    }

    private AMember readMemberReference(boolean allowNull) {
        AnyTag$.Value tag = AnyTag.apply(readByte());
        if (tag == AnyTag.LibraryFieldReference() || tag == AnyTag.LibraryProcedureReference()) {
            ALibrary library = readLibraryReference();
            int index = readUInt();
            return loader.getLibraryMemberReference(library, tag.id(), index);
        } else if (tag == AnyTag.ClassFieldReference() || tag == AnyTag.ClassConstructorReference() || tag == AnyTag.ClassProcedureReference()) {
            AClass classNode = readClassReference(false);
            int index = readUInt();
            return loader.getClassMemberReference(classNode, tag.id(), index);
        } else if (tag == AnyTag.NullReference()) {
            if (!allowNull) {
                throw new RuntimeException("Expected a member reference to be valid but was `null`.");
            }
            return null;
        } else {
            throw fail("Invalid member reference tag: " + tag);
        }
    }

    private AName readName() {
        String text = readStringReference();
        if (!text.isEmpty() && text.startsWith("_")) {
            return new AName(text, readLibraryReference());
        } else {
            return new AName(text, null);
        }
    }

    private AUri readImportUri() {
        return new AUri(readStringReference());
    }

    private void readLibrary() {
        int flags = readByte();
        _currentLibrary.isExternal_$eq((flags & 0x1) != 0);
        _currentLibrary.name_$eq(readStringOrNullIfEmpty());
        _currentLibrary.importUri_$eq(readImportUri());

        // TODO(jensj): We currently save (almost the same) uri twice.
        _currentLibrary.fileUri_$eq(readUriReference());

        _fillLazilyLoadedList(_currentLibrary.classes(), (int tag) -> readClass(AClass.build(), tag));
        _fillLazilyLoadedList(_currentLibrary.fields(), (int tag) -> readField(AField.build(null), tag));
        _fillLazilyLoadedList(_currentLibrary.procedures(), (int tag) -> readProcedure(AProcedure.build(null, null, null), tag));
    }

    private AClass readClass(AClass node, int tag) {
        assert (node != null);
        AnyTag$.Value t = AnyTag.apply(tag);
        if (t == AnyTag.NormalClass()) {
            readNormalClass(node);
        } else if (t == AnyTag.MixinClass()) {
            readMixinClass(node);
        } else {
            throw fail("Invalid class tag: " + tag);
        }
        return node;
    }

    private void readNormalClass(AClass node) {
        int flags = readByte();
        node.isAbstract_$eq((flags & 0x1) != 0);
        node.level_$eq(
                _currentLibrary.isExternal()
                        ?
                        ((flags & 0x2) != 0) ?
                                ClassLevel.Type()
                                : ClassLevel.Hierarchy()
                        : ClassLevel.Body());
        node.name_$eq(readStringOrNullIfEmpty());
        node.fileUri_$eq(readUriReference());
        node.annotations_$eq(readAnnotationList(node));

        readAndPushTypeParameterList(node.typeParameters(), node);
        node.supertype_$eq(readSupertypeOption());

        _fillNonTreeNodeList(node.implementedTypes(), () -> readSupertype());
        _fillLazilyLoadedList(node.fields(), (int tag) -> readField(AField.build(null), tag));
        _fillLazilyLoadedList(node.constructors(), (int tag) -> readConstructor(AConstructor.build(null), tag));
        _fillLazilyLoadedList(node.procedures(), (int tag) -> readProcedure(AProcedure.build(null, null, null), tag));
        typeParameterStack.clear();
    }

    private void readMixinClass(AClass node) {
        int flags = readByte();
        node.isAbstract_$eq((flags & 0x1) != 0);
        node.level_$eq(_currentLibrary.isExternal()
                ? ((flags & 0x2) != 0) ? ClassLevel.Type() : ClassLevel.Hierarchy()
                : ClassLevel.Body());

        node.name_$eq(readStringOrNullIfEmpty());
        node.fileUri_$eq(readUriReference());
        node.annotations_$eq(readAnnotationList(node));

        readAndPushTypeParameterList(node.typeParameters(), node);
        node.supertype_$eq(readSupertype());
        node.mixedInType_$eq(readSupertype());
        _fillNonTreeNodeList(node.implementedTypes(), () -> readSupertype());
        _fillLazilyLoadedList(node.constructors(), (int tag) -> readConstructor(AConstructor.build(null), tag));
        typeParameterStack.clear();
    }

    private int getAndResetTransformerFlags() {
        int flags = _transformerFlags;
        _transformerFlags = 0;
        return flags;
    }

    /// Adds the given flag to the current [Member.transformerFlags].
    private void addTransformerFlag(int flags) {
        _transformerFlags |= flags;
    }

    private AField readField(AField node, int tag) {
        // Note: as with readProcedure and readConstructor, the tag parameter
        // is unused, but we pass it in to clarify that the tag has already been
        // consumed from the input.
        assert (AnyTag.apply(tag) == AnyTag.Field());
        node.fileOffset_$eq(readOffset());
        node.flags_$eq(readByte());
        node.name_$eq(readName());
        node.fileUri_$eq(readUriReference());
        node.annotations_$eq(readAnnotationList(node));

        node.ttype_$eq(readDartType());
        node.inferredValue_$eq(readOptionalInferredValue());
        node.initializer_$eq(readExpressionOption());

        node.transformerFlags_$eq(getAndResetTransformerFlags());
        return node;
    }

    private AConstructor readConstructor(AConstructor node, int tag) {
        assert (AnyTag.apply(tag) == AnyTag.Constructor());
        node.flags_$eq(readByte());
        node.name_$eq(readName());
        node.annotations_$eq(readAnnotationList(node));

        node.function_$eq(readFunctionNode());

        pushVariableDeclarations(node.function().positionalParameters());
        pushVariableDeclarations(node.function().namedParameters());
        _fillTreeNodeList(node.initializers(), () -> readInitializer(), node);

        variableStack.clear();

        node.transformerFlags_$eq(getAndResetTransformerFlags());
        return node;
    }

    private AProcedure readProcedure(AProcedure node, int tag) {
        assert (AnyTag.apply(tag) == AnyTag.Procedure());
        int kindIndex = readByte();
        node.kind_$eq(ProcedureKind.apply(kindIndex));
        node.flags_$eq(readByte());
        node.name_$eq(readName());
        node.fileUri_$eq(readUriReference());
        node.annotations_$eq(readAnnotationList(node));

        node.function_$eq(readFunctionNodeOption());
        node.transformerFlags_$eq(getAndResetTransformerFlags());
        return node;
    }

    private AInitializer readInitializer() {
        AnyTag$.Value tag = AnyTag.apply(readByte());
        if (tag == AnyTag.InvalidInitializer()) {
            return new AInvalidInitializer();
        } else if (tag == AnyTag.FieldInitializer()) {
            return new FieldInitializer(readMemberReference(false), readExpression());
        } else if (tag == AnyTag.SuperInitializer()) {
            return new SuperInitializer(readMemberReference(false), readArguments());
        } else if (tag == AnyTag.RedirectingInitializer()) {
            return new RedirectingInitializer(readMemberReference(false), readArguments());
        } else if (tag == AnyTag.LocalInitializer()) {
            return new ALocalInitializer(readAndPushVariableDeclaration());
        } else {
            throw fail("Invalid initializer tag:" + tag);
        }
    }

    private AFunctionNode readFunctionNodeOption() {
        return readAndCheckOptionTag() ? readFunctionNode() : null;
    }

    private AFunctionNode readFunctionNode() {
        AsyncMarker$.Value asyncMarker = AsyncMarker.apply(readByte());
        int typeParameterStackHeight = typeParameterStack.size();
        List<TypeParameter> typeParameters = readAndPushTypeParameterList(null, null);
        int requiredParameterCount = readUInt();
        int variableStackHeight = variableStack.size();
        List<AVariableDeclaration> positional = readAndPushVariableDeclarationList();
        List<AVariableDeclaration> named = readAndPushVariableDeclarationList();

        DartType returnType = readDartType();
        AInferredValue inferredReturnValue = readOptionalInferredValue();
        int oldLabelStackBase = labelStackBase;
        labelStackBase = labelStack.size();
        AStatement body = readStatementOption();
        labelStackBase = oldLabelStackBase;
        variableStack.clear();
        setLength(variableStack, variableStackHeight);
        typeParameterStack.clear();
        setLength(typeParameterStack, typeParameterStackHeight);
        return new AFunctionNode(body,
                typeParameters,
                requiredParameterCount,
                positional,
                named,
                returnType,
                inferredReturnValue,
                asyncMarker);
    }

    private void pushVariableDeclaration(AVariableDeclaration variable) {
        variableStack.add(variable);
    }

    private void pushVariableDeclarations(List<AVariableDeclaration> variables) {
        variableStack.addAll(variables);
    }

    private AVariableDeclaration readVariableReference() {
        int index = readUInt();
        if (index >= variableStack.size()) {
            throw fail("Invalid variable index: " + index);
        }
        return variableStack.get(index);
    }

    private String logicalOperatorToString(int index) {
        switch (index) {
            case 0:
                return "&&";
            case 1:
                return "||";
            default:
                throw fail("Invalid logical operator index: " + index);
        }
    }

    private List<AExpression> readExpressionList() {
        int size = readUInt();
        List<AExpression> r = mkList(size);
        for (int i = 0; i < size; i++) {
            r.set(i, readExpression());
        }
        return r;
    }

    private AExpression readExpressionOption() {
        return readAndCheckOptionTag() ? readExpression() : null;
    }

    private AExpression readExpression() {
        int tagByte = readByte();
        int tag = (tagByte & Masks.SpecializedTagHighBit().id()) == 0
                ? tagByte
                : (tagByte & Masks.SpecializedTagMask().id());
        AnyTag$.Value t = AnyTag.apply(tag);
        if (t == AnyTag.InvalidExpression())
            return new AInvalidExpression();
        else if (t == AnyTag.VariableGet())
            return new AVariableGet(readVariableReference(), readDartTypeOption());
        else if (t == AnyTag.SpecializedVariableGet()) {
            int index = tagByte & Masks.SpecializedPayloadMask().id();
            return new AVariableGet(variableStack.get(index), null);
        } else if (t == AnyTag.VariableSet())
            return new AVariableSet(readVariableReference(), readExpression());
        else if (t == AnyTag.SpecializedVariableSet()) {
            int index = tagByte & Masks.SpecializedPayloadMask().id();
            return new AVariableSet(variableStack.get(index), readExpression());
        } else if (t == AnyTag.PropertyGet()) {
            int offset = readOffset();
            APropertyGet pg = new APropertyGet(readExpression(), readName(), readMemberReference(true));
            pg.fileOffset_$eq(offset);
            return pg;
        } else if (t == AnyTag.PropertySet()) {
            int offset = readOffset();
            APropertySet pg = new APropertySet(readExpression(), readName(), readExpression(), readMemberReference(true));
            pg.fileOffset_$eq(offset);
            return pg;
        } else if (t == AnyTag.SuperPropertyGet()) {
            addTransformerFlag(TransformerFlag.superCalls().id());
            return new ASuperPropertyGet(readName(), readMemberReference(true));
        } else if (t == AnyTag.SuperPropertySet()) {
            addTransformerFlag(TransformerFlag.superCalls().id());
            return new ASuperPropertySet(readName(), readExpression(), readMemberReference(true));
        } else if (t == AnyTag.DirectPropertyGet()) {
            return new ADirectPropertyGet(readExpression(), readMemberReference(false));
        } else if (t == AnyTag.DirectPropertySet()) {
            return new ADirectPropertySet(readExpression(), readMemberReference(false), readExpression());
        } else if (t == AnyTag.StaticGet()) {
            int offset = readOffset();
            AStaticGet sg = new AStaticGet(readMemberReference(false));
            sg.fileOffset_$eq(offset);
            return sg;
        } else if (t == AnyTag.StaticSet()) {
            return new AStaticSet(readMemberReference(false), readExpression());
        } else if (t == AnyTag.MethodInvocation()) {
            int offset = readOffset();
            AMethodInvocation mi = new AMethodInvocation(readExpression(), readName(), readArguments(), readMemberReference(true));
            mi.fileOffset_$eq(offset);
            return mi;
        } else if (t == AnyTag.SuperMethodInvocation()) {
            int offset = readOffset();
            addTransformerFlag(TransformerFlag.superCalls().id());
            ASuperMethodInvocation smi = new ASuperMethodInvocation(
                    readName(), readArguments(), readMemberReference(true));
            smi.fileOffset_$eq(offset);
            return smi;
        } else if (t == AnyTag.DirectMethodInvocation()) {
            return new ADirectMethodInvocation(readExpression(), readMemberReference(false), readArguments());
        } else if (t == AnyTag.StaticInvocation()) {
            int offset = readOffset();
            AStaticInvocation si = new AStaticInvocation(readMemberReference(false), readArguments(), false);
            si.fileOffset_$eq(offset);
            return si;
        } else if (t == AnyTag.ConstStaticInvocation()) {
            int offset = readOffset();
            AStaticInvocation si = new AStaticInvocation(readMemberReference(false), readArguments(), true);
            si.fileOffset_$eq(offset);
            return si;
        } else if (t == AnyTag.ConstructorInvocation()) {
            int offset = readOffset();
            ConstructorInvocation ci = new ConstructorInvocation(readMemberReference(false), readArguments(), false);
            ci.fileOffset_$eq(offset);
            return ci;
        } else if (t == AnyTag.ConstConstructorInvocation()) {
            int offset = readOffset();
            ConstructorInvocation ci = new ConstructorInvocation(readMemberReference(false), readArguments(), true);
            ci.fileOffset_$eq(offset);
            return ci;
        } else if (t == AnyTag.Not()) {
            return new Not(readExpression());
        } else if (t == AnyTag.LogicalExpression())
            return new ALogicalExpression(readExpression(),
                    logicalOperatorToString(readByte()), readExpression());
        else if (t == AnyTag.ConditionalExpression())
            return new AConditionalExpression(readExpression(), readExpression(),
                    readExpression(), readDartTypeOption());
        else if (t == AnyTag.StringConcatenation())
            return new StringConcatenation(readExpressionList());
        else if (t == AnyTag.IsExpression())
            return new IsExpression(readExpression(), readDartType());
        else if (t == AnyTag.AsExpression())
            return new AsExpression(readExpression(), readDartType());
        else if (t == AnyTag.StringLiteral())
            return new AStringLiteral(readStringReference());
        else if (t == AnyTag.SpecializedIntLiteral()) {
            int biasedValue = tagByte & Masks.SpecializedPayloadMask().id();
            return new AIntLiteral(BigInt.apply(biasedValue - OtherEnum.SpecializedIntLiteralBias().id()));
        } else if (t == AnyTag.PositiveIntLiteral())
            return new AIntLiteral(BigInt.apply(readUInt()));
        else if (t == AnyTag.NegativeIntLiteral())
            return new AIntLiteral(BigInt.apply(-readUInt()));
        else if (t == AnyTag.BigIntLiteral())
            return new AIntLiteral(BigInt.apply(readStringReference()));
        else if (t == AnyTag.DoubleLiteral())
            return new ADoubleLiteral(Double.valueOf(readStringReference()));
        else if (t == AnyTag.TrueLiteral())
            return new ABooleanLiteral(true);
        else if (t == AnyTag.FalseLiteral())
            return new ABooleanLiteral(false);
        else if (t == AnyTag.NullLiteral())
            return new ANullLiteral();
        else if (t == AnyTag.SymbolLiteral())
            return new ASymbolLiteral(readStringReference());
        else if (t == AnyTag.TypeLiteral())
            return new TypeLiteral(readDartType());
        else if (t == AnyTag.ThisExpression())
            return new ThisExpression();
        else if (t == AnyTag.Rethrow())
            return new Rethrow();
        else if (t == AnyTag.Throw()) {
            int offset = readOffset();
            Throw th = new Throw(readExpression());
            th.fileOffset_$eq(offset);
            return th;
        } else if (t == AnyTag.ListLiteral()) {
            DartType typeArgument = readDartType();
            return new ListLiteral(readExpressionList(), typeArgument, false);
        } else if (t == AnyTag.ConstListLiteral()) {
            DartType typeArgument = readDartType();
            return new ListLiteral(readExpressionList(),
                    typeArgument, true);
        } else if (t == AnyTag.MapLiteral()) {
            DartType keyType = readDartType();
            DartType valueType = readDartType();
            return new MapLiteral(readMapEntryList(), keyType, valueType, false);
        } else if (t == AnyTag.ConstMapLiteral()) {
            DartType keyType = readDartType();
            DartType valueType = readDartType();
            return new MapLiteral(readMapEntryList(), keyType, valueType, true);
        } else if (t == AnyTag.AwaitExpression())
            return new AwaitExpression(readExpression());
        else if (t == AnyTag.FunctionExpression())
            return new FunctionExpression(readFunctionNode());
        else if (t == AnyTag.Let()) {
            AVariableDeclaration variable = readVariableDeclaration();
            int stackHeight = variableStack.size();
            pushVariableDeclaration(variable);
            AExpression body = readExpression();
            variableStack.clear();
            setLength(variableStack, stackHeight);
            return new Let(variable, body);
        } else
            throw fail("Invalid expression tag: " + tag);
    }


    private List<MapEntry> readMapEntryList() {
        int amount = readUInt();
        List<MapEntry> mel = mkList(amount);
        for (int i = 0; i < amount; i++) {
            mel.set(i, readMapEntry());
        }
        return mel;
    }

    private MapEntry readMapEntry() {
        return new MapEntry(readExpression(), readExpression());
    }

    private List<AStatement> readStatementList() {
        int length = readUInt();
        List<AStatement> sl = mkList(length);
        for (int i = 0; i < length; i++)
            sl.set(i, readStatement());
        return sl;
    }

    private AStatement readStatementOrNullIfEmpty() {
        AStatement node = readStatement();
        if (node instanceof AEmptyStatement) {
            return null;
        } else {
            return node;
        }
    }

    private AStatement readStatementOption() {
        return readAndCheckOptionTag() ? readStatement() : null;
    }

    private AStatement readStatement() {
        int tag = readByte();
        AnyTag$.Value t = AnyTag.apply(tag);

        if (t == AnyTag.InvalidStatement())
            return new AInvalidStatement();
        else if (t == AnyTag.ExpressionStatement())
            return new AExpressionStatement(readExpression());
        else if (t == AnyTag.Block())
            return readBlock();
        else if (t == AnyTag.EmptyStatement())
            return new AEmptyStatement();
        else if (t == AnyTag.AssertStatement())
            return new AssertStatement(readExpression(), readExpressionOption());
        else if (t == AnyTag.LabeledStatement()) {
            LabeledStatement label = new LabeledStatement(null);
            labelStack.add(label);
            label.body_$eq(readStatement());
            labelStack.remove(labelStack.size() - 1);
            return label;
        } else if (t == AnyTag.BreakStatement()) {
            int index = readUInt();
            return new BreakStatement(labelStack.get(labelStackBase + index));
        } else if (t == AnyTag.WhileStatement())
            return new WhileStatement(readExpression(), readStatement());
        else if (t == AnyTag.DoStatement())
            return new DoStatement(readStatement(), readExpression());
        else if (t == AnyTag.ForStatement()) {
            int variableStackHeight = variableStack.size();
            List<AVariableDeclaration> variables = readAndPushVariableDeclarationList();
            AExpression condition = readExpressionOption();
            List<AExpression> updates = readExpressionList();
            AStatement body = readStatement();

            setLength(variableStack, variableStackHeight);

            return new ForStatement(variables, condition, updates, body);
        } else if (t == AnyTag.ForInStatement() || t == AnyTag.AsyncForInStatement()) {
            boolean isAsync = (t == AnyTag.AsyncForInStatement());
            int variableStackHeight = variableStack.size();
            AVariableDeclaration variable = readAndPushVariableDeclaration();
            AExpression iterable = readExpression();
            AStatement body = readStatement();

            setLength(variableStack, variableStackHeight);

            return new ForInStatement(variable, iterable, body, isAsync);
        } else if (t == AnyTag.SwitchStatement()) {
            AExpression expression = readExpression();
            int count = readUInt();
            List<SwitchCase> cases = mkList(count);
            for (int i = 0; i < count; i++)
                cases.set(i, SwitchCase.build());
            switchCaseStack.addAll(cases);
            for (int i = 0; i < cases.size(); ++i) {
                SwitchCase caseNode = cases.get(i);
                _fillTreeNodeList(caseNode.expressions(), () -> readExpression(), caseNode);
                caseNode.isDefault_$eq(readByte() == 1);
                caseNode.body_$eq(readStatement());
            }
            setLength(switchCaseStack, switchCaseStack.size() - count);
            return new SwitchStatement(expression, cases);
        } else if (t == AnyTag.ContinueSwitchStatement()) {
            int index = readUInt();
            return new ContinueSwitchStatement(switchCaseStack.get(index));
        } else if (t == AnyTag.IfStatement())
            return new IfStatement(readExpression(), readStatement(), readStatementOrNullIfEmpty());
        else if (t == AnyTag.ReturnStatement())
            return new ReturnStatement(readExpressionOption());
        else if (t == AnyTag.TryCatch())
            return new TryCatch(readStatement(), readCatchList());
        else if (t == AnyTag.TryFinally())
            return new TryFinally(readStatement(), readStatement());
        else if (t == AnyTag.YieldStatement()) {
            int flags = readByte();
            return new YieldStatement(readExpression(),
                    (flags & YieldStatement.FlagYieldStar()) != 0,
                    (flags & YieldStatement.FlagNative()) != 0);
        } else if (t == AnyTag.VariableDeclaration()) {
            AVariableDeclaration variable = readVariableDeclaration();
            variableStack.add(variable); // Will be popped by the enclosing scope.
            return variable;
        } else if (t == AnyTag.FunctionDeclaration()) {
            AVariableDeclaration variable = readVariableDeclaration();
            variableStack.add(variable); // Will be popped by the enclosing scope.
            AFunctionNode function = readFunctionNode();
            return new FunctionDeclaration(variable, function);
        } else {
            throw fail("Invalid statement tag: " + tag);
        }
    }

    private List<ACatch> readCatchList() {
        int length = readUInt();
        List<ACatch> cl = mkList(length);
        for (int i = 0; i < length; i++)
            cl.set(i, readCatch());
        return cl;
    }

    private ACatch readCatch() {
        int variableStackHeight = variableStack.size();
        DartType guard = readDartType();
        AVariableDeclaration exception = readAndPushVariableDeclarationOption();
        AVariableDeclaration stackTrace = readAndPushVariableDeclarationOption();
        AStatement body = readStatement();
        setLength(variableStack, variableStackHeight);
        return new ACatch(exception, body, guard, stackTrace);
    }

    private ABlock readBlock() {
        int stackHeight = variableStack.size();
        List<AStatement> body = readStatementList();
        setLength(variableStack, stackHeight);
        return new ABlock(body);
    }

    private Supertype readSupertype() {
        InterfaceType type = (InterfaceType) readDartType();
        return new Supertype(type.classNode(), type.typeArguments());
    }

    private Supertype readSupertypeOption() {
        return readAndCheckOptionTag() ? readSupertype() : null;
    }

    List<Supertype> readSupertypeList() {
        int count = readUInt();
        List<Supertype> sl = mkList(count);
        for (int i = 0; i < count; i++)
            sl.set(i, readSupertype());
        return sl;
    }

    private List<DartType> readDartTypeList() {
        int count = readUInt();
        List<DartType> sl = mkList(count);
        for (int i = 0; i < count; i++)
            sl.set(i, readDartType());
        return sl;
    }

    private List<NamedType> readNamedTypeList() {
        int count = readUInt();
        List<NamedType> sl = mkList(count);
        for (int i = 0; i < count; i++)
            sl.set(i, readNamedType());
        return sl;
    }

    private NamedType readNamedType() {
        return new NamedType(readStringReference(), readDartType());
    }

    private DartType readDartTypeOption() {
        return readAndCheckOptionTag() ? readDartType() : null;
    }

    private DartType readDartType() {
        int tag = readByte();
        AnyTag$.Value t = AnyTag.apply(tag);
        if (t == AnyTag.BottomType())
            return new BottomType();
        else if (t == AnyTag.InvalidType())
            return new InvalidType();
        else if (t == AnyTag.DynamicType())
            return new DynamicType();
        else if (t == AnyTag.VoidType())
            return new VoidType();
        else if (t == AnyTag.InterfaceType())
            return new InterfaceType(readClassReference(false), readDartTypeList());
        else if (t == AnyTag.SimpleInterfaceType())
            return new InterfaceType(readClassReference(false), new ArrayList<>());
        else if (t == AnyTag.FunctionType()) {
            int typeParameterStackHeight = typeParameterStack.size();
            List<TypeParameter> typeParameters = readAndPushTypeParameterList(null, null);
            int requiredParameterCount = readUInt();
            List<DartType> positional = readDartTypeList();
            List<NamedType> named = readNamedTypeList();
            DartType returnType = readDartType();
            setLength(typeParameterStack, typeParameterStackHeight);
            return FunctionType.build(positional, returnType,
                    typeParameters,
                    requiredParameterCount,
                    named);
        } else if (t == AnyTag.SimpleFunctionType()) {
            List<DartType> positional = readDartTypeList();
            DartType returnType = readDartType();
            return FunctionType.build(positional, returnType);
        } else if (t == AnyTag.TypeParameterType()) {
            int index = readUInt();
            return new TypeParameterType(typeParameterStack.get(index));
        } else {
            throw fail("Invalid dart type tag: " + tag + "(" + t + ")");
        }
    }

    private List<TypeParameter> readAndPushTypeParameterList(List<TypeParameter> list, TreeNode parent) {
        int length = readUInt();
        if (length == 0) return list != null ? list : new ArrayList<>();
        if (list == null) {
            list = mkList(length);
            for (int i = 0; i < length; i++)
                list.set(i, new TypeParameter(null, null));
        } else {
            setLength(list, length);
            for (int i = 0; i < length; ++i) {
                list.set(i, new TypeParameter(null, null));
            }
        }
        typeParameterStack.addAll(list);
        for (int i = 0; i < list.size(); ++i) {
            readTypeParameter(list.get(i));
        }
        return list;
    }

    private void readTypeParameter(TypeParameter node) {
        node.name_$eq(readStringOrNullIfEmpty());
        node.bound_$eq(readDartType());
    }

    private AArguments readArguments() {
        List<DartType> typeArguments = readDartTypeList();
        List<AExpression> positional = readExpressionList();
        List<ANamedExpression> named = readNamedExpressionList();
        return new AArguments(positional, typeArguments, named);
    }

    private List<ANamedExpression> readNamedExpressionList() {
        int length = readUInt();
        List<ANamedExpression> nel = mkList(length);
        for (int i = 0; i < length; i++) {
            nel.set(i, readNamedExpression());
        }
        return nel;
    }

    private ANamedExpression readNamedExpression() {
        return new ANamedExpression(readStringReference(), readExpression());
    }

    private List<AVariableDeclaration> readAndPushVariableDeclarationList() {
        int length = readUInt();
        List<AVariableDeclaration> nel = mkList(length);
        for (int i = 0; i < length; i++) {
            nel.set(i, readAndPushVariableDeclaration());
        }
        return nel;
    }

    private AVariableDeclaration readAndPushVariableDeclarationOption() {
        return readAndCheckOptionTag() ? readAndPushVariableDeclaration() : null;
    }

    private AVariableDeclaration readAndPushVariableDeclaration() {
        AVariableDeclaration variable = readVariableDeclaration();
        variableStack.add(variable);
        return variable;
    }

    private AVariableDeclaration readVariableDeclaration() {
        int flags = readByte();
        return new AVariableDeclaration(readStringOrNullIfEmpty(),
                readDartType(),
                readOptionalInferredValue(),
                readExpressionOption(),
                (flags & 0x1) != 0,
                (flags & 0x2) != 0);
    }

    private int readOffset() {
        // Offset is saved as unsigned,
        // but actually ranges from -1 and up (thus the -1)
        return readUInt() - 1;
    }
}