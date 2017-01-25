package dk.au.cs.dartkreader.loader;

import java.util.List;
import dk.au.cs.ast.*;

public class BinaryLoader {

    static int _pow2roundup(int x) {
        --x;
        x |= x >> 1;
        x |= x >> 2;
        x |= x >> 4;
        x |= x >> 8;
        x |= x >> 16;
        return x + 1;
    }

    interface Builder<T> {
        T build();
    }


    private void setLength(List l, int newSize) {
        for (int i = l.size() - 1; i >= newSize; i--)
            l.remove(i);
        for (int i = l.size(); i < newSize; i++)
            l.add(null);
    }


    <T> T _extendList(List<T> items, int index, Builder<T> build) {
        if (items.size() <= index) {
            // Avoid excessive resizing by growing the list in steps.
            setLength(items, _pow2roundup(index + 1));
        }
        if (items.get(index) == null) {
            items.set(index, build.build());
        }
        return items.get(index);
    }

    AClass getClassReference(ALibrary library, int tag, int index) {
        return _extendList(library.classes(), index, () -> _buildClassReference(tag));
    }

    AClass _buildClassReference(int tag) {
        return AClass.build();
    }

    AField _buildFieldReference() {
        return AField.build(null);
    }

    AConstructor _buildConstructorReference() {
        return AConstructor.build(null);
    }

    AProcedure _buildProcedureReference() {
        return AProcedure.build(null, null, null);
    }

    AMember getMemberReference(TreeNode classOrLibrary, int tag, int index) {
        if (classOrLibrary instanceof AClass) {
            return getClassMemberReference((AClass) classOrLibrary, tag, index);
        } else {
            return getLibraryMemberReference((ALibrary) classOrLibrary, tag, index);
        }
    }

    AMember getLibraryMemberReference(ALibrary library, int tag, int index) {
        AnyTag$.Value t = AnyTag.apply(tag);
        if (t == AnyTag.LibraryFieldReference() || t == AnyTag.Field()) {
            return _extendList(library.fields(), index, () -> _buildFieldReference());
        } else if (t == AnyTag.LibraryProcedureReference() || t == AnyTag.Procedure()) {
            return _extendList(library.procedures(), index, () -> _buildProcedureReference());
        } else {
            throw new RuntimeException("Invalid library member reference tag: " + tag);
        }
    }

    AMember getClassMemberReference(AClass classNode, int tag, int index) {
        AnyTag$.Value t = AnyTag.apply(tag);
        if (t == AnyTag.ClassFieldReference() || t == AnyTag.Field()) {
            return _extendList(classNode.fields(), index, () -> _buildFieldReference());
        } else if (t == AnyTag.ClassConstructorReference() || t == AnyTag.Constructor()) {
            return _extendList(classNode.constructors(), index, () -> _buildConstructorReference());
        } else if (t == AnyTag.ClassProcedureReference() || t == AnyTag.Procedure()) {
            return _extendList(classNode.procedures(), index, () -> _buildProcedureReference());
        } else {
            throw new RuntimeException("Invalid library member reference tag: " + tag);
        }
    }
}
