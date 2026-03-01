//! Модуль с high IR представлением.
//!
//! High IR имеет SSA форму (?), полезную для выполнения пассов оптимизации и анализа.
//!
//! При преобразовании [`crate::parser::Ast`] в [`Hir`] для каждой команды вычисляется ее трехадресный код. Вывод типов определяет, какая команда принимает какие данные на вход за счет выведенных формы и размера стека. Преобразование в [`Hir`] также накладывает ограничения на тип главной программы -- она не должна принимать на вход дополнительных аргументов (или может принимать, но тогда это аргументы вызова?; подумать над этим).

/*
Пусть у нас программа
1 dup 0 > [ 1 + ] [ 1 - ] if print

Тогда тип для нее
A0 ->
A0 i1 ->
A0 i1 i1 ->
A0 i1 i1 i6 ->
A0 i1 b10 ->
A0 i1 b10 (A0 i1 ->
           A0 i1 i13 ->
           A0 i18) ->
A0 i1 b10 (A0 i1 -> A0 i1 i 13 -> A0 i18) (A0 i1 ->
                                           A0 i1 i21 ->
                                           A0 i18) ->
A0 i18 ->
A0

Из этого можно построить такой hir
anon1:
    i13 = 1
    i18 = i1 + i13
    ret i18
anon2:
    i21 = 1
    i18 = i1 - i21
    ret i18
start:
    i1 = 1
    i1 = i1
    i6 = 0
    b10 = 1 > 0
    if b1 then goto j1 else goto j2
j1:
    i18 = anon1(i1)
j2:
    i18 = anon2(i1)
j3:
    print(i18)
*/

struct Hir {}

use crate::{Type, parser::AstNode};

struct TypedAst {
    program: TypedProgram,
}

type TypedProgram = Vec<TypedAstNode>;

#[derive(Clone)]
struct TypedAstNode(Type, AstNode);

impl TypedAstNode {
    fn new(_t: Type, _node: AstNode) -> TypedAstNode {
        todo!()
    }
}
