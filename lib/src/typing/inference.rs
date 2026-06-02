//! Модуль с алгоритмами выводов типов для [`crate::parser::Ast`].

use std::collections::HashMap;

use derived_deref::{Deref, DerefMut};
use thiserror::Error;

use crate::{
    Context, ProgramId, hir::{DeclMap, DefMap}, parser::{AstNode, Builtin}, prelude::{STD_PRINT_FN_NAME, STD_READ_FN_NAME}, typing::{
        fmt::fmt_vec,
        structs::{StackCfg, StackVar, Type},
    }
};

#[derive(Error, Debug)]
pub enum TypingError {
    #[error("incompatible types {0} and {1}")]
    IncompatibleTypes(StackVar, StackVar),
    #[error("incompatible stacks {0} and {1}")]
    IncompatibleStacks(StackCfg, StackCfg),
    #[error("unknown declaration {0}")]
    UnknownIdentifier(String),
}

/// Представление ограничения
///
/// Вывод типов связан согласованием конфигураций стека между командами. Данный тип описывает такие требования согласования.
#[derive(Debug, Clone)]
pub(crate) enum Constraint {
    /// Требование унификации типов
    Unification(StackVar, StackVar),
    /// Требование согласования размеров стека
    TailExtension(StackCfg, StackCfg),
    // TODO склеить в один инвариант
}

impl Constraint {
    fn tail_extension(lhs: impl Into<StackCfg>, rhs: impl Into<StackCfg>) -> Constraint {
        Constraint::TailExtension(lhs.into(), rhs.into())
    }
}

#[derive(Debug, Clone)]
pub(crate) enum Replacement {
    Stack(StackCfg, StackCfg),
    Identity, // TODO убрать этот инвариант, тк он бесполезен
}

impl Replacement {
    fn stack(from: StackCfg, to: StackCfg) -> Replacement {
        Replacement::Stack(from, to)
    }

    fn term(from: StackVar, to: StackVar) -> Replacement {
        Replacement::Stack(vec![from].into(), vec![to].into())
    }
}

#[derive(Clone, Debug, Deref, DerefMut)]
pub(crate) struct TypesMap(pub(crate) HashMap<ProgramId, Type>);

/// Вывод типа для всей программы
pub(crate) fn infer_definitions<'n>(
    decls: &DeclMap,
    defs: &mut DefMap<'n>,
    ctx: &mut Context,
) -> Result<TypesMap, TypingError> {
    let mut def_types: TypesMap = TypesMap(HashMap::new());

    for (name, program_id) in decls.iter() {
        if let Some(t) = std_type(name) {
            def_types.insert(*program_id, t);
        } else {
            let program = *defs.get(program_id).unwrap();
            if !def_types.contains_key(program_id) {
                let t = infer(*program_id, program, decls, defs, &mut def_types, ctx)?;
                def_types.insert(*program_id, t);
            }
        }
    }

    Ok(def_types)
}

fn std_type(name: &String) -> Option<Type> {
    if name == STD_READ_FN_NAME {
        let tail = StackVar::tail();
        let int = StackVar::int();
        Option::Some(Type::from_inp_out([tail.clone()], [tail, int]))
    } else if name == STD_PRINT_FN_NAME {
        let tail = StackVar::tail();
        let int = StackVar::int();
        Option::Some(Type::from_inp_out([tail.clone(), int], [tail]))
    } else {
        Option::None
    }
}

/// Вывод типа для последовательности команд
fn infer<'n>(
    program_id: ProgramId,
    nodes: &'n Vec<AstNode>,
    decls: &DeclMap,
    defs: &mut DefMap<'n>,
    types: &mut TypesMap,
    ctx: &mut Context,
) -> Result<Type, TypingError> {
    ctx.emit_debug(format!("infer program {}", program_id));

    let mut prog_type = match nodes.first() {
        Some(first) => {
            ctx.emit_debug(format!("chaining {}", first));
            get_node_type(first, decls, defs, types, &mut ctx.step())
        }
        None => Ok(Type::trivial()),
    }?;
    ctx.emit_debug(format!("init node type: {}", prog_type));
    ctx.emit_debug("---");

    for node in nodes.iter().skip(1) {
        ctx.emit_debug(format!("chaining {}", node));
        let node_type = get_node_type(node, decls, defs, types, &mut ctx.step())?;
        ctx.emit_debug(format!("chain node type: {}", node_type));
        prog_type = chain(&prog_type, &node_type, &mut ctx.step())?;
        ctx.emit_debug("---");
    }

    ctx.emit_debug(format!("resulted type {}", prog_type));

    Ok(prog_type)
}

/// Типы для встроенных конструкций
///
/// ```text
/// eval    : (('S -> 'R) 'S -> 'R)
/// quote   : ('a 'S -> ('R -> 'a 'R) 'S)
/// compose : (('B -> 'C) ('A -> 'B) 'S -> ('A -> 'C) 'S)
/// dup     : ('a 'S -> 'a 'a 'S)
/// pop     : ('a 'S -> 'S)
/// swap    : ('a 'b 'S -> 'b 'a 'S)
/// cond    : ('a 'a Bool 'S -> 'a 'S)
/// while   : (('S -> Bool 'R) ('R -> 'S) 'S -> 'S)
/// ```
fn get_node_type<'n>(
    node: &'n AstNode,
    decls: &DeclMap,
    defs: &mut DefMap<'n>,
    types: &mut TypesMap,
    ctx: &mut Context,
) -> Result<Type, TypingError> {
    match node {
        AstNode::BuiltinIdentifier { id: _, value } => match value {
            Builtin::Eval => {
                let tail = StackVar::tail();
                let new_tail = StackVar::tail();
                // TODO тут точно надо ProgramId::new() ?
                let quote = StackVar::quote(
                    ProgramId::new(),
                    Type::from_inp_out([tail.clone()], [new_tail.clone()]),
                );
                Ok(Type::from_inp_out([tail, quote], [new_tail]))
            }
            Builtin::If => {
                let tail = StackVar::tail();
                let bool = StackVar::bool();
                let new_tail = StackVar::tail();
                let quote = StackVar::quote(
                    ProgramId::new(),
                    Type::from_inp_out([tail.clone()], [new_tail.clone()]),
                );
                Ok(Type::from_inp_out(
                    [tail, bool, quote.clone(), quote.clone()],
                    [new_tail],
                ))
            }
            Builtin::While => {
                let tail = StackVar::tail();
                let in_tail = StackVar::tail();
                let bool = StackVar::bool();
                let cond_quote = StackVar::quote(
                    ProgramId::new(),
                    Type::from_inp_out([tail.clone()], [in_tail.clone(), bool]),
                );
                let body_quote = StackVar::quote(
                    ProgramId::new(),
                    Type::from_inp_out([in_tail], [tail.clone()]),
                );
                Ok(Type::from_inp_out(
                    [tail.clone(), cond_quote, body_quote],
                    [tail],
                ))
            }
            Builtin::Add | Builtin::Sub | Builtin::Mul | Builtin::Div => {
                let tail = StackVar::tail();
                let a = StackVar::int();
                let b = StackVar::int();
                let c = StackVar::int();
                Ok(Type::from_inp_out([tail.clone(), a, b], [tail, c]))
            }
            Builtin::Less | Builtin::LessOrEq | Builtin::Great | Builtin::GreatOrEq => {
                let tail = StackVar::tail();
                let a = StackVar::int();
                let b = StackVar::int();
                let c = StackVar::bool();
                Ok(Type::from_inp_out([tail.clone(), a, b], [tail, c]))
            }
            Builtin::Pop => {
                let tail = StackVar::tail();
                let var = StackVar::var();
                Ok(Type::from_inp_out([tail.clone(), var], [tail.clone()]))
            }
            Builtin::Dup => {
                let tail = StackVar::tail();
                let var = StackVar::var();
                Ok(Type::from_inp_out(
                    [tail.clone(), var.clone()],
                    [tail, var.clone(), var],
                ))
            }
            Builtin::Swap => {
                let tail = StackVar::tail();
                let lhs = StackVar::var();
                let rhs = StackVar::var();
                Ok(Type::from_inp_out(
                    [tail.clone(), lhs.clone(), rhs.clone()],
                    [tail, rhs, lhs],
                ))
            }
            Builtin::Quote => {
                let tail = StackVar::tail();
                let tail_in = StackVar::tail();
                let var = StackVar::var();
                let var_quoted = StackVar::quote(
                    ProgramId::new(),
                    Type::from_inp_out([tail_in.clone()], [tail_in.clone(), var.clone()]),
                );
                Ok(Type::from_inp_out([tail.clone(), var], [tail, var_quoted]))
            }
            Builtin::Compose => {
                let tail = StackVar::tail();
                let from = StackVar::tail();
                let mid = StackVar::tail();
                let to = StackVar::tail();
                let quote1 = StackVar::quote(
                    ProgramId::new(),
                    Type::from_inp_out([from.clone()], [mid.clone()]),
                );
                let quote2 =
                    StackVar::quote(ProgramId::new(), Type::from_inp_out([mid], [to.clone()]));
                let quote_res = StackVar::quote(ProgramId::new(), Type::from_inp_out([from], [to]));
                Ok(Type::from_inp_out(
                    [tail.clone(), quote1, quote2],
                    [tail, quote_res],
                ))
            }
        },
        AstNode::Define {
            id: _,
            name: _,
            value: _,
        } => Ok(Type::trivial()),
        AstNode::Int { id: _, value: _ } => {
            let tail = StackVar::tail();
            let int = StackVar::int();
            Ok(Type::from_inp_out([tail.clone()], [tail, int]))
        }
        AstNode::Bool { id: _, value: _ } => {
            // TODO можно упростить с Int
            let tail = StackVar::tail();
            let bool = StackVar::bool();
            Ok(Type::from_inp_out([tail.clone()], [tail, bool]))
        }
        AstNode::Identifier { id: _, value } => {
            let prog_id = decls
                .get(value)
                .ok_or(TypingError::UnknownIdentifier(value.clone()))?;

            let t = types.get(prog_id);

            if let Some(t) = t {
                Ok(t.clone_change_id())
            } else {
                // TODO нужно ли тут клонировать программу с изменением id? Вроде как да, потому что надо гарантировать полиморфизм
                // Можно множить на каждый вызов новое "определение", а потом схлопывать одинаковые определения.
                // Можно ли отложить на более поздние этапы? Вопрос в том, до какого момента код еще может быть полиморфным.
                // Пока думаю, что лучше попозже заиметь мапу k: (ProgramId, Vec<VarType>, Vec<VarType>), v: ... с неполиморфными определениями.

                let std_t = std_type(value);

                let t = if std_t.is_none() {
                    let prog = defs
                        .get(prog_id)
                        .ok_or(TypingError::UnknownIdentifier(value.clone()))?;
                    infer(*prog_id, prog, decls, defs, types, &mut ctx.step())?
                } else {
                    std_t.unwrap()
                };

                // FIXME зачем тут clone_inp_out, а затем clone_id?
                // UPD потому что надо положить только начало и конец `clone_inp_out`, а еще отвязать переменные внутренние переменные от внешних `clone_id` (точно надо?)
                let t_return = t.clone_only_inp_out().clone_change_id();
                types.insert(*prog_id, t);
                Ok(t_return)
            }
        }
        AstNode::Quote {
            id: _,
            value: quote_program,
        } => {
            ctx.emit_debug(format!("infer quote {:?}", quote_program));

            let tail = StackVar::tail();
            let quote_type = infer(
                quote_program.id,
                quote_program,
                decls,
                defs,
                types,
                &mut ctx.step(),
            )?;
            let quote_var_id = quote_type.id;
            let quote = StackVar::quote(quote_program.id, quote_type.clone_only_inp_out());

            // Цитату необходимо добавить как отдельную программу
            defs.insert(quote_program.id, quote_program);
            types.insert(quote_program.id, quote_type);

            Ok(Type::from_id_inp_out(
                quote_var_id,
                [tail.clone()],
                [tail, quote],
            )) // T-QUOTE rule
        }
    }
}

/// Вывод (сцепка, chaining) общего типа для двух последовательных типов
///
/// Выходная конфигурация `lhs` должна быть сопоставлена с входной конфигурацией `rhs` (T-COMPOSE rule).
/// Сопоставление конфигураций -- попарное сопоставление переменных на верхушках стеков конфигураций. Сопоставление для цитат -- сопоставление их входных и выходных конфигураций.
/// В процессе сопоставления генерируются ограничения, для которых затем ищется наиболее общее решение -- унификация. Если решение не существует, то имеет место ошибка типизации.
fn chain(lhs: &Type, rhs: &Type, ctx: &mut Context) -> Result<Type, TypingError> {
    let (mut lhs, mut rhs) = (lhs.clone(), rhs.clone());
    ctx.emit_debug(format!("types lhs {} rhs {}", lhs, rhs));

    let mut constraints: Vec<Constraint> = constrain_chain(&lhs, &rhs, &mut ctx.step());
    let mut replacements: Vec<Replacement> = vec![];
    ctx.emit_debug(format!("constraints {}", fmt_vec(&constraints)));

    {
        let ctx = &mut ctx.step();
        while let Some(constraint) = constraints.pop() {
            ctx.emit_debug(format!("solve constraint {}", constraint));
            let replacement = chain_solve(constraint)?;
            ctx.emit_debug(format!("by replacement {}", replacement));
            let mut new_constraints: Vec<Constraint> = vec![];
            for constraint in &constraints {
                let mut constraints = constraint
                    .clone()
                    .apply_replacement(&replacement, &mut ctx.step());
                // ctx.emit_debug(format!(
                //     "replace constraint from {} to {}",
                //     constraint, fmt_vec(&constraints)
                // ));
                new_constraints.append(&mut constraints);
            }
            replacements.push(replacement);
            constraints = new_constraints;
        }
    }

    ctx.emit_debug(format!("replacements {}", fmt_vec(&replacements)));

    for replacement in replacements {
        lhs = lhs.apply_replacement(&replacement);
        rhs = rhs.apply_replacement(&replacement);
    }

    ctx.emit_debug(format!("chained types lhs {} rhs {}", lhs, rhs));

    Ok(lhs.append(rhs.seq.into_iter().skip(1)))
}

/// Поиск ограничений сцепки
///
/// Сцепка -- выход первого типа должен совпадать с входом второй типа.
fn constrain_chain(lhs: &Type, rhs: &Type, ctx: &mut Context) -> Vec<Constraint> {
    let (_, lhs_out) = lhs.inp_out();
    let (rhs_inp, _) = rhs.inp_out();
    constrain(lhs_out, rhs_inp, ctx)
}

/// Поиск ограничений эквивалентности
///
/// Эквивалентность типов -- вход и выход первого типа совпадают с входом и выходом второго типа.
fn constrain_equivalence(lhs: &Type, rhs: &Type, ctx: &mut Context) -> Vec<Constraint> {
    let (lhs_inp, lhs_out) = lhs.inp_out();
    let (rhs_inp, rhs_out) = rhs.inp_out();
    let mut constraints: Vec<Constraint> = vec![];

    constraints.append(&mut constrain(lhs_inp, rhs_inp, ctx));
    constraints.append(&mut constrain(lhs_out, rhs_out, ctx));

    constraints
}

/// Поиск ограничений для двух стековых конфигураций
fn constrain(lhs: &StackCfg, rhs: &StackCfg, ctx: &mut Context) -> Vec<Constraint> {
    let mut lhs_iter = lhs.iter().rev().peekable();
    let mut rhs_iter = rhs.iter().rev().peekable();
    let mut constraints: Vec<Constraint> = vec![];

    while lhs_iter.peek().is_some() || rhs_iter.peek().is_some() {
        let lhs = lhs_iter.next().unwrap();
        let lhs_has_next = lhs_iter.peek().is_some();
        let rhs = rhs_iter.next().unwrap();
        let rhs_has_next = rhs_iter.peek().is_some();

        if lhs_has_next == rhs_has_next {
            if lhs != rhs {
                if let StackVar::Quote {
                    program_id: _,
                    inner: lhs,
                } = lhs
                    && let StackVar::Quote {
                        program_id: _,
                        inner: rhs,
                    } = rhs
                {
                    constraints.append(&mut constrain_equivalence(lhs, rhs, &mut ctx.step()));
                } else {
                    constraints.push(Constraint::Unification(lhs.clone(), rhs.clone()));
                }
            }
        } else if !rhs_has_next {
            let lhs: Vec<StackVar> = vec![lhs.clone()]
                .into_iter()
                .chain(lhs_iter.cloned())
                .rev()
                .collect();
            let rhs: Vec<StackVar> = vec![rhs.clone()];
            constraints.push(Constraint::tail_extension(lhs, rhs));
            break;
        } else if !lhs_has_next {
            let lhs: Vec<StackVar> = vec![lhs.clone()];
            let rhs: Vec<StackVar> = vec![rhs.clone()]
                .into_iter()
                .chain(rhs_iter.cloned())
                .rev()
                .collect();
            constraints.push(Constraint::tail_extension(lhs, rhs));
            break;
        }
    }

    constraints
}

/// Поиск решения для ограничения. Подстановка -- сведение двух конфигураций в одну согласно правилам:
///
/// 1. Если два типа, то выбирается наиболее конкретный (пример, Int и Var -> Int)
/// 2. Если конфигурации разного размера, то выбирается наиболее длинная. По сути -- сводится к п.1, если считать, что наиболее общий == наиболее длинный.
fn chain_solve(restriction: Constraint) -> Result<Replacement, TypingError> {
    match restriction {
        Constraint::Unification(lhs, rhs) => {
            // Пока правила достаточно простые, reduce всегда возвращает `Ok(to)`, если сведение возможно
            let r_lhs = chain_reduce(&lhs, &rhs).is_some();
            let r_rhs = chain_reduce(&rhs, &lhs).is_some();
            // ctx.emit_debug(format!("reduce_lhs {} reduce_rhs {}", r_lhs, r_rhs));
            // Приоритетно менять правую часть, чтобы не возникло циклов
            // Но вообще надо бы подумать, действительно ли никак не решить цикл без этого странного необходимого порядка
            if r_rhs {
                Ok(Replacement::term(rhs, lhs))
            } else if r_lhs {
                Ok(Replacement::term(lhs, rhs))
            } else {
                Err(TypingError::IncompatibleTypes(lhs, rhs))
            }
        }
        Constraint::TailExtension(lhs, rhs) => {
            if lhs.first() == rhs.first() {
                return Err(TypingError::IncompatibleStacks(lhs, rhs));
            }

            if lhs.len() < rhs.len() {
                Ok(Replacement::stack(lhs, rhs))
            } else if lhs.len() > rhs.len() {
                Ok(Replacement::stack(rhs, lhs))
            } else {
                Ok(Replacement::Identity)
            }
        }
    }
}

/// Если переменную `from` можно свести к `to` в контексте сцепки типов, то функция возвращает результат сведения
fn chain_reduce<'t>(from: &'t StackVar, to: &'t StackVar) -> Option<&'t StackVar> {
    match (from, to) {
        // Стек можно свести только к другому стеку
        (StackVar::Tail(_), StackVar::Tail(_)) => Option::Some(to),

        // Переменную можно свести к любому более конкретному типу
        (StackVar::Var(_), StackVar::Tail(_)) => Option::Some(to),
        (StackVar::Var(_), StackVar::Var(_)) => Option::Some(to),
        (
            StackVar::Var(_),
            StackVar::Quote {
                program_id: _,
                inner: _,
            },
        ) => Option::Some(to),
        (StackVar::Var(_), StackVar::Int(_)) => Option::Some(to),
        (StackVar::Var(_), StackVar::Bool(_)) => Option::Some(to),

        // Цитату можно свести только к другой цитате
        (
            StackVar::Quote {
                program_id: _,
                inner: _,
            },
            StackVar::Quote {
                program_id: _,
                inner: _,
            },
        ) => Option::Some(to),

        // Число можно свести к числу и булю
        (StackVar::Int(_), StackVar::Int(_)) => Option::Some(to),
        (StackVar::Int(_), StackVar::Bool(_)) => Option::Some(to),

        // Буль можно свести к булю
        (StackVar::Bool(_), StackVar::Bool(_)) => Option::Some(to),

        // В остальных случаях свести нельзя
        _ => Option::None,
    }
}

impl Constraint {
    fn apply_replacement(self, replacement: &Replacement, ctx: &mut Context) -> Vec<Constraint> {
        match replacement {
            Replacement::Stack(_, _) => {
                let (lhs, rhs) = match self {
                    Constraint::Unification(lhs, rhs) => {
                        (StackCfg::new([lhs]), StackCfg::new([rhs]))
                    }
                    Constraint::TailExtension(lhs, rhs) => (lhs, rhs),
                };

                constrain(
                    &stack_cfg_apply_replacement(lhs, replacement),
                    &stack_cfg_apply_replacement(rhs, replacement),
                    ctx,
                )
            }
            Replacement::Identity => vec![self],
        }
    }
}

impl Type {
    fn apply_replacement(self, replacement: &Replacement) -> Type {
        Type::new(
            self.id,
            self.seq
                .into_iter()
                .map(|stack_cfg| stack_cfg_apply_replacement(stack_cfg, replacement))
                .collect::<Vec<_>>(),
        )
    }
}

fn stack_cfg_apply_replacement(old: StackCfg, replacement: &Replacement) -> StackCfg {
    match replacement {
        Replacement::Stack(from, to) => {
            let mut new: StackCfg = StackCfg::empty();
            let mut i = 0;

            while i < old.len() {
                if old[i..].starts_with(from) {
                    let mut to = to.clone();
                    new.append(&mut to);
                    i += from.len();
                } else {
                    let old = old[i].clone();
                    if let StackVar::Quote {
                        program_id,
                        inner,
                    } = &old
                    {
                        // FIXME надо делать замены внутри цитат?
                        // Кажется, что нет
                        // UPD Делать замены надо, но как-то аккуратно... quote swap quote compose eval
                        // UPD 2 Проблема скорее в самом dup, из-за которого подстановки решили не делать -- в нем нужно полностью копировать цитату
                        // UPD 3 с учетом чейнинга в таком случае теряется тип копируемой переменной (тк копирование происходит раньше замены). Придумать случай, когда эта логика ломается 
                        // new.push(StackVar::quote(*program_id, inner.clone_change_id().apply_replacement(replacement)));
                        new.push(StackVar::quote(*program_id, inner.clone().apply_replacement(replacement)));
                        // new.push(old);
                    } else {
                        new.push(old);
                    }
                    i += 1;
                }
            }

            new
        }
        Replacement::Identity => old,
    }
}
