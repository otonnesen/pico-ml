use std::collections::HashMap;
use std::fmt::Display;

use crate::ast::{BinaryOperator, Expr, Ident, UnaryOperator};

#[derive(Debug, Clone)]
pub enum Type {
    Int,
    Dec,
    Bool,
    TypeVar(u8),
    Fun(Box<Type>, Box<Type>),
    List(Box<Type>),
}

#[derive(Debug)]
struct TypingEnv {
    env: HashMap<Ident, Type>,
}

#[derive(Debug)]
struct TypeVarEnv {
    typevar_binds: HashMap<u8, Option<Type>>,
    typevar_count: u8,
}

impl Clone for TypeVarEnv {
    fn clone(&self) -> Self {
        Self {
            typevar_binds: self.typevar_binds.to_owned(),
            typevar_count: self.typevar_count,
        }
    }
}

impl TypeVarEnv {
    fn new() -> Self {
        Self {
            typevar_binds: HashMap::new(),
            typevar_count: 0,
        }
    }

    pub fn next(&mut self) -> Type {
        let typevar = Type::TypeVar(self.typevar_count);
        self.typevar_binds.insert(self.typevar_count, None);
        self.typevar_count += 1;
        typevar
    }
    pub fn get_bind(&self, k: u8) -> Option<&Type> {
        self.typevar_binds.get(&k).unwrap().as_ref()
    }

    pub fn bind(&mut self, i: u8, typ: &Type) {
        self.typevar_binds.insert(i, Some(typ.to_owned()));
    }
}

impl<'a> TypingEnv {
    fn new() -> Self {
        Self {
            env: HashMap::new(),
        }
    }

    pub fn get(&'a self, k: &Ident, typevar_env: &'a mut TypeVarEnv) -> Option<&Type> {
        match self.env.get(k) {
            t @ Some(Type::TypeVar(i)) => {
                if let vt_t @ Some(_) = typevar_env.get_bind(*i) {
                    vt_t
                } else {
                    t
                }
            }
            t @ Some(_) => t,
            None => None,
        }
    }

    pub fn insert(&mut self, k: Ident, v: Type) -> Option<Type> {
        self.env.insert(k, v)
    }
}

impl Clone for TypingEnv {
    fn clone(&self) -> Self {
        Self {
            env: self.env.to_owned(),
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Int => write!(f, "int"),
            Type::Dec => write!(f, "dec"),
            Type::Bool => write!(f, "bool"),
            Type::TypeVar(i) => write!(f, "typevar({})", i),
            Type::Fun(a, r) => write!(f, "{} -> {}", a, r),
            Type::List(t) => write!(f, "[{}]", t),
        }
    }
}

fn compat(t1: &Type, t2: &Type, env: &mut TypingEnv, typevar_env: &mut TypeVarEnv) -> bool {
    match t1 {
        Type::Int => matches!(t2, &Type::Int),
        Type::Dec => matches!(t2, &Type::Dec),
        Type::Bool => matches!(t2, &Type::Bool),
        Type::TypeVar(i) => match t2 {
            Type::TypeVar(j) => {
                match (
                    typevar_env.to_owned().get_bind(*i),
                    typevar_env.to_owned().get_bind(*j),
                ) {
                    (None, None) => true,
                    (None, Some(r)) => {
                        typevar_env.bind(*i, r);
                        true
                    }
                    (Some(l), None) => {
                        typevar_env.bind(*j, l);
                        true
                    }
                    (Some(l), Some(r)) => compat(l, r, env, typevar_env),
                }
            }
            t => {
                typevar_env.bind(*i, t);
                true
            }
        },
        Type::Fun(a1_t, r1_t) => match t2 {
            Type::Fun(a2_t, r2_t) => {
                compat(a1_t, a2_t, env, typevar_env) && compat(r1_t, r2_t, env, typevar_env)
            }
            _ => false,
        },
        Type::List(l1_t) => match t2 {
            Type::List(l2_t) => compat(l1_t, l2_t, env, typevar_env),
            _ => false,
        },
    }
}

fn op_compat(
    op: &BinaryOperator,
    t1: &Type,
    t2: &Type,
    env: &mut TypingEnv,
    typevar_env: &mut TypeVarEnv,
) -> bool {
    match op {
        BinaryOperator::Plus => {
            compat(t1, &Type::Int, env, typevar_env) && compat(t2, &Type::Int, env, typevar_env)
        }
        BinaryOperator::FPlus => {
            compat(t1, &Type::Dec, env, typevar_env) && compat(t2, &Type::Dec, env, typevar_env)
        }
        BinaryOperator::Minus => {
            compat(t1, &Type::Int, env, typevar_env) && compat(t2, &Type::Int, env, typevar_env)
        }
        BinaryOperator::FMinus => {
            compat(t1, &Type::Dec, env, typevar_env) && compat(t2, &Type::Dec, env, typevar_env)
        }
        BinaryOperator::Mul => {
            compat(t1, &Type::Int, env, typevar_env) && compat(t2, &Type::Int, env, typevar_env)
        }
        BinaryOperator::FMul => {
            compat(t1, &Type::Dec, env, typevar_env) && compat(t2, &Type::Dec, env, typevar_env)
        }
        BinaryOperator::Div => {
            compat(t1, &Type::Int, env, typevar_env) && compat(t2, &Type::Int, env, typevar_env)
        }
        BinaryOperator::FDiv => {
            compat(t1, &Type::Dec, env, typevar_env) && compat(t2, &Type::Dec, env, typevar_env)
        }
        BinaryOperator::Lt => {
            (compat(t1, &Type::Int, env, typevar_env) && compat(t2, &Type::Int, env, typevar_env))
                || (compat(t1, &Type::Dec, env, typevar_env)
                    && compat(t2, &Type::Dec, env, typevar_env))
        }
        BinaryOperator::Le => {
            (compat(t1, &Type::Int, env, typevar_env) && compat(t2, &Type::Int, env, typevar_env))
                || (compat(t1, &Type::Dec, env, typevar_env)
                    && compat(t2, &Type::Dec, env, typevar_env))
        }
        BinaryOperator::Gt => {
            (compat(t1, &Type::Int, env, typevar_env) && compat(t2, &Type::Int, env, typevar_env))
                || (compat(t1, &Type::Dec, env, typevar_env)
                    && compat(t2, &Type::Dec, env, typevar_env))
        }
        BinaryOperator::Ge => {
            (compat(t1, &Type::Int, env, typevar_env) && compat(t2, &Type::Int, env, typevar_env))
                || (compat(t1, &Type::Dec, env, typevar_env)
                    && compat(t2, &Type::Dec, env, typevar_env))
        }
        BinaryOperator::Eq => {
            (compat(t1, &Type::Int, env, typevar_env) && compat(t2, &Type::Int, env, typevar_env))
                || (compat(t1, &Type::Dec, env, typevar_env)
                    && compat(t2, &Type::Dec, env, typevar_env))
                || (compat(t1, &Type::Bool, env, typevar_env)
                    && compat(t2, &Type::Bool, env, typevar_env))
        }
        BinaryOperator::Neq => {
            (compat(t1, &Type::Int, env, typevar_env) && compat(t2, &Type::Int, env, typevar_env))
                || (compat(t1, &Type::Dec, env, typevar_env)
                    && compat(t2, &Type::Dec, env, typevar_env))
                || (compat(t1, &Type::Bool, env, typevar_env)
                    && compat(t2, &Type::Bool, env, typevar_env))
        }
        BinaryOperator::And => {
            compat(t1, &Type::Bool, env, typevar_env) && compat(t2, &Type::Bool, env, typevar_env)
        }
        BinaryOperator::Or => {
            compat(t1, &Type::Bool, env, typevar_env) && compat(t2, &Type::Bool, env, typevar_env)
        }
    }
}

fn typecheck_unaryop(op: &UnaryOperator, _env: &TypingEnv) -> Result<Type, String> {
    match op {
        UnaryOperator::Neg => Ok(Type::Int),
        UnaryOperator::FNeg => Ok(Type::Dec),
    }
}

fn typecheck_binaryop(op: &BinaryOperator, _env: &TypingEnv) -> Result<Type, String> {
    match op {
        BinaryOperator::Plus => Ok(Type::Int),
        BinaryOperator::FPlus => Ok(Type::Dec),
        BinaryOperator::Minus => Ok(Type::Int),
        BinaryOperator::FMinus => Ok(Type::Dec),
        BinaryOperator::Mul => Ok(Type::Int),
        BinaryOperator::FMul => Ok(Type::Dec),
        BinaryOperator::Div => Ok(Type::Int),
        BinaryOperator::FDiv => Ok(Type::Dec),
        BinaryOperator::Lt => Ok(Type::Bool),
        BinaryOperator::Le => Ok(Type::Bool),
        BinaryOperator::Gt => Ok(Type::Bool),
        BinaryOperator::Ge => Ok(Type::Bool),
        BinaryOperator::Eq => Ok(Type::Bool),
        BinaryOperator::Neq => Ok(Type::Bool),
        BinaryOperator::And => Ok(Type::Bool),
        BinaryOperator::Or => Ok(Type::Bool),
    }
}

fn typecheck_expr(
    expr: &Expr,
    env: &mut TypingEnv,
    typevar_env: &mut TypeVarEnv,
) -> Result<Type, String> {
    match expr {
        Expr::Ident(id) => match env.get(id, typevar_env) {
            Some(t) => Ok(t.to_owned()),
            None => Err(format!("Ident {} not present in env", id)),
        },
        Expr::Integer(_) => Ok(Type::Int),
        Expr::Decimal(_) => Ok(Type::Dec),
        Expr::Boolean(_) => Ok(Type::Bool),
        Expr::EmptyList => Ok(Type::List(Box::new(typevar_env.next()))),
        Expr::UnaryOp(op, expr) => {
            let op_t = typecheck_unaryop(op, env)?;
            let expr_t = typecheck_expr(expr, env, typevar_env)?;
            if compat(&op_t, &expr_t, env, typevar_env) {
                Ok(op_t)
            } else {
                Err(format!(
                    "Incompatible types in unop: {:?} != {:?}",
                    op_t, expr_t
                ))
            }
        }
        Expr::BinaryOp(op, l, r) => {
            let op_t = typecheck_binaryop(op, env)?;
            let l_t = typecheck_expr(l, env, typevar_env)?;
            let r_t = typecheck_expr(r, env, typevar_env)?;
            if op_compat(op, &l_t, &r_t, env, typevar_env) {
                Ok(op_t)
            } else {
                return Err(format!(
                    "Incompatible types in binop: {:?} {:?} {:?}",
                    l_t, op_t, r_t
                ));
            }
        }
        Expr::Cons(hd, tl) => {
            let hd_t = typecheck_expr(hd, env, typevar_env)?;
            let tl_t = typecheck_expr(tl, env, typevar_env)?;
            if let Type::List(l_t) = &tl_t {
                if compat(&hd_t, l_t, env, typevar_env) {
                    return Ok(Type::List(Box::new(hd_t)));
                }
            } else if let t @ Type::TypeVar(_) = &tl_t {
                if compat(t, &hd_t, env, typevar_env) {
                    return Ok(Type::List(Box::new(t.to_owned())));
                }
            }
            Err(format!(
                "Incompatible types in cons: {:?}::{:?}",
                hd_t, tl_t
            ))
        }
        Expr::IfThenElse(cond, then_expr, else_expr) => {
            let cond_t = typecheck_expr(cond, env, typevar_env)?;
            let then_t = typecheck_expr(then_expr, env, typevar_env)?;
            let else_t = typecheck_expr(else_expr, env, typevar_env)?;

            if !compat(&cond_t, &Type::Bool, env, typevar_env) {
                return Err(format!("If condition must be bool, got {:?}", cond_t));
            }
            if compat(&then_t, &else_t, env, typevar_env) {
                Ok(then_t)
            } else {
                Err(format!(
                    "Incompatible types in if expr: {:?} != {:?}",
                    then_t, else_t
                ))
            }
        }
        Expr::MatchWith(_, _) => todo!("???"),
        Expr::Fun(id, body) => {
            let arg_t = typevar_env.next();
            let mut new_env = env.to_owned();
            new_env.insert(id.to_string(), arg_t.to_owned());
            let body_t = typecheck_expr(body, &mut new_env, typevar_env)?;
            Ok(Type::Fun(Box::new(arg_t), Box::new(body_t)))
        }
        Expr::FunApp(fun, arg) => {
            let arg_t = typecheck_expr(arg, env, typevar_env)?;
            let fun_t = typecheck_expr(fun, env, typevar_env)?;
            if let Type::Fun(a_t, r_t) = fun_t {
                if compat(&a_t, &arg_t, env, typevar_env) {
                    Ok(*r_t)
                } else {
                    Err(format!(
                        "Incompatible types in function application: {} != {}",
                        arg_t, a_t
                    ))
                }
            } else if let Type::TypeVar(_) = fun_t {
                Ok(fun_t)
            } else {
                Err(format!("Function must have type Fun, got {:?}", fun_t))
            }
        }
        Expr::LetIn(id, bound, body) => {
            let bound_t = typecheck_expr(bound, env, typevar_env)?;
            let mut new_env = env.to_owned();
            new_env.insert(id.to_string(), bound_t);
            typecheck_expr(body, &mut new_env, typevar_env)
        }
        Expr::LetRecFunIn(_, _, _, _) => todo!(),
        Expr::LetRecIn(id, bound, body) => {
            let mut new_env = env.to_owned();
            let var_t = typevar_env.next();
            new_env.insert(id.to_string(), var_t);
            let bound_t = typecheck_expr(bound, &mut new_env, typevar_env)?;
            new_env.insert(id.to_string(), bound_t);
            typecheck_expr(body, &mut new_env, typevar_env)
        }
        Expr::Paren(e) => typecheck_expr(e, env, typevar_env),
    }
}

fn replace_bound_typevars(t: Type, typevar_env: &mut TypeVarEnv) -> Type {
    match t {
        Type::List(ref tv) => {
            if let Type::TypeVar(i) = *tv.to_owned() {
                if let Some(l_t) = typevar_env.get_bind(i) {
                    Type::List(Box::new(l_t.to_owned()))
                } else {
                    t
                }
            } else {
                t
            }
        }
        _ => t,
    }
}

pub fn typecheck(expr: &Expr) -> Result<Type, String> {
    let mut typevar_env = TypeVarEnv::new();
    let t = typecheck_expr(expr, &mut TypingEnv::new(), &mut typevar_env);
    if let Ok(ty) = t {
        Ok(replace_bound_typevars(ty, &mut typevar_env))
    } else {
        t
    }
}
