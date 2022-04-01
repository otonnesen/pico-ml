use std::collections::HashMap;
use std::fmt::Display;

use crate::ast::{BinaryOperator, Expr, Ident, UnaryOperator};

#[derive(Debug, Clone)]
pub enum Type {
    Int,
    Dec,
    Bool,
    Any,
    Fun(Box<Type>, Box<Type>),
    List(Box<Type>),
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Int => write!(f, "int"),
            Type::Dec => write!(f, "dec"),
            Type::Bool => write!(f, "bool"),
            Type::Any => write!(f, "any"),
            Type::Fun(a, r) => write!(f, "{} -> {}", a, r),
            Type::List(t) => write!(f, "[{}]", t),
        }
    }
}

fn compat(t1: &Type, t2: &Type) -> bool {
    matches!(t2, &Type::Any)
        || match t1 {
            Type::Int => matches!(t2, &Type::Int),
            Type::Dec => matches!(t2, &Type::Dec),
            Type::Bool => matches!(t2, &Type::Bool),
            Type::Any => true,
            Type::Fun(a1_t, r1_t) => match t2 {
                Type::Fun(a2_t, r2_t) => compat(&*a1_t, &*a2_t) && compat(&*r1_t, &*r2_t),
                _ => false,
            },
            Type::List(l1_t) => match t2 {
                Type::List(l2_t) => compat(&*l1_t, &*l2_t),
                _ => false,
            },
        }
}

fn op_compat(op: &BinaryOperator, t1: &Type, t2: &Type) -> bool {
    match op {
        BinaryOperator::Plus => compat(t1, &Type::Int) && compat(t2, &Type::Int),
        BinaryOperator::FPlus => compat(t1, &Type::Dec) && compat(t2, &Type::Dec),
        BinaryOperator::Minus => compat(t1, &Type::Int) && compat(t2, &Type::Int),
        BinaryOperator::FMinus => compat(t1, &Type::Dec) && compat(t2, &Type::Dec),
        BinaryOperator::Mul => compat(t1, &Type::Int) && compat(t2, &Type::Int),
        BinaryOperator::FMul => compat(t1, &Type::Dec) && compat(t2, &Type::Dec),
        BinaryOperator::Div => compat(t1, &Type::Int) && compat(t2, &Type::Int),
        BinaryOperator::FDiv => compat(t1, &Type::Dec) && compat(t2, &Type::Dec),
        BinaryOperator::Lt => {
            (compat(t1, &Type::Int) && compat(t2, &Type::Int))
                || (compat(t1, &Type::Dec) && compat(t2, &Type::Dec))
        }
        BinaryOperator::Le => {
            (compat(t1, &Type::Int) && compat(t2, &Type::Int))
                || (compat(t1, &Type::Dec) && compat(t2, &Type::Dec))
        }
        BinaryOperator::Gt => {
            (compat(t1, &Type::Int) && compat(t2, &Type::Int))
                || (compat(t1, &Type::Dec) && compat(t2, &Type::Dec))
        }
        BinaryOperator::Ge => {
            (compat(t1, &Type::Int) && compat(t2, &Type::Int))
                || (compat(t1, &Type::Dec) && compat(t2, &Type::Dec))
        }
        BinaryOperator::Eq => {
            (compat(t1, &Type::Int) && compat(t2, &Type::Int))
                || (compat(t1, &Type::Dec) && compat(t2, &Type::Dec))
                || (compat(t1, &Type::Bool) && compat(t2, &Type::Bool))
        }
        BinaryOperator::Neq => {
            (compat(t1, &Type::Int) && compat(t2, &Type::Int))
                || (compat(t1, &Type::Dec) && compat(t2, &Type::Dec))
                || (compat(t1, &Type::Bool) && compat(t2, &Type::Bool))
        }
        BinaryOperator::And => compat(t1, &Type::Bool) && compat(t2, &Type::Bool),
        BinaryOperator::Or => compat(t1, &Type::Bool) && compat(t2, &Type::Bool),
    }
}

fn typecheck_unaryop(op: &UnaryOperator, _env: &HashMap<Ident, Type>) -> Result<Type, String> {
    match op {
        UnaryOperator::Neg => Ok(Type::Int),
        UnaryOperator::FNeg => Ok(Type::Dec),
    }
}

fn typecheck_binaryop(op: &BinaryOperator, _env: &HashMap<Ident, Type>) -> Result<Type, String> {
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

fn typecheck_expr(expr: &Expr, env: &HashMap<Ident, Type>) -> Result<Type, String> {
    match expr {
        Expr::Ident(id) => match env.get(id) {
            Some(t) => Ok(t.clone()),
            None => Err(format!("Ident {} not present in env", id)),
        },
        Expr::Integer(_) => Ok(Type::Int),
        Expr::Decimal(_) => Ok(Type::Dec),
        Expr::Boolean(_) => Ok(Type::Bool),
        Expr::EmptyList => Ok(Type::List(Box::new(Type::Any))),
        Expr::UnaryOp(op, expr) => {
            let op_t = typecheck_unaryop(op, env)?;
            let expr_t = typecheck_expr(expr, env)?;
            if compat(&op_t, &expr_t) {
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
            let l_t = typecheck_expr(l, env)?;
            let r_t = typecheck_expr(r, env)?;
            if op_compat(&op, &l_t, &r_t) {
                Ok(op_t)
            } else {
                return Err(format!(
                    "Incompatible types in binop: {:?} {:?} {:?}",
                    l_t, op_t, r_t
                ));
            }
        }
        Expr::Cons(hd, tl) => {
            let hd_t = typecheck_expr(hd, env)?;
            let tl_t = typecheck_expr(tl, env)?;
            if let Type::List(l_t) = &tl_t {
                if compat(&hd_t, &l_t) {
                    return Ok(Type::List(Box::new(hd_t)));
                }
            } else if let Type::Any = &tl_t {
                    return Ok(Type::List(Box::new(Type::Any)));
                }
            Err(format!(
                "Incompatible types in cons: {:?}::{:?}",
                hd_t, tl_t
            ))
        }
        Expr::IfThenElse(cond, then_expr, else_expr) => {
            let cond_t = typecheck_expr(cond, env)?;
            let then_t = typecheck_expr(then_expr, env)?;
            let else_t = typecheck_expr(else_expr, env)?;

            if !compat(&cond_t, &Type::Bool) {
                return Err(format!("If condition must be bool, got {:?}", cond_t));
            }
            if compat(&then_t, &else_t) {
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
            let mut new_env = env.clone();
            new_env.insert(id.to_string(), Type::Any);
            let body_t = typecheck_expr(body, &new_env)?;
            Ok(Type::Fun(Box::new(Type::Any), Box::new(body_t)))
        }
        Expr::FunApp(fun, arg) => {
            // TODO: This has to be wrong
            let arg_t = typecheck_expr(arg, env)?;
            let fun_t = typecheck_expr(fun, env)?;
            if let Type::Fun(a_t, r_t) = fun_t {
                if compat(&a_t, &arg_t) {
                    Ok(*r_t)
                } else {
                    Err(format!(
                        "Incompatible types in function application: {} != {}",
                        arg_t, a_t
                    ))
                }
            } else if let Type::Any = fun_t {
                Ok(Type::Any)
            } else {
                println!("{:?}", env);
                Err(format!("Function must have type Fun, got {:?}", fun_t))
            }
        }
        Expr::LetIn(id, bound, body) => {
            let bound_t = typecheck_expr(bound, env)?;
            let mut new_env = env.clone();
            new_env.insert(id.to_string(), bound_t);
            typecheck_expr(body, &new_env)
        }
        Expr::LetRecFunIn(_, _, _, _) => todo!(),
        Expr::LetRecIn(id, bound, body) => {
            let mut new_env = env.clone();
            new_env.insert(id.to_string(), Type::Any);
            let bound_t = typecheck_expr(bound, &new_env)?;
            new_env.insert(id.to_string(), bound_t);
            typecheck_expr(body, &new_env)
        }
        Expr::Paren(e) => typecheck_expr(e, env),
    }
}

pub fn typecheck(expr: &Expr) -> Result<Type, String> {
    typecheck_expr(expr, &HashMap::new())
}
