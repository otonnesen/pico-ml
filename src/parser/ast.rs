pub type Ident = String;

#[derive(Debug, Clone)]
pub enum UnaryOperator {
    Neg,
    FNeg,
}

impl std::fmt::Display for UnaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnaryOperator::Neg => write!(f, "-"),
            UnaryOperator::FNeg => write!(f, "-."),
        }
    }
}

#[derive(Debug, Clone)]
pub enum BinaryOperator {
    Plus,
    FPlus,
    Minus,
    FMinus,
    Mul,
    FMul,
    Div,
    FDiv,
    Lt,
    Le,
    Gt,
    Ge,
    Eq,
    Neq,
    And,
    Or,
}

impl std::fmt::Display for BinaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinaryOperator::Plus => write!(f, "+"),
            BinaryOperator::FPlus => write!(f, "+."),
            BinaryOperator::Minus => write!(f, "-"),
            BinaryOperator::FMinus => write!(f, "-."),
            BinaryOperator::Mul => write!(f, "*"),
            BinaryOperator::FMul => write!(f, "*."),
            BinaryOperator::Div => write!(f, "/"),
            BinaryOperator::FDiv => write!(f, "/."),
            BinaryOperator::Lt => write!(f, "<"),
            BinaryOperator::Le => write!(f, "<="),
            BinaryOperator::Gt => write!(f, ">"),
            BinaryOperator::Ge => write!(f, ">="),
            BinaryOperator::Eq => write!(f, "=="),
            BinaryOperator::Neq => write!(f, "!="),
            BinaryOperator::And => write!(f, "&&"),
            BinaryOperator::Or => write!(f, "||"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Ident(Ident),
    Integer(i32),
    Decimal(f32),
    Boolean(bool),
    EmptyList,
    UnaryOp(UnaryOperator, Box<Expr>),
    BinaryOp(BinaryOperator, Box<Expr>, Box<Expr>),
    Cons(Box<Expr>, Box<Expr>),
    IfThenElse(Box<Expr>, Box<Expr>, Box<Expr>),
    MatchWith(Box<Expr>, Box<Clause>),
    Fun(Ident, Box<Expr>),
    FunApp(Box<Expr>, Box<Expr>),
    LetIn(Ident, Box<Expr>, Box<Expr>),
    LetRecFunIn(Ident, Ident, Box<Expr>, Box<Expr>),
    LetRecIn(Ident, Box<Expr>, Box<Expr>),
    Paren(Box<Expr>),
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Ident(id) => write!(f, "{}", id),
            Expr::Integer(i) => write!(f, "{}", i),
            Expr::Decimal(d) => write!(f, "{}", d),
            Expr::Boolean(b) => write!(f, "{}", b),
            Expr::EmptyList => write!(f, "[]"),
            Expr::UnaryOp(op, expr) => write!(f, "{}{}", op, *expr),
            Expr::BinaryOp(op, l, r) => write!(f, "{} {} {}", *l, op, *r),
            Expr::Cons(hd, tl) => write!(f, "{}::{}", *hd, *tl),
            Expr::IfThenElse(cond, if_expr, else_expr) => {
                write!(f, "if {} then {} else {}", *cond, *if_expr, *else_expr)
            }
            Expr::MatchWith(expr, clause) => write!(f, "match {} with {}", *expr, *clause),
            Expr::Fun(id, expr) => write!(f, "fun {} -> {}", id, *expr),
            Expr::FunApp(fun, arg) => write!(f, "{} {}", *fun, *arg),
            Expr::LetIn(id, bound, body) => write!(f, "let {} = {} in {}", id, *bound, *body),
            Expr::LetRecFunIn(_, _, _, _) => todo!(),
            Expr::LetRecIn(id, bound, body) => {
                write!(f, "let rec {} = {} in {}", id, *bound, *body)
            }
            Expr::Paren(expr) => write!(f, "({})", expr),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Pattern {
    Ident(Ident),
    EmptyList,
    Wildcard,
    Cons(Box<Pattern>, Box<Pattern>),
}

impl std::fmt::Display for Pattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Pattern::Ident(id) => write!(f, "{}", id),
            Pattern::EmptyList => write!(f, "[]"),
            Pattern::Wildcard => write!(f, "_"),
            Pattern::Cons(hd, tl) => write!(f, "{}::{}", *hd, *tl),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Clause {
    Clause(Pattern, Expr),
    Cont(Box<Clause>, Box<Clause>),
}

impl std::fmt::Display for Clause {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Clause::Clause(pat, expr) => write!(f, "{} -> {}", pat, expr),
            Clause::Cont(clause, cont) => write!(f, "{} | {}", clause, cont),
        }
    }
}
