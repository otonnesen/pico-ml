pub type Ident = String;

#[derive(Debug, Clone)]
pub enum UnaryOperator {
    Neg,
    FNeg,
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
}

#[derive(Debug, Clone)]
pub enum Pattern {
    Ident(Ident),
    EmptyList,
    Wildcard,
    Cons(Box<Pattern>, Box<Pattern>),
}

#[derive(Debug, Clone)]
pub enum Clause {
    Clause(Pattern, Expr),
    Cont(Box<Clause>, Box<Clause>),
}
