use pest::Span;
use pest_ast::FromPest;
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "tree.pest"]
pub struct TreeParser;

fn span_into_string(span: Span) -> String {
    span.as_str().to_string()
}

#[derive(Debug, Clone, FromPest)]
#[pest_ast(rule(Rule::title))]
pub struct Title {
    #[pest_ast(outer(with(span_into_string)))]
    pub title: String,
}

#[derive(Debug, Clone, FromPest)]
#[pest_ast(rule(Rule::title))]
pub struct P {
    #[pest_ast(outer(with(span_into_string)))]
    pub content: String,
}

#[derive(Debug, Clone, FromPest)]
#[pest_ast(rule(Rule::title))]
pub struct Ol {
    #[pest_ast(outer(with(span_into_string)))]
    pub content: String,
}

#[derive(Debug, Clone, FromPest)]
#[pest_ast(rule(Rule::title))]
pub struct Ul {
    #[pest_ast(outer(with(span_into_string)))]
    pub content: String,
}

#[derive(Debug, Clone, FromPest)]
#[pest_ast(rule(Rule::title))]
pub struct Li {
    #[pest_ast(outer(with(span_into_string)))]
    pub content: String,
}

#[derive(Debug, Clone, FromPest)]
#[pest_ast(rule(Rule::top_))]
pub enum Top {
    Title(Title),
    P(P),
    Ol(Ol),
    Ul(Ul),
    Li(Li),
}

#[derive(Debug, FromPest)]
#[pest_ast(rule(Rule::file))]
pub struct EFile {
    pub tops: Vec<Top>,
    _eoi: Eoi,
}

#[derive(Debug, FromPest)]
#[pest_ast(rule(Rule::EOI))]
struct Eoi {}
