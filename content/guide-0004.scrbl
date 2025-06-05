@title{Math Support}

@p{@code{tr} use @a['target: "_blank" 'href: "https://katex.org/"]{Katex} in two forms: @code["@m{...}"] and @code["@mm{...}"]. One for inline and one for block, e.g. @code["@m{a \\in \\R}"] produces @m{a \in \R}; @code["@mm{ \\int_{a}^{b} x^2 \\,dx }"] produces}
@mm{ \int_{a}^{b} x^2 \,dx }
@p{Except Katex, @code{tr} also supports backend LaTeX solution, you can put tikz program in @code["@tikzcd{...}"] block to draw quiver, for example}
@tikzcd{
	A && B \\
	\\
	&& C
	\arrow[from=1-1, to=1-3]
	\arrow[from=1-1, to=3-3]
	\arrow[from=1-3, to=3-3]
}
@p{Maybe @code{tr} can allow more TeX program in the future.}
