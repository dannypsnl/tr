@title{Test tikzcd}

@p{First quiver!}
@tikzcd{
	A && B \\
	\\
	&& C
	\arrow[from=1-1, to=1-3]
	\arrow[from=1-1, to=3-3]
	\arrow[from=1-3, to=3-3]
}
