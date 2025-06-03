@title{TR}

@p{@code{tr} is a site generator based on a collection of scribbles & racket programs.}
@p{Dependencies of the tool:}
@ul{
  @li{racket 8.17 (with package @code{dirname}/@code{json})}
  @li{MacTex}
  @li{dvisvgm}
}
@p{Steps to use this project:}
@ol{
  @li{@code{git clone https://github.com/dannypsnl/tr.git}}
  @li{Run @code{raco pkg install --auto} in the project}
  @li{Correct site information in @code{rss.scrbl}}
  @li{Update @code{*.scrbl} in @code{content/} directory}
  @li{Run @{make build} to build HTMLs}
  @li{Run @{make serve} to view result}
}
