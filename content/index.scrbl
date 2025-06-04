@title{TR}

@p{@code{tr} is a site generator based on a collection of scribbles & racket programs.}
@p{Dependencies of the tool:}
@ul{
  @li{racket 8.17}
  @li{MacTex}
  @li{dvisvgm}
  @li{deno}
}
@p{Steps to use this project:}
@ol{
  @li{@code{git clone https://github.com/dannypsnl/tr.git}}
  @li{Run @code{raco pkg install --auto} in the project}
  @li{Correct site information in @code{rss.scrbl}}
  @li{Update @code{*.scrbl} in @code{content/} directory}
  @li{Run @code{make build} to build HTMLs}
  @li{Run @code{make serve} to view result}
}
@p{You can use @kbd{Ctrl} (or @kbd{Meta} on MacOS) + @kbd{K} to toggle search bar.}
