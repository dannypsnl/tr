@title{Setup}

@p{Steps to setup the project:}
@ol{
  @li{@code{git clone https://github.com/dannypsnl/tr.git} (or you could fork and maintain)}
  @li{Run @code{raco pkg install --auto} in the project}
  @li{Correct site information in @code{site.json}}
}
@p{Now you can update @code{*.scrbl} in @code{content/} directory as you wanted. Each @code{*.scrbl} is called a @em{card}.}
@p{A card @code{addr.scrbl} (no matter its full path) has an address @code{addr}, and the address must be unique.}
@p{The content of card mostly are just HTML in @a['target: "_blank" 'href: "https://docs.racket-lang.org/scribble/reader.html"]{at-exp} form, @a['target: "_blank" 'href: "https://docs.racket-lang.org/scribble-pp/html-html.html"]{scribble HTML} describes more about what can be used.}
@p{Further guide will teach you what's in the toolbox.}
