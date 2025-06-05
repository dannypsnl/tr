@title{Setup}

@p{Steps to setup the project:}
@ol{
  @li{@code{git clone https://github.com/dannypsnl/tr.git} (or you could fork and maintain)}
  @li{Run @code{raco pkg install --auto} in the project}
  @li{Correct site information in @code{site.json}}
}
@p{Now you can update @code{*.scrbl} in @code{content/} directory as you wanted. Every @code{*.scrbl} is called a @em{card}, every card must have a unique address, its content mostly are just HTML, lookup at @a['href: "https://docs.racket-lang.org/scribble-pp/html-html.html"]{scribble HTML} for more information about what can be used. Further guide will teach you what's in the toolbox.}
