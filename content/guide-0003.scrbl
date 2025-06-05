@title{Link to other cards}

@p{You can use @code["@mention{addr}"] to mention a card, it will tell @code{addr} it has a backlinks, e.g. I can mention the guide itself @mention{guide-0000}. If the mentioned card has taxon @code{Reference}, it will not be a related card, but a references card, let me mention a cool book as an example @mention{10print}.}
@p{You can use @code["@transclude{addr}"] to include a card, it will tell @code{addr} it has a context. This guide itself is an example of transclude.}

@h2{Embed external web page}
@p{Since you can use any HTML, @code{iframe} for sure is counted, you can embed external web page, e.g.}
@iframe[
  'src: "https://en.wikipedia.org/wiki/Riemann_surface"
  'height: "500"
]
