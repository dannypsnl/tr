#show link: underline

#let tree(title: str, url: str, taxon: none, body) = {

  heading(title, level: 2)
  link(url)[goto]
  body
}
