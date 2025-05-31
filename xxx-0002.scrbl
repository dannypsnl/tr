#lang scribble/text
@(require scribble/html/html
          "tr.rkt")

@html{
  @body{
    @tr-title{Second}

    @p{second tree}

    @embed['type: "text/html" 'src: "xxx-0001.html"]
  }
}
