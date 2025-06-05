@title{Shell Commands}

@p{If you face error like @code{raco tr} cannot find such command, run @code{raco setup}.}
@ol{
  @li{Run @code{raco tr build} to build HTMLs}
  @li{Run @code{raco tr watch} to watch content changes and build on fly}
  @li{Run @code{make serve} to view result}
  @li{
    Run @code{raco tr next <prefix>} to get next available address, for example, you might like to run
    @pre{vi $(raco tr next xxx).scrbl}
    to work on a new file (replace @code{vi} with any command line editor command)
  }
}
@p{You are very likely wanted to change @code{make deploy}, the output directory is @code{_build}, upload the directory to any host you want to use.}
