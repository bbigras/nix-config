jj log -r@ -l1 --ignore-working-copy --no-graph --color always  -T '
  separate(" ",
    branches.map(|x| if(
        x.name().substr(0, 10).starts_with(x.name()),
        x.name().substr(0, 10),
        x.name().substr(0, 9) ++ "…")
      ).join(" "),
    tags.map(|x| if(
        x.name().substr(0, 10).starts_with(x.name()),
        x.name().substr(0, 10),
        x.name().substr(0, 9) ++ "…")
      ).join(" "),
    surround("\"","\"",
      if(
         description.first_line().substr(0, 24).starts_with(description.first_line()),
         description.first_line().substr(0, 24),
         description.first_line().substr(0, 23) ++ "…"
      )
    ),
    if(conflict, "conflict"),
    if(divergent, "divergent"),
    if(hidden, "hidden"),
  )
'
