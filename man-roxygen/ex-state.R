state <- stateSpec(
  name = "my_display",
  sort = list(state = "desc", county = "asc"),
  filter = list(
    county = list(regex = "Ben"),
    state = list(select = c("OR", "WA")),
    meanList = list(from = 50, to = 150)
  ),
  layout = list(nrow = 2, ncol = 4),
  labels = c("county", "state")
)

state <- validateState(state, checkDisplay = FALSE)

makeStateHash(state)
