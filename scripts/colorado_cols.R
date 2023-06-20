colorado_colors <- c(
  `avs maroon` = "#6F263D",
  `avs blue` = "#236192",
  `broncos orange` = "#FB4F14",
  `broncos blue` = "#002244",
  `buffs gold` = "#CFB87C",
  `falcons blue` = "#003087",
  `flag red` = "#BF0A30",
  `flag blue` = "#002868",
  `flag gold` = "#FFD700",
  `nuggets yellow` = "#FEC524",
  `nuggets red` = "#8B2131",
  `plate green` = "#007235",
  `rockies purple` = "#333366"
)

colorado_cols <- function(...) {
  cols <- c(...)

  if (is.null(cols))
    return (colorado_colors)

  colorado_colors[cols]
}