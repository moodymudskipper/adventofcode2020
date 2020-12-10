#' day 2
#'
#' @export
day2 <- function() {
  # for notes
  letter <- password <- count <- pos1 <- pos2 <- NULL

  # dependencies
  `%>%` <- magrittr::`%>%`
  unglue_data <- unglue::unglue_data

  # data
  input <- readLines("inst/extdata/day2.txt")

  # part 1
  part1 <-
    unglue_data(input, "{min}-{max} {letter}: {password}", convert = TRUE) %>%
    transform(count = mapply(
      function(l, ls) sum(l == ls),
      letter,
      strsplit(password, ""))) %>%
    subset(count >= min & count <= max) %>%
    nrow()

  # part 2
  part2 <-
    unglue_data(input, "{pos1}-{pos2} {letter}: {password}", convert = TRUE) %>%
    subset(mapply(
      function(l, ls, pos1, pos2) sum(l == ls[c(pos1,pos2)]) == 1,
      letter,
      strsplit(password, ""),
      pos1,
      pos2)) %>%
    nrow()

  list(part1 = part1, part2 = part2)
}
