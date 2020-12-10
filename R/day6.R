#' day 6
#'
#' @export
day6 <- function() {
  # for notes
  . <- NULL

  # dependencies
  `%>%` <- magrittr::`%>%`

  # data
  input <- paste(readLines("inst/extdata/day6.txt"), collapse = "\n")

  # part 1
  part1 <-
    strsplit(input, "\n\n")[[1]] %>%
    gsub("\n", "", .) %>%
    strsplit("") %>%
    lapply(unique) %>%
    lengths() %>%
    sum()

  # part 2
  part2 <-
    strsplit(input, "\n\n")[[1]] %>%
    strsplit("\n") %>%
    sapply(function(x) {
      l <- length(x)
      tab <- strsplit(x, "") %>%
        unlist() %>%
        table()
      sum(tab  == l)
    }) %>%
    sum()

  list(part1 = part1, part2 = part2)
}
