#' day 13
#'
#' @export
day17 <- function() {
  input <- c("####...#",
             "......#.",
             "#..#.##.",
             ".#...#.#",
             "..###.#.",
             "##.###..",
             ".#...###",
             ".##....#")

  data <- do.call(rbind, strsplit(input, "")) == "#"

  # a wrapper with better arg order and defaults
  replicate2 <- function(x, n) replicate(n, x, simplify = FALSE)

  # convert list to dots to avoid horrible do.call calls
  as_dots <- function(x) {
    f <- function(...) environment()$...
    do.call(f, as.list(x))
  }

  solve <- function(data0, n_dim, n_cycles) {
    # create empty array, we make it cubic to simplify later computations
    side0 <- nrow(data0)
    side  <- 2*n_cycles + side0 + 2
    mid <- 2:(side-1) # indices to build central square/cube
    data <- array(dim = rep(side, n_dim)) & FALSE

    # fill in initial data
    init_ind <- c(
      replicate2(n_cycles+1+seq(side0), 2),   # skip margins
      rep(n_cycles+1+(side0+1)/2, n_dim - 2)) # keep central layer
    `...` <- as_dots(init_ind)
    data[...] <- data0

    for(i in seq(n_cycles)) {
      # build offset cubes
      `...` <- as_dots(replicate2(1:3, n_dim))
      meta_indices <- expand.grid(...)

      `...` <- as_dots(meta_indices)
      offset_cubes <- Map(..., f = function(...){
        `...` <- as_dots(list(mid-1, mid, mid+1)[c(...)])
        data[...]})

      # compute active neighbours and update data
      `...` <- as_dots(replicate2(mid, n_dim))
      data_mid <- data[...]
      act_nb <- Reduce("+", offset_cubes) - data_mid

      `...` <- as_dots(replicate2(mid, n_dim))
      data[...] <- (data_mid & act_nb %in% 2:3) | (!data_mid & act_nb == 3)
    }
    sum(data)
  }

  list(part1 = solve(data, 3, 6), part2 =solve(data, 4, 6))
}
