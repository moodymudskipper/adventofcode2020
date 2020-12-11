
<!-- README.md is generated from README.Rmd. Please edit that file -->

# adventofcode2020

My solutions for adventofcode2020. You can install it with

``` r
remotes::install_github("moodymudskipper/adventofcode2020")
```

The package contains functions `day1`, `day2` etc, the readme displays
the body of these functions.

## day 1

    function() {
      # for notes
      Var1 <- Var2 <- Var3 <- . <- NULL
      # dependencies
      `%>%` <- magrittr::`%>%`
    
      # data
      input <-
        scan("inst/extdata/day1.txt", what = numeric(), sep = "\n", quiet = TRUE)
    
      # part 1
      part1 <-
        input %>%
        expand.grid(.,.) %>%
        subset(Var1 < Var2 & Var1+Var2 == 2020) %>%
        prod()
    
      # part 2
      part2 <-
        input %>%
        expand.grid(.,.,.) %>%
        subset(Var1 < Var2 & Var2 < Var3 & Var1+Var2+Var3 == 2020) %>%
        prod()
    
      list(part1 = part1, part2 = part2)
    }

## day 2

    function() {
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

## day 3

    function() {
      # data
      input <- readLines("inst/extdata/day3.txt")
    
      # part 1
    
      # make input a logical matrix of trees
      trees_mat <- as.matrix(do.call(rbind, strsplit(input, ""))) == "#"
      # make sure it's at least 3 times wider then long
      mult <- ceiling(3 * nrow(trees_mat) / ncol(trees_mat))
      trees_mat <- do.call(cbind, replicate(mult, trees_mat, simplify = FALSE))
      # transpose so we reaching bottom means reaching end of matrix
      trees_mat <- t(trees_mat)
      part1 <- sum(trees_mat[seq(1, length(trees_mat), 3 + nrow(trees_mat))])
    
      # part 2
    
      # same as part 1
      trees_mat <- as.matrix(do.call(rbind, strsplit(input, ""))) == "#"
    
      # wrap part 1 in function (we keep trees_map global)
      count_trees <- function(right, down) {
        mult <- ceiling(right * nrow(trees_mat) / ncol(trees_mat))
        trees_mat <- do.call(cbind, replicate(mult, trees_mat, simplify = FALSE))
        # transpose so we reaching bottom means reaching end of matrix
        trees_mat <- t(trees_mat)
        sum(trees_mat[seq(1, length(trees_mat), right + nrow(trees_mat)*down)])
      }
    
      part2 <- prod(mapply(count_trees, c(1,3,5,7,1), c(1,1,1,1,2)))
    
      list(part1 = part1, part2 = part2)
    }

## day 4

    function() {
      # for notes
      key <- value <- byr <- ecl <- eyr <- iyr <- pid <- hcl <- NULL
    
      # dependencies
      `%>%` <- magrittr::`%>%`
      map_dfr     <- purrr::map_dfr
      n_distinct  <- dplyr::n_distinct
      with_groups <- dplyr::with_groups
      filter      <- dplyr::filter
      case_when   <- dplyr::case_when
      between     <- dplyr::between
      spread      <- tidyr::spread
      unglue_data <- unglue::unglue_data
    
      # data
      input <- paste(readLines("inst/extdata/day4.txt"), collapse = "\n")
    
      # part1
      passports <- strsplit(input, "\n\n")[[1]]
      passports <- strsplit(passports, "\\s")
      passports <- map_dfr(passports, unglue_data, "{key}:{value}", .id = "id")
      valid_passports <-
        passports %>%
        with_groups("id", filter, all(c("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid") %in% key))
    
      part1 <- dplyr::n_distinct(valid_passports$id)
    
      # part2
      silent_as_numeric <- function(x) suppressWarnings(as.numeric(x))
    
      part2 <-
        valid_passports %>%
        filter(key != "cid") %>%
        spread(key, value) %>%
        filter(between(as.numeric(byr), 1920, 2002),
               between(as.numeric(iyr), 2010, 2020),
               between(as.numeric(eyr), 2020, 2030),
               #grepl("^(\\d{3}cm)|(\\d{2}in)$", hgt),
               case_when(
                 substr(hgt, 3,4) == "in" ~
                   between(silent_as_numeric(substr(hgt, 1,2)), 59, 76),
                 substr(hgt, 4,5) == "cm" ~
                   between(silent_as_numeric(substr(hgt, 1,3)), 150, 193),
                 TRUE ~ FALSE),
               grepl("^#[0-9a-f]{6}$", hcl),
               ecl %in% c("amb", "blu", "brn", "gry", "grn", "hzl", "oth"),
               grepl("^[0-9]{9}$", pid)
        ) %>%
        nrow()
    
    
      list(part1 = part1, part2 = part2)
    }

## day 5

    function() {
      # data
      input <- readLines("inst/extdata/day5.txt")
    
      # part 1
      all_ids <- strtoi(chartr("FBLR", "0101", input), base = 2)
      part1 <- max(all_ids)
    
      # part 2
      part2 <- tail(setdiff(seq(max(all_ids)), all_ids), 1)
      list(part1 = part1, part2 = part2)
    }

## day 6

    function() {
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

## day 7

    function() {
      # for notes
      key <- value <- color <- parent_color <- i <- NULL
    
      # dependencies
      `%>%` <- magrittr::`%>%`
      unglue_data <- unglue::unglue_data
      filter      <- dplyr::filter
      mutate_at   <- dplyr::mutate_at
      select      <- dplyr::select
      gather      <- tidyr::gather
      separate    <- tidyr::separate
      spread      <- tidyr::spread
    
      # data
      input <- readLines("inst/extdata/day7.txt")
    
      # part 1
      patterns <- c(
        "{parent_color} bags contain no other bags.",
        "{parent_color} bags contain {n_1} {color_1} bag{}, {n_2} {color_2} bag{}, {n_3} {color_3} bag{}, {n_4} {color_4} bag{}.",
        "{parent_color} bags contain {n_1} {color_1} bag{}, {n_2} {color_2} bag{}, {n_3} {color_3} bag{}.",
        "{parent_color} bags contain {n_1} {color_1} bag{}, {n_2} {color_2} bag{}.",
        "{parent_color} bags contain {n_1} {color_1} bag{}."
      )
    
      clean_data <-
        input %>%
        unglue_data(patterns) %>%
        gather(key,value,-1) %>%
        separate(key, c("col", "i")) %>%
        spread(col, value) %>%
        select(-i) %>%
        filter(!is.na(color)) %>%
        mutate_at("n", as.numeric)
    
      find_parents <- function(col) {
        direct_parents <- filter(clean_data, color %in% col)$parent_color
        if(length(direct_parents))
          unique(c(direct_parents, find_parents(direct_parents)))
        else
          character(0)
      }
    
      part1 <- length(find_parents("shiny gold"))
    
      # part 2
      count_children <- function(col) {
        children <- filter(clean_data, parent_color == col)
        if(nrow(children))
          sum(children$n) + sum(children$n * sapply(children$color, count_children))
        else
          0
      }
    
      part2 <- count_children("shiny gold")
    
      list(part1 = part1, part2 = part2)
    }

## day 8

    function() {
      # dependencies
      `%>%`       <- magrittr::`%>%`
      unglue_data <- unglue::unglue_data
      mutate      <- dplyr::mutate
    
      # data
      input <- readLines("inst/extdata/day8.txt")
    
      # part 1
      clean_data <- clean_data1 <-
        unglue_data(input, "{call} {value}", convert = TRUE) %>%
        mutate(passes = 0)
    
      i <- 1
      passes <- 0
      glob <- 0
      repeat {
        passes <- clean_data1[i, "passes"] + 1
        clean_data1[i, "passes"] <- passes
        if(passes == 2) break
        call  <- clean_data1[i, "call"]
        value <- clean_data1[i, "value"]
        if(call == "acc") {
          glob <- glob + value
          i <- i + 1
        } else if(call == "jmp") {
          i <- i + value
        } else {
          i <- i + 1
        }
      }
      part1 <- glob
    
      # part 2
    
      # return NULL if inf loop, glob otherwise
      test_code <- function(clean_data) {
        i <- 1
        passes <- 0
        glob <- 0
        repeat {
          passes <- clean_data[i, "passes"] + 1
          clean_data[i, "passes"] <- passes
          if(passes == 2) return(NULL)
          call  <- clean_data[i, "call"]
          value <- clean_data[i, "value"]
          if(call == "acc") {
            glob <- glob + value
            i <- i + 1
          } else if(call == "jmp") {
            i <- i + value
          } else {
            i <- i + 1
          }
          if(i == nrow(clean_data)) return(glob)
        }
      }
    
      for (j in seq(nrow(clean_data))) {
        call  <- clean_data[j, "call"]
        clean_data_modif <- clean_data
        if(call == "nop") {
          clean_data_modif[j, "call"] <- "jmp"
          res <- test_code(clean_data_modif)
          if(!is.null(res)) break
        } else   if(call == "jmp") {
          clean_data_modif[j, "call"] <- "nop"
          res <- test_code(clean_data_modif)
          if(!is.null(res)) break
        }
      }
      part2 <- res
    
      list(part1 = part1, part2 = part2)
    }

## day 9

    function() {
      # for notes
      . <- Var1 <- Var2 <- NULL
    
      # dependencies
      `%>%` <- magrittr::`%>%`
    
      # data
      input <-
        scan("inst/extdata/day9.txt", what = numeric(), sep = "\n", quiet = TRUE)
    
      # part 1
      i <- 26
      while(input[i] %in% (
        input[(i-25):(i-1)] %>% expand.grid(.,.) %>% subset(Var1 != Var2) %>% rowSums())
      ) i <- i +1
      part1 <- input[i]
    
      # part 2
      j <- 1
      while(!part1 %in% (tmp <- cumsum(input[j:(i-1)]))) j <- j+1
      range <- input[j:(which(part1 == tmp) + j - 1)]
      part2 <- max(range) + min(range)
    
      list(part1 = part1, part2 = part2)
    }

## day 10

    function() {
    
      # data
      input <- scan("inst/extdata/day10.txt", what = numeric(), sep = "\n", quiet = TRUE)
    
      # part 1
    
      diffs <- c(diff(c(0,sort(input))), 3)
      tab   <- tabulate(diffs)
      part1 <- prod(tab[c(1,3)])
    
      # jumps are 3 at most, and tab shows we have no jump of 2, thus we can
      # find rles of 1s and find all possible combinations of 1,2 and 3 that we
      # can build from those.
      # we find that the max running length of 1s is 4, small enough to do manually :
      # max(rle(diff(sort(input)))$lengths) # 4
      # 4 can be expressed in 7 ways :
      # 4 = 1 + 1 + 1 + 1
      # 4 = 1 + 1 + 2
      # 4 = 1 + 2 + 1
      # 4 = 2 + 1 + 1
      # 4 = 2 + 2
      # 4 = 1 + 3
      # 4 = 3 + 1
      # 3 can be expressed in 4 ways
      # 3 = 1 + 1 + 1
      # 3 = 1 + 2
      # 3 = 2 + 1
      # 3 = 3
      # 2 can be expressed in 2 ways
      # 2 = 1 + 1
      # 2 = 2
    
      # part 2
      multiplier <- c(1,2,4,7)
      rle_1s     <- with(rle(diffs), lengths[values == 1])
      part2 <- prod(multiplier[rle_1s])
      part2 <- format(part2, scientific = FALSE) # to see all digits
    
      list(part1 = part1, part2 = part2)
    }

## day 11

part 2 is wrong, works only on example.

    function() {
      # dependencies
      `%>%` <- magrittr::`%>%`
    
      # data
      input <- readLines("inst/extdata/day11.txt")
    
    #   # example
    #   input <- "L.LL.LL.LL
    # LLLLLLL.LL
    # L.L.L..L..
    # LLLL.LL.LL
    # L.LL.LL.LL
    # L.LLLLL.LL
    # ..L.L.....
    # LLLLLLLLLL
    # L.LLLLLL.L
    # L.LLLLL.LL"
    # input <- strsplit(input, "\n")[[1]]
      mat <- as.matrix(do.call(rbind, strsplit(input, "")))
      bkp_mat <-  mat
      nr <- nrow(mat)
      nc <- ncol(mat)
    
      # part 1
    
      repeat {
        bkp_mat1 <- mat
        taken_mat <- mat == "#"
        # build 8 padded matrices
        left_padded     <- cbind(FALSE, taken_mat[,-nc])
        right_padded    <- cbind(taken_mat[,-1], FALSE)
        top_padded      <- rbind(FALSE, taken_mat[-nr,])
        bottom_padded   <- rbind(taken_mat[-1,], FALSE)
        top_left_padded       <- rbind(FALSE, left_padded[-nr,])
        bottom_left_padded    <- rbind(left_padded[-1,], FALSE)
        top_right_padded      <- rbind(FALSE, right_padded[-nr,])
        bottom_right_padded   <- rbind(right_padded[-1,], FALSE)
    
        # sum `#` of padded matrices gives number of neighbouring `#`
        n_adj_taken <-
          left_padded + right_padded + top_padded + bottom_padded +
          top_left_padded + bottom_left_padded + top_right_padded + bottom_right_padded
    
        mat[] <- ifelse(mat == "L" & !n_adj_taken, "#",
                        ifelse(mat == "#" & n_adj_taken >= 4, "L", mat))
        if(identical(mat, bkp_mat1))
          break
      }
      part1 <- sum(mat == "#")
    
      # part 2
      mat <- bkp_mat
      # create an indicator for all diagonals in the matrix
      downward_diag_ind <- row(mat) - col(mat)
      upward_diag_ind   <- row(mat) + col(mat)
      mat_try_list <- list()
      repeat {
        bkp_mat2 <- mat
        mat_try_list[[length(mat_try_list) + 1]] <- mat
    
        # "#" must be contagious to rightmost "." for left_padded, and so on
        # we first build rles for all directions
        hor_rle <- apply(mat, 1, rle)
        ver_rle <- apply(mat, 2, rle)
        upward_diag_rle   <- lapply(split(mat, upward_diag_ind), rle)
        downward_diag_rle <- lapply(split(mat, downward_diag_ind), rle)
    
        # build 8 padded matrices
        left_padded     <-
          lapply(hor_rle, function(x) {
            l <- length(x$values)
          x$values[-1][x$values[-l] == "#" & x$values[-1] == "."] <- "#"
          inverse.rle(x)
        }) %>%
          do.call(rbind, .) %>%
          {cbind(".", .[,-nc])} %>%
          `==` ("#")
    
        right_padded     <-
          lapply(hor_rle, function(x) {
            l <- length(x$values)
            x$values[-l][x$values[-1] == "#" & x$values[-l] == "."] <- "#"
            inverse.rle(x)
          }) %>%
          do.call(rbind, .) %>%
          {cbind(.[,-1], ".")} %>%
          `==` ("#")
    
        top_padded     <-
          lapply(ver_rle, function(x) {
            l <- length(x$values)
            x$values[-1][x$values[-l] == "#" & x$values[-1] == "."] <- "#"
            inverse.rle(x)
          }) %>%
          do.call(cbind, .) %>%
          {rbind(".", .[-nc,])} %>%
          `==` ("#")
    
        bottom_padded     <-
          lapply(ver_rle, function(x) {
            l <- length(x$values)
            x$values[-l][x$values[-1] == "#" & x$values[-l] == "."] <- "#"
            inverse.rle(x)
          }) %>%
          do.call(cbind, .) %>%
          {rbind(.[-1,], ".")} %>%
          `==` ("#")
    
        top_left_padded        <- mat
        split(top_left_padded, downward_diag_ind) <-
          lapply(downward_diag_rle, function(x) {
            l <- length(x$values)
            x$values[-1][x$values[-l] == "#" & x$values[-1] == "."] <- "#"
            inverse.rle(x)
          })
        top_left_padded <-
          top_left_padded %>%
          {rbind(".", .[-nc,])} %>%
          {cbind(".", .[,-nc])} %>%
          `==` ("#")
    
        bottom_left_padded      <- mat
        split(bottom_left_padded, upward_diag_ind) <-
          lapply(upward_diag_rle, function(x) {
            l <- length(x$values)
            x$values[-1][x$values[-l] == "#" & x$values[-1] == "."] <- "#"
            inverse.rle(x)
          })
        bottom_left_padded <-
          bottom_left_padded %>%
          {rbind(.[-1,], ".")} %>%
          {cbind(".", .[,-nc])} %>%
          `==` ("#")
    
        top_right_padded        <- mat
        split(top_right_padded, upward_diag_ind) <-
          lapply(upward_diag_rle, function(x) {
            l <- length(x$values)
            x$values[-l][x$values[-1] == "#" & x$values[-l] == "."] <- "#"
            inverse.rle(x)
          })
        top_right_padded <-
          top_right_padded %>%
          {rbind(".", .[-nc,])} %>%
          {cbind(.[,-1], ".")} %>%
          `==` ("#")
    
        bottom_right_padded        <- mat
        split(bottom_right_padded, downward_diag_ind) <-
          lapply(downward_diag_rle, function(x) {
            l <- length(x$values)
            x$values[-l][x$values[-1] == "#" & x$values[-l] == "."] <- "#"
            inverse.rle(x)
          })
        bottom_right_padded <-
          bottom_right_padded %>%
          {rbind(.[-1,], ".")} %>%
          {cbind(.[,-1], ".")} %>%
          `==` ("#")
    
        n_adj_taken <- Reduce(`+`, list(
          left_padded, right_padded, top_padded, bottom_padded,
          top_left_padded, bottom_left_padded, top_right_padded, bottom_right_padded))
        #n_adj_taken <- F
        mat[] <- ifelse(mat == "L" & !n_adj_taken, "#",
                        ifelse(mat == "#" & n_adj_taken >= 5, "L", mat))
    
        if(identical(mat, bkp_mat2))
          break
      }
      part2 <- sum(mat == "#") # should not be 2219
    
      list(part1 = part1, part2 = part2)
    }
