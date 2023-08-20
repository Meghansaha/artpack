
square_packer <- function(n, min_x = 0, max_x = 100, min_y = 0, max_y = 100,
                          big_r = 5, med_r = 2, small_r = 1,
                          color_pal = NULL, color_type = "regular", angles = FALSE) {
  if (!is.numeric(big_r)) {
    stop("`size` must be numeric.") # nocov
  } else if (big_r <= 0) {
    stop("`size` must be greater than zero.") # nocov
  }

  if (!is.numeric(med_r)) {
    stop("`size` must be numeric.") # nocov
  } else if (med_r <= 0) {
    stop("`size` must be greater than zero.") # nocov
  }

  if (!is.numeric(small_r)) {
    stop("`size` must be numeric.") # nocov
  } else if (small_r <= 0) {
    stop("`size` must be greater than zero.") # nocov
  }


  distance <- function(x1, y1, x2, y2) {
    sqrt(((x2 - x1)^2) + ((y2 - y1)^2))
  }
  # Big squares----
  big_iter <- 1:(n * .02)
  big_x <- c(x = sample(min_x + (big_r):max_x - (big_r), 1))
  big_y <- c(y = sample(min_y + (big_r):max_y - (big_r), 1))
  i <- 1
  tries <- 0
  repeat{
    x <- sample(min_x + (big_r):max_x - (big_r), 1)
    y <- sample(min_y + (big_r):max_y - (big_r), 1)
    logic <- map2(big_x, big_y, ~ distance(.x, .y, x, y)) >= big_r * 2
    if (sum(logic) == length(big_y)) {
      big_x <- append(big_x, c(x = x))
      big_y <- append(big_y, c(y = y))
      i <- i + 1
    } else {
      tries <- tries + 1
    }
    if (i == (length(big_iter) + 1)) {
      break
    }
    if (tries == 3000) {
      break
    }
  }
  new_iter <- 1:length(big_y)
  big_angles <- sample(0:360, length(big_y), replace = TRUE)

  if (length(big_x) != length(big_y)) {
    stop("length of big_x and big_y don't match.") # nocov
  } else if (length(big_y) != length(new_iter)) {
    stop("length of big_y and new_iter don't match.") # nocov
  } else if (length(new_iter) != length(big_angles)) {
    stop("length of new_iter and big_angles") # nocov
  }

  sq_coord <- .705 * big_r

  if (angles) {
    big_squares <- pmap(list(
      big_x,
      big_y,
      new_iter,
      big_angles
    ), ~ artpack::rotator(tibble(
      x = c(-sq_coord, sq_coord, sq_coord, -sq_coord, -sq_coord) + ..1,
      y = c(-sq_coord, -sq_coord, sq_coord, sq_coord, -sq_coord) + ..2,
      group = paste0("big_", ..3)
    ), ..4)) |>
      list_rbind()
  } else {
    big_squares <- pmap(list(
      big_x,
      big_y,
      new_iter
    ), ~ tibble(
      x = c(-sq_coord, sq_coord, sq_coord, -sq_coord, -sq_coord) + ..1,
      y = c(-sq_coord, -sq_coord, sq_coord, sq_coord, -sq_coord) + ..2,
      group = paste0("big_", ..3)
    )) |>
      list_rbind()
  }


  message("Big Squares Complete!")

  # med squares----
  med_iter <- 1:((n * .50) + 1)
  med_x <- c(x = sample(min_x + (med_r):max_x - (med_r), 1))
  med_y <- c(y = sample(min_y + (med_r):max_y - (med_r), 1))
  i <- 1
  tries <- 0
  repeat{
    x <- sample(min_x + (med_r):max_x - (med_r), 1)
    y <- sample(min_y + (med_r):max_y - (med_r), 1)

    logic <- map2(big_x, big_y, ~ distance(.x, .y, x, y)) >= big_r + med_r
    logic2 <- map2(med_x, med_y, ~ distance(.x, .y, x, y)) >= med_r * 2

    if ((sum(logic2) == length(med_y)) & (sum(logic) == length(big_y))) {
      med_x <- append(med_x, c(x = x))
      med_y <- append(med_y, c(y = y))
      i <- i + 1
    } else {
      tries <- tries + 1
    }
    if (i == (length(med_iter) + 1)) {
      break
    }
    if (tries == 10000) {
      break
    }
  }

  med_x <- med_x[-1]
  med_y <- med_y[-1]
  new_iter <- 1:length(med_y)
  med_angles <- sample(0:360, length(med_y), replace = TRUE)

  if (length(med_x) != length(med_y)) {
    stop("length of med_x and med_y don't match.") # nocov
  } else if (length(med_y) != length(new_iter)) {
    stop("length of med_y and new_iter don't match.") # nocov
  } else if (length(new_iter) != length(med_angles)) {
    stop("length of new_iter and med_angles") # nocov
  }


  sq_coord <- .705 * med_r

  if (angles) {
    med_squares <- pmap(list(
      med_x,
      med_y,
      new_iter,
      med_angles
    ), ~ artpack::rotator(tibble(
      x = c(-sq_coord, sq_coord, sq_coord, -sq_coord, -sq_coord) + ..1,
      y = c(-sq_coord, -sq_coord, sq_coord, sq_coord, -sq_coord) + ..2,
      group = paste0("med_", ..3)
    ), ..4)) |>
      list_rbind()
  } else {
    med_squares <- pmap(list(
      med_x,
      med_y,
      new_iter
    ), ~ tibble(
      x = c(-sq_coord, sq_coord, sq_coord, -sq_coord, -sq_coord) + ..1,
      y = c(-sq_coord, -sq_coord, sq_coord, sq_coord, -sq_coord) + ..2,
      group = paste0("med_", ..3)
    )) |>
      list_rbind()
  }

  message("Med squares Complete!")


  # small squares----
  small_iter <- 1:((n * .75) + 1)
  small_x <- c(x = sample(min_x + (small_r):max_x - (small_r), 1))
  small_y <- c(y = sample(min_y + (small_r):max_y - (small_r), 1))
  i <- 1
  tries <- 0
  repeat{
    x <- sample(min_x + (small_r):max_x - (small_r), 1)
    y <- sample(min_y + (small_r):max_y - (small_r), 1)

    logic <- map2(big_x, big_y, ~ distance(.x, .y, x, y)) >= big_r + small_r
    logic2 <- map2(med_x, med_y, ~ distance(.x, .y, x, y)) >= small_r + med_r
    logic3 <- map2(small_x, small_y, ~ distance(.x, .y, x, y)) >= small_r * 2

    if ((sum(logic3) == length(small_y)) & (sum(logic2) == length(med_y)) & (sum(logic) == length(big_y))) {
      small_x <- append(small_x, c(x = x))
      small_y <- append(small_y, c(y = y))
      i <- i + 1
    } else {
      tries <- tries + 1
    }
    if (i == (length(small_iter) + 1)) {
      break
    }
    if (tries == 9000) {
      break
    }
  }
  small_x <- small_x[-1]
  small_y <- small_y[-1]
  new_iter <- 1:length(small_y)
  small_angles <- sample(0:360, length(small_y), replace = TRUE)

  if (length(small_x) != length(small_y)) {
    stop("length of small_x and small_y don't match.") # nocov
  } else if (length(small_y) != length(new_iter)) {
    stop("length of small_y and new_iter don't match.") # nocov
  } else if (length(new_iter) != length(small_angles)) {
    stop("length of new_iter and small_angles") # nocov
  }


  sq_coord <- .705 * small_r

  if (angles) {
    small_squares <- pmap(list(
      small_x,
      small_y,
      new_iter,
      small_angles
    ), ~ artpack::rotator(tibble(
      x = c(-sq_coord, sq_coord, sq_coord, -sq_coord, -sq_coord) + ..1,
      y = c(-sq_coord, -sq_coord, sq_coord, sq_coord, -sq_coord) + ..2,
      group = paste0("small_", ..3)
    ), ..4)) |>
      list_rbind()
  } else {
    small_squares <- pmap(list(
      small_x,
      small_y,
      new_iter
    ), ~ tibble(
      x = c(-sq_coord, sq_coord, sq_coord, -sq_coord, -sq_coord) + ..1,
      y = c(-sq_coord, -sq_coord, sq_coord, sq_coord, -sq_coord) + ..2,
      group = paste0("small_", ..3)
    )) |>
      list_rbind()
  }

  message("Small squares Complete!")

  all_squares <- rbind(big_squares, med_squares, small_squares)

  all_squares <- all_squares |>
    group_by(group)

  if (!is.null(color_pal)) {
    group_ns <- all_squares |>
      group_size()

    total_groups <- length(group_ns)
    color_opts <- switch(color_type,
      "regular" = rep(colorRampPalette(color_pal)(total_groups), each = group_ns[1]),
      "reverse" = rev(rep(colorRampPalette(color_pal)(total_groups), each = group_ns[1])),
      "random" = rep(sample(colorRampPalette(color_pal)(total_groups)), each = group_ns[1]),
      stop(paste0("Invalid `color_type`: '", color_type, "' is not a valid option.\nPlease input one of the following: 'regular', 'reverse', or 'random'.")) # nocov
    )



    all_squares$fill <- color_opts
  }
  return(all_squares)
}
