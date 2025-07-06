# Input:
# - A beta matrix
# - x: The x-variable(s)
# - y: The y-variable(s)
# - exclude: Variables to be excluded from the plot
# Output:
# - A beta-matrix with only x, y, and mediators,
#   in ("non-reduced") column echelon form
fixed_beta <- function(
                beta_matrix,
                x = NULL,
                y = NULL,
                exclude = NULL) {

  # Always remove exclude first, if any

  if (!is.null(exclude)) {
    check_exclude(beta_matrix = beta_matrix,
                  exclude = exclude)
    i <- match(exclude, colnames(beta_matrix))
    i <- i[!is.na(i)]
    if (length(i) > 0) {
      beta1 <- beta_matrix[-i, -i]
    } else {
      beta1 <- beta_matrix
    }
  } else {
    beta1 <- beta_matrix
  }

  # Drop orphan variable(s)

  i <- colSums(beta1)
  j <- rowSums(beta1)
  tmp <- (i == 0) & (j == 0)
  beta1 <- beta1[!tmp, !tmp]

  if (is.null(x)) {
    # Determine x automatically
    x <- x_from_beta(beta1)
    if (length(x) == 0) {
      stop("The model has no x-variable(s).")
    }
  }

  if (is.null(y)) {
    # Determine y automatically
    y <- y_from_beta(beta1)
    if (length(y) == 0) {
      stop("The model has no y-variable(s).")
    }
  }

  # Sanity checks

  bnames <- colnames(beta1)
  y <- intersect(y, bnames)
  x <- intersect(x, bnames)
  if (length(y) == 0) {
    stop("All y(s) not in the model.")
  }
  if (length(x) == 0) {
    stop("All x(s) not in the model.")
  }

  # Reorder the rows and columns
  i <- c(x, setdiff(bnames, c(x, y)), y)
  beta1 <- beta1[i, i]
  beta1 <- lower_e(beta1)
  beta1
}

# Input:
# - A beta matrix
# Output:
# - A matrix in ("non-reduced") column echelon form
lower_e <- function(m) {

  allnames <- colnames(m)
  m1 <- m
  x0 <- character(0)

  # Reorder the rows and columns
  # y variable(s) last
  a <- colSums(m1)
  y0 <- colnames(m1)[a == 0]
  i <- c(setdiff(allnames, y0), y0)
  m1 <- m1[i, i]

  # up0 <- all((m1[upper.tri(m1)]) == 0)
  while (length(x0) < ncol(m)) {
    j <- setdiff(allnames, c(x0, y0))
    if (length(j) == 0) break
    a <- rowSums(m1[j, j, drop = FALSE])
    x0 <- c(x0, names(a)[(a == 0)])
    i <- c(x0, setdiff(allnames, c(x0, y0)), y0)
    m1 <- m1[i, i, drop = FALSE]
  }
  m1
}

# Input:
# - A beta-matrix with only x, y, and mediators,
#   in ("non-reduced") column echelon form
# Output:
# - A list of character vectors of columns of variables
column_list <- function(beta_matrix) {
  out <- list()
  m <- beta_matrix
  i <- which(colSums(m) == 0)
  if (length(i) > 0) {
    out_last <- list(colnames(m)[i])
    m <- m[-i, -i]
  } else {
    out_last <- list()
  }
  while (nrow(m) > 0) {
    tmp <- rowSums(m)
    i <- which(tmp == 0)
    x <- colnames(m)[i]
    out <- c(out, list(x))
    m <- m[-i, -i, drop = FALSE]
  }
  out <- c(out, out_last)
  out
}

# Input:
# - A list of character vectors,
#   such as the output of column_list().
# Output:
# - A layout x-y matrix
c_list_to_layout <- function(
                      c_list,
                      v_pos = c("middle", "lower", "upper")
                    ) {
  v_pos <- match.arg(v_pos)
  f <- function(i) {
    xx <- c_list[[i]]
    v_i <- switch(v_pos,
                  upper = seq_along(xx) - 1,
                  lower = seq_along(xx) - length(xx),
                  middle = (seq_along(xx) - 1) - (length(xx) - 1) / 2)
    out <- cbind(i, v_i)
    rownames(out) <- xx
    colnames(out) <- c("x", "y")
    out
  }
  out <- sapply(
          seq_along(c_list),
          f,
          simplify = FALSE
        )
  out <- do.call(rbind,
                 out)
  attr(out, "v_pos") <- v_pos
  out
}

# Input:
# - A layout x-y matrix, such as the output of
#   c_list_to_layout().
# - A beta matrix. Required to identify
#   paths in the model.
# Output:
# - A modified layout x-y matrix
fix_mxy <- function(
              m,
              beta,
              v_preference = c("upper", "lower")
            ) {
  v_pos <- attr(m, "v_pos")
  v_preference <- switch(
                    v_pos,
                    lower = "lower",
                    upper = "upper",
                    middle = match.arg(v_preference))
  m_new <- m
  mnames <- rownames(m)
  for (i in unique(m_new[, "x", drop = TRUE])[-1]) {
    m_i <- mnames[m[, "x"] == i]
    x_i <- mnames[m[, "x"] < i]
    y_i <- mnames[m[, "x"] > i]
    lines_i <- all_lines(
                  m = m_new,
                  from = x_i,
                  to = y_i
                )
    k0 <- length(m_i)
    # m_i_lower and m_i_upper used
    # when v_pos = "middle"
    if (k0 > 1) {
      if (((k0 %% 2) == 0)) {
        # Even
        m_i_lower <- m_i[seq(1, k0 / 2)]
        m_i_upper <- m_i[seq(k0 / 2 + 1, length(m_i))]
      } else {
        # Odd
        if (v_preference == "upper") {
          m_i_lower <- m_i[seq(1, floor(k0 / 2))]
          m_i_upper <- m_i[seq(floor(k0 / 2) + 1, length(m_i))]
        } else {
          m_i_lower <- m_i[seq(1, ceiling(k0 / 2))]
          m_i_upper <- m_i[seq(ceiling(k0 / 2) + 1, length(m_i))]
        }
      }
    } else {
      if (v_preference == "upper") {
        m_i_lower <- character(0)
        m_i_upper <- m_i
      } else {
        m_i_lower <- m_i
        m_i_upper <- character(0)
      }
    }
    # delta_lower and delta_upper used only
    # if v_pos is "middle"
    delta_lower <- switch(
                  v_pos,
                  upper = 0.5,
                  lower = -0.5,
                  middle = -0.5
                )
    delta_upper <- switch(
                  v_pos,
                  upper = 0.5,
                  lower = -0.5,
                  middle = 0.5
                )
    delta <- switch(
                v_pos,
                upper = 0.5,
                lower = -0.5,
                middle = NA)
    if (v_pos == "middle") {
      for (m_ii in c("lower", "upper")) {
        m_ij <- switch(m_ii,
                       lower = m_i_lower,
                       upper = m_i_upper)
        if (length(m_ij) == 0) next
        chk <- check_pass_thru(
                        m_i = m_ij,
                        m = m_new,
                        lines_i = lines_i
                      )
        ok <- all(unlist(chk) != 0)
        delta_ij <- switch(m_ii,
                           lower = delta_lower,
                           upper = delta_upper)
        while (!ok) {
          m_new[m_ij, "y"] <- m_new[m_ij, "y"] + delta_ij
          chk <- check_pass_thru(
                          m_i = m_ij,
                          m = m_new,
                          lines_i = lines_i
                        )
          ok <- all(unlist(chk) != 0)
        }
      }
    } else {
      # v_pos is "lower" or "upper"
      chk <- check_pass_thru(
                      m_i = m_i,
                      m = m_new,
                      lines_i = lines_i
                    )
      ok <- all(unlist(chk) != 0)
      while (!ok) {
        m_new[m_i, "y"] <- m_new[m_i, "y"] + delta
        chk <- check_pass_thru(
                        m_i = m_i,
                        m = m_new,
                        lines_i = lines_i
                      )
        ok <- all(unlist(chk) != 0)
      }
    }
  }
  m_new
}

# Input:
# - m: Layout x-y matrix
# - from: Lines from
# - to: Lines to
# Output:
# - A list of equations
all_lines <- function(m,
                      from,
                      to) {
  out <- vector("list", length(from) * length(to))
  i <- 1
  for (p1 in from) {
    for (p2 in to) {
      a <- m[p1, "y"] - m[p2, "y"]
      b <- m[p2, "x"] - m[p1, "x"]
      c <- m[p1, "x"] * m[p2, "y"] -
           m[p2, "x"] * m[p1, "y"]
      out[[i]] <- c(a = a, b = b, c = c)
      i <- i + 1
    }
  }
  out
}

# Input:
# - m_i: The names of the mediators
# - m: The layout matrix in x-y form
# - lines_i: The a, b, c for the lines that may pass through m_i
# Output:
#- A list of vectors. If 0, a mediator is on a line.
check_pass_thru <- function(
                      m_i,
                      m,
                      lines_i) {
  out <- vector("list", length(m_i))
  names(out) <- m_i
  for (mm in m_i) {
    chk <- sapply(
              lines_i,
              function(xx) {
                xx["a"] * m[mm, "x"] +
                xx["b"] * m[mm, "y"] +
                xx["c"]
              })
    out[[mm]] <- chk
  }
  out
}


# Convert a x-y matrix to a
# semPlot layout matrix
layout_matrix_from_mxy <- function(
                            m
                          ) {
  out0 <- m
  y <- out0[, "y", drop = TRUE]
  y <- to_integer(y)
  y <- y - max(y)
  y <- y * -1 + 1
  y <- tryCatch(y / gcd_k(y),
                error = function(e) y)
  out0[, "y"] <- y

  x <- out0[, "x", drop = TRUE]
  x <- to_integer(x)
  x <- x - min(x) + 1
  x <- tryCatch(x / gcd_k(x),
                error = function(e) x)
  out0[, "x"] <- x

  out0 <- out0[, c("y", "x")]
  out0 <- split(out0, rownames(out0))
  do.call(layout_matrix,
          out0)
}

gcd_k <- function(x) {
  for (i in seq_along(x[-1]) + 1) {
    if (i == 2) {
      out <- gcd_2(x[i], x[i])
    } else {
      out <- gcd_2(out, x[i])
    }
    out <- unname(out)
  }
  out
}

gcd_2 <- function(x, y) {
  # Based on https://stackoverflow.com/a/21504113/4085819
  r <- x %% y
  return(ifelse(r,
                Recall(y, r),
                y))
}

to_integer <- function(x) {
  ok <- isTRUE(all.equal(round(x), x))
  k <- 1
  x0 <- x * k
  while (!ok || k > 100) {
    k <- k + 1
    x0 <- x * k
    ok <- isTRUE(all.equal(round(x0), x0))
  }
  round(x0)
}

# Input:
# - A qgraph object
# Output:
# - A beta matrix
qgraph_to_beta <- function(object) {
  # TODO:
  # - What to do with loadings?
  # - What to do with intercepts?
  e <- object$Edgelist
  i_directed <- (e$directed & !e$bidirectional)
  i_from <- e$from[i_directed]
  i_to <- e$to[i_directed]
  ge <- object$graphAttributes$Edges
  gn <- object$graphAttributes$Nodes
  in_beta <- sort(unique(c(i_from, i_to)))
  node_names <- unlist(gn$names)[in_beta]
  p <- length(node_names)
  out <- matrix(0, p, p)
  colnames(out) <- rownames(out) <- node_names
  i_from <- node_names[i_from]
  i_to <- node_names[i_to]
  for (i in seq_along(i_from)) {
    out[i_to[i], i_from[i]] <- 1
  }
  out
}

# Input:
# - A beta matrix
# Output:
# - A character vector of "pure" x
x_from_beta <- function(
                beta_matrix
              ) {
  i <- rowSums(beta_matrix)
  x <- colnames(beta_matrix)[i == 0]
  x
}

# Input:
# - A beta matrix
# Output:
# - A character vector of "pure" y
y_from_beta <- function(
                  beta_matrix
                ) {
  i <- rowSums(beta_matrix)
  j <- colSums(beta_matrix)
  y <- colnames(beta_matrix)[(j == 0) & (i > 0)]
  y
}

# Input:
# - A beta matrix
# Output:
# - A character vector of mediators
m_from_beta <- function(
                  beta_matrix
                ) {
  i <- rowSums(beta_matrix)
  j <- colSums(beta_matrix)
  m <- colnames(beta_matrix)[(j > 0) & (i > 0)]
  m
}

# Input:
# - A beta matrix
# - A vector of variables to be dropped
check_exclude <- function(
                beta_matrix,
                exclude
              ) {
  m0 <- m_from_beta(beta_matrix)
  if (length(intersect(exclude, m0)) > 0) {
    stop("One or more variables in 'exclude' is/are mediators and should not be excluded.")
  }
}

# Input:
# - A qgraph object
# Output:
# - A qgraph object with all directed paths no curvature
make_straight <- function(object) {
  e2 <- object$Edgelist
  i1 <- !e2$bidirectional
  i2 <- (e2$from != e2$to)
  i <- i1 & i2
  if (any(i)) {
    object$graphAttributes$Edges$curve[i] <- 0
  }
  object
}

# Input:
# - A qgraph object
# Output:
# - Logical
has_intercept <- function(object) {
  "triangle" %in% object$graphAttributes$Nodes$shape
}

# Input:
# - A qgraph object
# Output:
# - Logical
is_multigroup_qgraph <- function(object) {
  all(sapply(object, \(x) inherits(x, "qgraph")))
}