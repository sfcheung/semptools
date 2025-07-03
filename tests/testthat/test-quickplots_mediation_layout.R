skip("WIP")

library(lavaan)
library(semPlot)

test_that("q_plot layout", {


# Input:
# - A beta-matrix with only x, y, and mediators,
#   in ("non-reduced") column echelon form
# Output:
# - A list o character vectors of columns of variables
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
# - A beta matrix
# - x: The x-variable
# - y: The y-variable
# - cov: Covariates to be excluded
# Output:
# - A beta-matrix with only x, y, and mediators,
#   in ("non-reduced") column echelon form
fixed_beta <- function(
                beta_matrix,
                x = NULL,
                y = NULL,
                cov = NULL) {
  if (is.null(x)) {
    i <- rowSums(beta_matrix)
    x <- colnames(beta_matrix)[i == 0]
    if (length(x) == 0) {
      stop("The model has no x-variable(x).")
    }
    if (!is.null(cov)) {
      x <- setdiff(x, cov)
    }
  }
  if (is.null(y)) {
    i <- rowSums(beta_matrix)
    j <- colSums(beta_matrix)
    y <- colnames(beta_matrix)[(j == 0) & (i > 0)]
    if (length(y) == 0) {
      stop("The model has no y-variable(x).")
    }
    if (!is.null(cov)) {
      y <- setdiff(y, cov)
    }
  }
  if (!is.null(cov)) {
    i <- match(cov, colnames(beta_matrix))
    i <- i[!is.na(i)]
    if (length(i) > 0) {
      beta1 <- beta_matrix[-i, -i]
    }
  } else {
    beta1 <- beta_matrix
  }
  bnames <- colnames(beta1)
  y <- intersect(y, bnames)
  x <- intersect(x, bnames)
  if (length(y) == 0) {
    stop("All y(s) not in the model.")
  }
  if (length(x) == 0) {
    stop("All x(s) not in the model.")
  }
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
  # This method is not efficient,
  # but good enough for typical models.
  x <- colnames(m)[1]
  y <- colnames(m)[ncol(m)]
  mnames <- colnames(m)[-c(1, ncol(m))]
  tmp <- replicate(length(mnames),
                   mnames,
                   simplify = FALSE)
  tmp <- do.call(expand.grid,
                 tmp)
  tmp <- tmp[apply(tmp,
                   MARGIN = 1,
                   \(x) length(unique(x))) == length(mnames), ]
  tmp <- apply(tmp,
               MARGIN = 1,
               \(xx) unname(c(x, xx, y)),
               simplify = FALSE)
  orders <- unname(tmp)

  m_k0 <- vector("numeric", nrow(m))
  for (i in seq_along(orders)) {
    m_tmp <- m[orders[[i]], orders[[i]]]
    up0 <- max(m_tmp[upper.tri(m_tmp)]) == 0
    if (up0) {
      tmp <- apply(m_tmp,
                  MARGIN = 1,
                  trailing_0)
      m_k0[i] <- sum(tmp)
    } else {
      m_k0[i] <- 0
    }
  }
  i <- which.max(m_k0)
  m[orders[[i]], orders[[i]]]
}

# Find the number of leading zeros in a
# numeric vector
leading_0 <- function(x) {
  k0 <- 0
  for (i in seq_len(length(x))) {
    if (x[i] == 0) {
      k0 <- k0 + 1
    } else {
      break
    }
  }
  k0
}

# Find the number of trailing zeros in a
# numeric vector
trailing_0 <- function(x) {
  x <- rev(x)
  leading_0(x)
}


# Input:
# - A list of character vectors
# Output:
# - A layout matrix
c_list_to_layout <- function(c_list,
                             v_pos = c("middle", "lower", "upper")) {
  v_pos <- match.arg(v_pos)
  k_max <- sapply(c_list,
                  length)
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
# - Layout matrix
# - A beta matrix
# Output:
# - A modified layout matrix
fix_mxy <- function(
              m,
              beta,
              v_preference = c("upper", "lower")) {
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
# - beta: Regression path matrix
# - m: The x-y layout matrix
# Output:
# - A list of paths, from to_bezier
all_paths <- function(
                      beta,
                      m) {
  all_paths <- mapply(
                  \(x, y) {list(from = x, to = y)},
                  x = colnames(beta)[col(beta)[beta > 0]],
                  y = rownames(beta)[row(beta)[beta > 0]],
                  SIMPLIFY = FALSE,
                  USE.NAMES = FALSE
                )
  all_paths <- lapply(all_paths,
                      to_bezier,
                      m = m)
  all_paths
}

# Input:
# - A list of paths, from to_bezier
# Output:
# - A matrix of intersection, in progress from.
intersect_matrix <- function(all_paths) {
  p <- length(all_paths)
  k <- seq_along(all_paths)
  out <- matrix(NA, p, p)
  outnames <- sapply(
                all_paths,
                \(x) paste0(x$to, "~", x$from),
                simplify = TRUE)
  colnames(out) <- rownames(out) <- outnames
  for (i in k) {
    for (j in setdiff(k, seq(1, i))) {
      path_i <- all_paths[[i]]
      path_j <- all_paths[[j]]
      tmp <- find_intersect(
                    path_i,
                    path_j
                  )
      out[i, j] <- tmp["t"]
      out[j, i] <- tmp["u"]
    }
  }
  out <- round(out, digits = 8)
  out[is.infinite(out)] <- NA
  i <- (out <= 0) | (out >= 1)
  i <- i | t(i)
  out[i] <- NA
  out
}

# Input:
# - A vector of progress values
# Output:
# - A progress value
safe_edge_label_position <- function(
                      x,
                      default_pos = .5,
                      tolerance = .1
                    ) {
  if (all(is.na(x))) {
    return(default_pos)
  }
  x <- x[!is.na(x)]
  x <- sort(x)
  if (min(abs(default_pos - x)) > tolerance) {
    return(default_pos)
  }
  x0 <- c(tolerance, x, 1 - tolerance)
  x1 <- diff(x0)
  i <- which.max(x1)
  out <- mean(x0[c(i, i + 1)])
  out
}

# Input:
# - A matrix of intersections
# Output:
# - A named vector of new positions
all_safe_edge_label_pos <- function(
                              m_intersect,
                              default_pos = .5,
                              tolerance = .05
                            ) {
  p <- nrow(m_intersect)
  m0 <- split(m_intersect,
              rownames(m_intersect))
  out <- sapply(
            m0,
            safe_edge_label_position,
            default_pos = default_pos,
            tolerance = tolerance,
            simplify = TRUE,
            USE.NAMES = TRUE)
  out <- out[out != default_pos]
  out
}

# Input:
# - Two paths, each from to_bezier
# Output:
# - A vector of two prgress values
find_intersect <- function(
                      path_i,
                      path_j) {
  a1 <- cbind(path_i$from_xy - path_j$from_xy,
              path_j$from_xy - path_j$to_xy)
  b1 <- cbind(path_i$from_xy - path_i$to_xy,
              path_j$from_xy - path_j$to_xy)
  t <- det(a1) / det(b1)
  a2 <- cbind(path_i$from_xy - path_i$to_xy,
              path_i$from_xy - path_j$from_xy)
  b2 <- cbind(path_i$from_xy - path_i$to_xy,
              path_j$from_xy - path_j$to_xy)
  u <- -det(a2) / det(b2)
  c(t = t, u = u)
}

# Input:
# - path: list(from, to)
# - m: mxy matrix
# Output
# - A list with Bezier parameters
to_bezier <- function(
                path,
                m
              ) {
  from <- path$from
  to <- path$to
  from_x <- m[from, "x"]
  from_y <- m[from, "y"]
  to_x <- m[to, "x"]
  to_y <- m[to, "y"]
  ax <- from_x
  bx <- to_x - from_x
  ay <- from_y
  by <- to_y - from_y
  list(from = from,
       to = to,
       from_xy = c(x = from_x, y = from_y),
       to_xy = c(x = to_x, y = to_y),
       coef_x = c(a = ax, b = bx),
       coef_y = c(a = ay, b = by))
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

# Input:
# - m: Layout matrix
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

# Convert a x-y matrix to a
# semPlot layout matrix
layout_matrix_from_mxy <- function(
                            m) {
  out0 <- m
  y <- out0[, "y", drop = TRUE]
  y <- y - max(y)
  # TODO:
  # - Find a more efficient method to
  #   create a layout matrix
  tmp <- 2 * 3 * 5 * 2 * 7 * 2 * 3
  y <- y * -tmp + 1
  out0[, "y"] <- y
  out0 <- out0[, c("y", "x")]
  out0 <- split(out0, rownames(out0))
  do.call(layout_matrix,
          out0)
}

# Input:
# - A qgraph object
# Output:
# - A beta matrix
qgraph_to_beta <- function(object) {
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

mod_pa2 <-
 'x3 ~ x1 + x2 + c3
  x4 ~ x3 + x1 + x2
  x5 ~ x4 + c1
  y ~ x5 + x3 + c2
 '

mod_pa2 <-
"
m11 ~ c1 + x1
m21 ~ c2 + m11
m2 ~ m11 + c3
m22 ~ m11 + c3
y ~ m2 + m21 + m22 + x1
"


mod_pa2 <-
"
m11 ~ c1 + x1
m21 ~ c2 + m11
m22 ~ m11 + c3
y ~ m2 + m21 + m22 + x1
y1 ~ m2 + x1
"



fit <- lavaan::sem(mod_pa2,
                    do.fit = FALSE)
dat <- simulateData(parameterTable(fit),
                    sample.nobs = 500,
                    seed = 1234)
fit <- lavaan::sem(mod_pa2,
                   dat)
beta0 <- lavaan::lavInspect(fit,
                            "free")$beta

beta1 <- fixed_beta(
            beta0,
            cov = c("c1", "c2", "c3"))

beta1 <- fixed_beta(
            beta0)


c_list <- column_list(beta1)

m <- c_list_to_layout(
          c_list,
          v_pos = "middle"
        )

m_new <- fix_mxy(
            m, beta1,
            v_preference = "lower"
          )

pm <- semPlotModel(fit) |> drop_nodes(c("c1", "c2", "c3"))
p <- semPaths(
          pm,
          whatLabels = "est",
          layout = layout_matrix_from_mxy(m_new),
          DoNotPlot = TRUE)
plot(p)

tmp <- qgraph_to_beta(p)
fixed_beta(tmp)
beta1

m_new_paths <- all_paths(beta1,
                         m_new)
m_new_intersect <- intersect_matrix(m_new_paths)
m_new_intersect
pos_new <- all_safe_edge_label_pos(m_new_intersect)
if (length(pos_new) > 0) {
  p <- set_edge_label_position(
              p,
              pos_new
            )
}
plot(p)

m_new2 <- m_new
m_new2["m21", "y"] <- -1.25
semPaths(pm,
         whatLabels = "est",
         exoCov = FALSE,
         layout = layout_matrix_from_mxy(m_new2))

semPaths(pm,
         layout = layout_matrix_from_mxy(m))

})
