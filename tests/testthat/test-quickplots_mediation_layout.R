skip("WIP")

library(lavaan)
library(semPlot)

test_that("q_plot layout", {


# Input:
# - beta: Regression path matrix
# - m: The layout x-y matrix
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

lower_e(beta0)

beta1 <- fixed_beta(
            beta0)

beta1 <- fixed_beta(
            beta0,
            cov = c("c1", "c2", "c3"))
beta1

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
semPaths(
          pm,
          whatLabels = "est",
          layout = layout_matrix_from_mxy(m))
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
