# Input:
# - beta: Regression path matrix
# - m: The x-y layout matrix
# Output:
# - A list of paths, from to_bezier
all_paths <- function(
                      beta,
                      m) {
  out <- mapply(
                  \(x, y) {list(from = x, to = y)},
                  x = colnames(beta)[col(beta)[beta > 0]],
                  y = rownames(beta)[row(beta)[beta > 0]],
                  SIMPLIFY = FALSE,
                  USE.NAMES = FALSE
                )
  out <- lapply(out,
                      to_bezier,
                      m = m)
  out
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
# - A layout matrix
# Output:
# - A x-y matrix
layout_to_layoutxy <- function(m) {
  vnames <- unique(m[!is.na(m)])
  x <- col(m)[!is.na(m)]
  y <- row(m)[!is.na(m)]
  out <- cbind(x = x, y = y)
  rownames(out) <- vnames
  out[, "y"] <- max(out[, "y"]) - out[, "y"]
  out
}

# Input:
# - A qgraph object
# Output:
# - A 2-column x-y layout matrix with names
qgraph_to_layoutxy <- function(object) {
  layout0 <- object$layout
  vnames <- unlist(object$graphAttributes$Node$names)
  rownames(layout0) <- vnames
  colnames(layout0) <- c("x", "y")
  layout0
}

# Input:
# - A qgraph object
# Output:
# - A vector of edge position
qgraph_to_edge_label_positions <- function(object) {
  # TODO:
  # - What to do with loadings?
  # - What to do with intercepts?
  e <- object$Edgelist
  i_directed <- (e$directed & !e$bidirectional)
  i_from <- e$from[i_directed]
  i_to <- e$to[i_directed]
  ge <- object$graphAttributes$Edges
  ge_pos <- ge$edge.label.position
  gn <- object$graphAttributes$Nodes
  in_beta <- sort(unique(c(i_from, i_to)))
  node_names <- unlist(gn$names)[in_beta]
  p <- length(node_names)
  out <- matrix(0, p, p)
  colnames(out) <- rownames(out) <- node_names
  i_from <- node_names[i_from]
  i_to <- node_names[i_to]
  for (i in seq_along(i_from)) {
    e_ij <- edge_index(
                  object,
                  from = i_from[i],
                  to = i_to[i]
                )
    out[i_to[i], i_from[i]] <- ge_pos[e_ij]
  }
  out
}
