# Input:
# - A qgraph object
# Output:
# - A numeric vector of the angles of the residuals
qgraph_to_resid_angles <- function(object) {
  n0 <- object$graphAttributes$Node
  out <- n0$loopRotation * 180 / pi
  node_names <- unlist(n0$names)
  names(out) <- node_names
  out
}

# Input:
# - A vector of angles
# Output:
# - The angle in the middle of the first largest arc
#   3' is 0, 12' is 90, etc.
largest_arc <- function(angles_i,
                        default_angle = 0) {
  a0 <- angles_i[!is.na(angles_i)]
  if (length(a0) == 0) {
    return(default_angle)
  }
  a0 <- sort(a0)
  a0 <- c(a0, a0[1] + 360)
  b1 <- diff(a0)
  i <- which.max(b1)
  out <- mean(a0[c(i, i + 1)])
  if (out > 360) {
    out <- out - 360
  }
  out
}

# Input:
# - A list of paths, from to_bezier
# Output:
# - A matrix of angle
angle_matrix <- function(all_paths) {
  vnames <- lapply(
              all_paths,
              \(x) c(x$from, x$to)
            )
  vnames <- unique(unlist(vnames))
  q <- length(vnames)
  out <- matrix(NA, q, q)
  colnames(out) <- rownames(out) <- vnames
  k2 <- seq_along(vnames)
  for (i in k2) {
    v_i <- vnames[i]
    angles_to <- sapply(all_paths,
                        angle_i,
                        target = v_i,
                        USE.NAMES = FALSE)
    angles_to <- angles_to[!is.na(angles_to)]
    if (length(angles_to) == 0) {
      next
    }
    out[v_i, names(angles_to)] <- angles_to
  }
  out
}

# Input:
# - A path
# Output:
# - An angle for the target-node.
# - 3' is 0, 12' is 90, 6' is 270, ...
angle_i <- function(path_i,
                    target) {
  if ((target != path_i$to) &&
      (target != path_i$from)) {
    return(NA)
  } else {
    target_is_from <- (target == path_i$from)
    out0 <- atan2(
              path_i$from_xy["y"] - path_i$to_xy["y"],
              path_i$from_xy["x"] - path_i$to_xy["x"]
            )
    out0 <- out0 * 180 / pi
    if (out0 < 0) {
      out0 <- 360 + out0
    }
    if (target_is_from) {
      out0 <- out0 - 180
    }
    if (out0 < 0) {
      out0 <- 360 + out0
    }
  }
  names(out0) <- ifelse(target_is_from,
                       yes = path_i$to,
                       no = path_i$from)
  out0
}
