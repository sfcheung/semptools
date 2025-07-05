skip("WIP")

library(lavaan)
library(semPlot)


auto_resid_rotation <- function(
                          object,
                          layout,
                          default_angle = 0,
                          style = c("1200", "geometry"),
                          update_plot = TRUE
                        ) {

  style <- match.arg(style)

  object_type <- NA
  if (inherits(object, "lavaan")) {
    object_type <- "lavaan"
    if (lavaan::lavTech(object, "ngroups") != 1) {
      stop("Multigroup models not supported.")
    }
    beta0 <- lavaan::lavInspect(
                object,
                what = "free"
              )$beta
    if (is.null(beta0)) {
      stop("The model has no structural paths. Is it a CFA model?")
    }
    mxy <- layout_to_layoutxy(layout)
  } else if (inherits(object, "qgraph")) {
    object_type <- "qgraph"
    beta0 <- qgraph_to_beta(object)
    mxy <- qgraph_to_layoutxy(object)
  } else {
    stop("object is not a supported type.")
  }

  all_paths <- all_paths(
                    beta0,
                    mxy
                  )
  all_angles <- angle_matrix(all_paths)

  resid_pos <- apply(
                  all_angles,
                  MARGIN = 1,
                  FUN = largest_arc
                )
  resid_pos <- resid_pos[!is.na(resid_pos)]
  vnames <- names(resid_pos)

  if (object_type == "lavaan") {
    default_angle1 <- rep(default_angle, length(resid_pos))
  } else if (object_type == "qgraph") {
    n_angle <- qgraph_to_resid_angles(object)
    default_angle1 <- n_angle[vnames]
  }

  if (style == "1200") {
    resid_pos <- 90 - resid_pos
  } else {
    default_angle1 <- default_angle1 + 90
  }
  i <- resid_pos == default_angle1
  resid_pos <- resid_pos[!i]

  if (object_type == "qgraph") {
    if (update_plot) {
      if (length(resid_pos) > 0) {
        out <- rotate_resid(
                  semPaths_plot = object,
                  rotate_resid_list = resid_pos
                )
      } else {
        out <- object
      }
    } else {
      out <- resid_pos
    }
  } else {
    out <- resid_pos
  }
  out
}

# Input:
# - A qgraph object
# Output:
# - A numeric vector of the angles of the residuals
qgraph_to_resid_angles <- function(object) {
  n0 <- object$graphAttributes$Node
  out <- n0$loopRotation
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

mod_pa <-
  'x1 ~~ x2
   x3 ~  x1 + x2
   x4 ~  x1 + x3
  '
fit_pa <- lavaan::sem(mod_pa, pa_example)
m <- matrix(c("x1",   NA,  NA,   NA,
              NA, "x3",  NA, "x4",
              "x2",   NA,  NA,   NA), byrow = TRUE, 3, 4)
p_pa <- semPaths(fit_pa, whatLabels = "est",
                 sizeMan = 10,
                 edge.label.cex = 1.15,
                 layout = m,
                 DoNotPlot = TRUE)

plot(auto_resid_rotation(p_pa))

test_that("add_rsq", {
    p_rsq <- add_rsq(p_pa, fit_pa)
    plot(p_rsq)
    plot(auto_resid_rotation(p_rsq))


    p_rsq2 <- add_rsq(p_pa, fit_pa, rsq_string = "Rsq:")
    p_rsq3 <- add_rsq(p_pa, ests = parameterEstimates(fit_pa, rsquare = TRUE,
                                                      se = FALSE))
    expect_identical(p_rsq$graphAttributes$Edges$labels,
                     p_pa_est_chk)
    expect_identical(p_rsq2$graphAttributes$Edges$labels,
                     p_pa_est_chk2)
    expect_identical(p_rsq3$graphAttributes$Edges$labels,
                     p_pa_est_chk)
    p_rsq_std <- add_rsq(p_std, fit_pa)
    p_rsq_std2 <- add_rsq(p_std, ests = parameterEstimates(fit_pa, rsquare = TRUE,
                                                           se = FALSE))
    expect_identical(p_rsq_std$graphAttributes$Edges$labels,
                     p_pa_std_chk)
    expect_identical(p_rsq_std2$graphAttributes$Edges$labels,
                     p_pa_std_chk)
  })

# Test a structure-only diagram

mod <-
  'f1 =~ x01 + x02 + x03
   f2 =~ x04 + x05 + x06 + x07
   f3 =~ x08 + x09 + x10
   f4 =~ x11 + x12 + x13 + x14
   f3 ~  f1 + f2
   f4 ~  f1 + f3
  '
fit_sem <- lavaan::sem(mod, sem_example)
est <- lavaan::parameterEstimates(fit_sem, rsquare = TRUE)[, c("lhs", "op", "rhs", "est", "pvalue")]
m <- layout_matrix(f1 = c(1, 1),
                   f2 = c(3, 1),
                   f3 = c(2, 2),
                   f4 = c(2, 3))
p <- semPaths(fit_sem, whatLabels="est",
        sizeMan = 5,
        nCharNodes = 0, nCharEdges = 0,
        edge.width = 0.8, node.width = 0.7,
        edge.label.cex = 0.6,
        mar = c(10,10,10,10),
        layout = m,
        structural = TRUE,
        style = "lisrel",
        DoNotPlot = TRUE)
p2_std_chk <- est[(est$op == "r2") & (est$lhs %in% c("f3", "f4")), "est"]
p2_std_chk <- paste0("R2=", formatC(p2_std_chk, 2, format = "f"))
test_that(
  "add_rsq: structural", {
    p2 <- add_rsq(p, fit_sem)
    expect_identical(p2$graphAttributes$Edges$labels[5:6],
                     p2_std_chk)
  })
