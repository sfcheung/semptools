#' @noRd
node_names_list <- function(
  object
) {

  if (!inherits(object, "qgraph")) {
    stop("The first argument value is not a qgraph object.")
  }

  # Work on both lists and vectors
  node_names <- as.list(object$graphAttributes$Node$names)
  node_labels <- as.list(object$graphAttributes$Node$labels)
  if (!is.null(names(node_names))) {
    node_names_original <- names(node_names)
  } else {
    node_names_original <- lapply(
      node_names,
      as.character
    )
  }
  # All the elements are lists themselves
  out <- list(
    names_original = node_names_original,
    names = node_names,
    labels = node_labels
  )
  out
}

#' @noRd
match_nodes <- function(
  x,
  node_names,
  check_nodes = TRUE
) {
  i1 <- match(x, node_names$names_original)
  i2 <- match(x, node_names$names)
  i3 <- match(x, node_names$labels)
  out <- i1
  out[!is.na(i2)] <- i2[!is.na(i2)]
  out[!is.na(i3)] <- i3[!is.na(i3)]
  if (check_nodes &&
      any(is.na(out))) {
    stop("Not all nodes are in the plot.")
  }
  out
}