#' @noRd
node_names_list <- function(
  object
) {
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

