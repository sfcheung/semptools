# Internal helper function. Not to be exported.

edge_index <- function(
  semPaths_plot,
  from,
  to
) {
  if (is.null(semPaths_plot)) {
      stop("semPaths_plot not specified.")
  } else {
    if (!inherits(semPaths_plot, "qgraph")) {
      stop("semPaths_plot is not a qgraph object.")
    }
  }

  Nodes_names <- node_names_list(semPaths_plot)

  Nodes_in_id_from <- match_nodes(
    from,
    Nodes_names,
    check_nodes = FALSE
  )
  Nodes_in_id_to <- match_nodes(
    to,
    Nodes_names,
    check_nodes = FALSE
  )
  if (is.na(Nodes_in_id_from) ||
      is.na(Nodes_in_id_to)) {
    return(NA)
  }

  out <- which(
            semPaths_plot$Edgelist$from == Nodes_in_id_from &
            semPaths_plot$Edgelist$to == Nodes_in_id_to
          )
  if (length(out) > 0) {
    return(out)
  } else {
    return(NA)
  }
}

#' @noRd
# Internal helper function. Not to be exported.

edge_index_old <- function(semPaths_plot, from, to) {
    if (is.null(semPaths_plot)) {
        stop("semPaths_plot not specified.")
      } else {
        if (!inherits(semPaths_plot, "qgraph")) {
            stop("semPaths_plot is not a qgraph object.")
          }
      }
    Nodes_names <- semPaths_plot$graphAttributes$Nodes$names
    # Need to check whether all edges exist in the graph
    Nodes_id <- seq_len(length(Nodes_names))
    names(Nodes_id) <- Nodes_names
    Nodes_id2 <- Nodes_id
    names(Nodes_id2) <- names(semPaths_plot$graphAttributes$Nodes$names)

    out <- which(semPaths_plot$Edgelist$from == Nodes_id[from] &
                 semPaths_plot$Edgelist$to == Nodes_id[to])
    if (length(out) > 0) {
        return(out)
      } else {
        # Check the names of Nodes_names
        out <- which(semPaths_plot$Edgelist$from == Nodes_id2[from] &
                     semPaths_plot$Edgelist$to == Nodes_id2[to])
        if (length(out) > 0) {
            return(out)
          } else {
            return(NA)
          }
      }
  }