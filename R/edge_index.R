# Internal helper function. Not to be exported.

edge_index <- function(semPaths_plot, from, to) {
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
        return(out)
      }
  }