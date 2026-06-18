# Internal helper function. Not to be exported.

edge_names <- function(
  semPaths_plot,
  names_to_use = c("names", "names_original", "labels")
) {
  if (is.null(semPaths_plot)) {
    stop("semPaths_plot not specified.")
  } else {
    if (!inherits(semPaths_plot, "qgraph")) {
      stop("semPaths_plot is not a qgraph object.")
    }
  }
  names_to_use <- match.arg(names_to_use)

  Nodes_names <- node_names_list(semPaths_plot)

  names1 <- unlist(Nodes_names[[names_to_use]])
  names1 <- unname(names1)

  edges_from <- semPaths_plot$Edgelist$from
  edges_to <- semPaths_plot$Edgelist$to
  edges_bi <- semPaths_plot$Edgelist$bidirectional

  edges_table <- data.frame(
                  from = edges_from,
                  to = edges_to,
                  bi = edges_bi,
                  row.names = NULL
                )

  # ==== Mark duplicated bidirectional edges ====

  to_drop <- list()
  for (i in seq_along(edges_to)) {
    if (isTRUE(edges_table[i, "bi"]) &&
        edges_table[i, "from"] != edges_table[i, "to"]) {
      to_drop <- c(
                    to_drop,
                    list(edges_table[i, c("to", "from")])
                  )
    }
  }

  if (length(to_drop) > 0) {
    a <- lapply(
            to_drop,
            function(x) sort(unname(unlist(x)))
          )
    to_drop <- to_drop[!duplicated(a)]
  }

  edges_table$drop <- FALSE
  if (length(to_drop) > 0) {
    for (x in to_drop) {
      i <- (edges_table$from == x$to) &
          (edges_table$to == x$from)
      if (any(i)) {
        edges_table[i, "drop"] <- TRUE
      }
    }
  }

  # ==== Create edge names ====

  from_names <- names1[edges_table$from]
  to_names <- names1[edges_table$to]
  op <- ifelse(
          edges_table$bi,
          yes = "~~",
          no = "~"
        )
  out <- paste(
            to_names,
            op,
            from_names
          )
  edges_table$name <- out

  return(edges_table)
}

#' @noRd
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