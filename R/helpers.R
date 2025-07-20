#' @noRd

node_plot <- function(semPaths_plot) {
    nodes <- semPaths_plot$graphAttributes$Nodes
    nodes$names
  }

#' @noRd

man_plot <- function(semPaths_plot) {
    nodes <- semPaths_plot$graphAttributes$Nodes
    nodes$names[nodes$shape == "square"] | nodes$names[nodes$shape == "rectangle"]
  }

#' @noRd

lat_plot <- function(semPaths_plot) {
    nodes <- semPaths_plot$graphAttributes$Nodes
    nodes$names[nodes$shape == "circle"] | nodes$names[nodes$shape == "ellipse"]
  }

#' @noRd

indicator_plot <- function(semPaths_plot) {
    nodes <- semPaths_plot$graphAttributes$Nodes
    man_id <- which((nodes$shape == "square") | (nodes$shape == "rectangle"))
    lat_id <- which((nodes$shape == "circle") | (nodes$shape == "ellipse"))
    edges <- as.data.frame(semPaths_plot$Edgelist)
    edges2 <- edges[edges$directed & !edges$bidirectional, ]
    id <- (edges2$from %in% lat_id) & (edges2$to %in% man_id)
    nodes$names[edges2$to[id]]
  }

#' @noRd

loading_plot <- function(semPaths_plot,
                         add_isolated_manifest = TRUE) {
    nodes <- semPaths_plot$graphAttributes$Nodes
    # Assume that:
    #   squares are manifest variables
    #   circles are latent variables
    man_id <- which((nodes$shape == "square") | (nodes$shape == "rectangle"))
    lat_id <- which((nodes$shape == "circle") | (nodes$shape == "ellipse"))
    edges <- as.data.frame(semPaths_plot$Edgelist)
    edges2 <- edges[edges$directed & !edges$bidirectional, ]
    id <- (edges2$from %in% lat_id) & (edges2$to %in% man_id)
    edges3 <- edges2[id, ]
    if (add_isolated_manifest) {
        # Isolated manifest variables
        id2 <- !(man_id %in% edges2$to)
        iso_man <- unlist(nodes$names)[id2]
      } else {
        iso_man <- NULL
      }
    edges3$lhs <- unlist(nodes$names)[edges3$to]
    edges3$rhs <- unlist(nodes$names)[edges3$from]
    edges4 <- edges3[!duplicated(edges3$lhs), ]
    out <- c(edges4$lhs, iso_man)
    names(out) <- c(edges4$rhs, iso_man)
    out
  }

#' @noRd

add_manifest <- function(factor_layout,
                         indicator_order,
                         indicator_factor) {
    factor_names <- as.vector(factor_layout)
    factor_names <- factor_names[!is.na(factor_names)]
    factor_names2 <- unique(indicator_factor)
    to_add <- setdiff(factor_names, factor_names2)
    if (length(to_add) > 0) {
        indicator_order <- c(indicator_order, to_add)
        indicator_factor <- c(indicator_factor, to_add)
      }
    out <- list(indicator_order = indicator_order,
                indicator_factor = indicator_factor)
    return(out)
  }

#' @noRd

check_node_label_string <- function(x) {
    chk <- sapply(x, is.character)
    if (!all(chk)) {
        msg <- paste("Not all labels are strings.",
                     "Please set labels after applying this function.")
        tmp <- paste(names(x)[!chk], collapse = ", ")
        msg <- paste(msg,
                     "Node(s) with non-string label(s):",
                     tmp)
        stop(msg)
      } else {
        return(TRUE)
      }
  }

#' @noRd

check_node_label_changed <- function(x) {
    check_node_label_string(x)
    chk <- names(x) == unlist(x)
    if (!all(chk)) {
        msg <- paste("Not all nodes have labels identical to node names.",
                     "Please set labels after applying this function,",
                     "and please set nCharNodes = 0 when calling semPaths().")
        tmp <- paste(names(x)[!chk], collapse = ", ")
        msg <- paste(msg,
                     "Node(s) with changed/shortened label(s):",
                     tmp)
        stop(msg)
      } else {
        return(TRUE)
      }
  }
