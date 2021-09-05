abort_nomatch <- function(nodes_names, ests_names) {
  rlang::abort(
    paste0("The node names in the semPaths graph: \n",
           paste(nodes_names, collapse = ", "),
           "\ndo not match the variable names in the fitted object: \n",
           paste(ests_names, collapse = ", "),
           "\nIf the `nodeLabels` argument was used",
           "in the original `semPaths()` call, please remove it, ",
           "and use the semptools::change_node_label() function",
           "to change node labels.")
  )
}

check_match_labels <- function(names_in, names_graph) {
  if (!all(names_in %in% names_graph)) {
    rlang::abort(
      paste0("One or more nodes in label_list: \n",
             paste(names_in, collapse = ", "),
             "\ndo not match the node names in the semPaths graph: \n",
             paste(names_graph, collapse = ", "))
    )
  }
}
