#'@title Configure the layout of factors of an SEM graph by semPaths
#'
#'@description Configure the layout of factors and adjust other aspects of an SEM graph by semPaths.
#'
#'@details Modified a qgraph object generated by semPaths based on a SEM model
#'         with latent factors. Currently only support a model in which all 
#'         exogenous variables are latent factors, and all observed variables are 
#'         indicators. If a variable has only one indicator, it is easier to 
#          configure the layout by using a phantom variable for this variable.
#'
#'@return
#' A qgrpah based on the original one, with various aspects of the model modified.
#' 
#'@param semPaths_plot A qgraph object generated by semPaths, or a similar qgrpah
#'                      object modified by other semptools functions.
#'@param indicator_order A string vector of the indicators. The order of the names
#'                       is the order of the indicators in the graph, when they 
#'                       are drawn on the bottom of the graph. The indicators
#'                       should be grouped by the factors on which they load on.
#'                       For example, if x1, x2, x4 load on f2, and x3, x5, x6 
#'                       load on f1, then vector should be either c("x1", "x2", "x4", "x3", "x5", "x6")
#'                       or c("x3", "x5", "x6", "x1", "x2", "x4"). Indicators within
#'                       a group can be ordered in any way.
#'@param indicator_factor A string vector of the same length of the indicator_order,
#'                        storing the name of the factor for which each of the indicator 
#"                        in indicator_factor loads on.  
#'                       For example, if x1, x2, x4 load on f2, and x3, x5, x6 
#'                       load on f1, and indicator_order is c("x3", "x5", "x6", "x1", "x2", "x4"),
#'                       then indicator_factor should be c("f2", "f2", "f2", "f1", "f1", "f1").
#'@param factor_layout A matrix of arbitrary size. This matrix will serve as a grid for 
#'                     users to specify where each latent factor should be placed 
#'                     approximately on the graph. Each cell should contain NA or 
#'                     the name of a latent factor. The locations of all latent factors
#'                     must be explicitly specified by this matrix.
#'@param factor_point_to A matrix of the same size as factor_layout. This matrix 
#'                       specifies where the indicators of each factor are positioned.
#'                     Each cell should contain NA or one of these strings: 
#'                     "down", "left", "up", or "right". This is the direction that the 
#'                     corresponding latent factor (specified in factor_layout)
#'                     points to its indicators.
#'@param indicator_push (Optional) This argument is used to adjust the position of 
#'                      indicators of selected latent factors. It must be a list of lists.
#'                      Each sublist has two named elements: node, the name of a latent 
#'                      factor, and push, how the positions of its indicators will be 
#'                      adjusted. If push = 1, there is no change. Larger than one,
#'                      the indicators will be "pushed" away from the latent factors.
#'                      Less than one, the indicators will be "pulled" to the latent 
#'                      factors. 
#'@param indicator_spread (Optional) This argument is used to adjust the distance between  
#'                      indicators of selected latent factors. It must be a list of lists.
#'                      Each sublist has two named elements: node, the name of a latent 
#'                      factor, and spread, how the distance between indicators will be 
#'                      adjusted. If spread = 1, there is no change. Larger than one,
#'                      the indicators will be "spread" away from each other.
#'                      Less than one, the indicators will be placed closer to each
#'                      others. 
#'@param loading_position Default is .5. This is used adjust the position of the loadings.
#'                  If this is one single number, it will be used to set the positions
#'                  of all loadings. If it is .5, the loadings are placed on the center of 
#'                  the arrows. Larger the number, closer the loadings to the indicators.
#'                  Smaller the number, closer to the latent factors.
#'                  This argument also accepts a list of lists, allowing users to 
#'                  specify the positions of loadings for each factor separately. 
#'                  Each sublist will have two named elements: node, the name of the 
#'                  latent factor, and position, the positions of all loadings of this 
#'                  factors. The values of these positions are used in the same way as 
#'                  specifying one single number.
#'@examples
#'library(lavaan)
#'library(semPlot)
#'library(magrittr)
#'mod <- 
#'  'f1 =~ x01 + x02 + x03
#'   f2 =~ x04 + x05 + x06 + x07
#'   f3 =~ x08 + x09 + x10
#'   f4 =~ x11 + x12 + x13 + x14
#'   f3 ~  f1 + f2
#'   f4 ~  f1 + f3
#'  '
#'fit_sem <- lavaan::sem(mod, sem_example)
#'lavaan::parameterEstimates(fit_sem)[, c("lhs", "op", "rhs", "est", "pvalue")]
#'p <- semPaths(fit_sem, whatLabels="est",
#'        sizeMan = 5,
#'        nCharNodes = 0, nCharEdges = 0,
#'        edge.width = 0.8, node.width = 0.7,
#'        edge.label.cex = 0.6,
#'        style = "ram", 
#'        mar = c(10,10,10,10))
#'indicator_order  <- c("x04", "x05", "x06", "x07", "x01", "x02", "x03", 
#'                      "x11", "x12", "x13", "x14", "x08", "x09", "x10")
#'indicator_factor <- c( "f2",  "f2",  "f2",  "f2",  "f1",  "f1",  "f1",  
#'                      "f4",  "f4",  "f4",  "f4",  "f3",  "f3",  "f3")
#'factor_layout <- matrix(c("f1",   NA,   NA,
#'                            NA, "f3", "f4",
#'                          "f2",   NA,   NA), byrow = TRUE, 3, 3)
#'factor_point_to <- matrix(c("left",     NA,      NA,
#'                                NA, "down", "down",
#'                            "left",     NA,      NA), byrow = TRUE, 3, 3)
#'indicator_push <- list(list(node = "f3", push =   2),
#'                       list(node = "f4", push =   1.5))
#'indicator_spread <- list(list(node = "f1", spread =   2),
#'                         list(node = "f2", spread =   2))
#'loading_position <- list(list(node = "f1", position = .5),
#'                         list(node = "f2", position = .8),
#'                         list(node = "f3", position = .8))
#'p2 <- set_sem_layout(p, 
#'                       indicator_order = indicator_order,
#'                       indicator_factor = indicator_factor,
#'                       factor_layout = factor_layout,
#'                       factor_point_to = factor_point_to,
#'                       indicator_push = indicator_push,
#'                       indicator_spread = indicator_spread,
#'                       loading_position = loading_position) %>%
#'         set_curve(list(list(from = "f1", to = "f2", new_curve =  -1),
#'                        list(from = "f1", to = "f4", new_curve = 1.5))) %>%
#'         mark_sig(fit_sem) %>% 
#'         mark_se(fit_sem, sep = "\n")
#'plot(p2)
#'
#' @export

set_sem_layout <- function(semPaths_plot, 
                             indicator_order = NULL,
                             indicator_factor = NULL,
                             factor_layout = NULL,
                             factor_point_to = NULL,
                             indicator_push = NULL,
                             indicator_spread = NULL,
                             loading_position = .5) {
    if (is.null(indicator_order)) {
        stop("indicator_order not specified.")
      }
    if (is.null(indicator_factor)) {
        stop("indicator_factor not specified.")
      }
    if (is.null(factor_layout)) {
        stop("factor_layout not specified.")
      }
    if (is.null(factor_point_to)) {
        stop("factor_point_to not specified.")
      }
    if (is.null(semPaths_plot)) {
        stop("semPaths_plot not specified.")
      } else {
        if (!inherits(semPaths_plot, "qgraph")) {
            stop("semPaths_plot is not a qgraph object.")
          }
      }  
    Nodes_names <- semPaths_plot$graphAttributes$Nodes$names  
    if (!all(Nodes_names[semPaths_plot$graphAttributes$Nodes$shape == "square"] %in% indicator_order)) {
        warning("One or more indicators in the graph may not be in indicator_order. Unexpected results may occur.")
      }
    if (!all(Nodes_names[semPaths_plot$graphAttributes$Nodes$shape == "circle"] %in% indicator_factor)) {
        warning("One or more factors in the graph may not be in indicator_factor. Unexpected results may occur.")
      }
    if (!all(factor_layout[!is.na(factor_layout)] %in% indicator_factor)) {
        stop("The position of one or more latent factors are not in factor_layout.")
      }
    if (!all(!is.na(factor_layout) == !is.na(factor_point_to))) {
        stop("The positions of the indicators of one or more latent factors are not specified in factor_point_to.")
      }
      
    # Set the estate

    factor_order <- unique(indicator_factor)

    layout_nrow <- nrow(factor_layout)
    layout_ncol <- ncol(factor_layout)
    
    factor_coord <- t(sapply(factor_order, function(x) {
                              which(factor_layout == x, arr.ind = TRUE)
                            }, USE.NAMES = TRUE))
    factor_coord_point_to <- sapply(factor_order, function(x) {
                              factor_point_to[factor_coord[x, 1],
                                              factor_coord[x, 2]]
                            }, USE.NAMES = TRUE)
    indicator_grouped <- split(indicator_order, indicator_factor)
    position_grouped <- sapply(indicator_grouped, function(x) {
                              out <- seq(-1, 1, length.out = length(x) + 2)
                              out <- out[-1]
                              out <- out[-length(out)]
                              out
                            }, simplify = "array", USE.NAMES = TRUE)

    factor_coord2y <- -1*(2*(factor_coord[, 1]*2 - 1)/(2*layout_nrow) - 1)
    factor_coord2x <- 2*(factor_coord[, 2]*2 - 1)/(2*layout_ncol) - 1
    box_width  <- 2/layout_ncol
    box_height <- 2/layout_nrow
    
    set_indicator_xy <- function(x, 
                                 position_grouped,
                                 factor_coord_point_to,
                                 factor_coord2x,
                                 factor_coord2y,
                                 box_width,
                                 box_height,
                                 indicator_push,
                                 indicator_spread) {
        position_grouped_i <- position_grouped[[x]]
        factor_coord_point_to_i <- factor_coord_point_to[x]
        factor_coord2x_i <- factor_coord2x[x]
        factor_coord2y_i <- factor_coord2y[x]
        if (is.null(indicator_push)) {
            indicator_push_i <- 1
          } else {
            tmp <- sapply(indicator_push, 
              function(y, node) {ifelse(y$node == node, y$push, NA)}, node = x)
            indicator_push_i <- tmp[!is.na(tmp)]
            if (length(indicator_push_i) == 0) indicator_push_i <- 1
          }
        if (is.null(indicator_spread)) {
            indicator_spread_i <- 1
          } else {
            tmp <- sapply(indicator_spread, 
              function(y, node) {ifelse(y$node == node, y$spread, NA)}, node = x)
            indicator_spread_i <- tmp[!is.na(tmp)]
            if (length(indicator_spread_i) == 0) indicator_spread_i <- 1
          }
        k <- length(position_grouped_i)         
        position_grouped_x <- 
          position_grouped_i * (indicator_spread_i*box_width/2) + 
                                factor_coord2x_i
        position_grouped_y <- 
          position_grouped_i * (indicator_spread_i*box_height/2) + 
                                factor_coord2y_i
        if (factor_coord_point_to_i == "down") {
            position_grouped_x <- position_grouped_x
            position_grouped_y <- rep(factor_coord2y_i - 
                                      (indicator_push_i*box_height/2), k)
          } else if (factor_coord_point_to_i == "up") {
            position_grouped_x <- position_grouped_x
            position_grouped_y <- rep(factor_coord2y_i + 
                                      (indicator_push_i*box_height/2), k)
          } else if (factor_coord_point_to_i == "left") {
            position_grouped_x <- rep(factor_coord2x_i - 
                                      (indicator_push_i*box_width/2), k)
            position_grouped_y <- position_grouped_y
          } else if (factor_coord_point_to_i == "right") {
            position_grouped_x <- rep(factor_coord2x_i +  
                                      (indicator_push_i*box_width/2), k)
            position_grouped_y <- position_grouped_y
          }
        list(position_grouped_x = position_grouped_x,
             position_grouped_y = position_grouped_y)
      }
    indicator_xy <- sapply(factor_order, set_indicator_xy, 
                             position_grouped = position_grouped, 
                             factor_coord_point_to = factor_coord_point_to,
                             factor_coord2x = factor_coord2x, 
                             factor_coord2y = factor_coord2y,
                             box_width = box_width,
                             box_height = box_height,
                             indicator_push = indicator_push,
                             indicator_spread = indicator_spread,
                             simplify = FALSE)
    spread_indicator_xy_i <- function(x, indicator_xy, indicator_grouped) {
                                data.frame(node = indicator_grouped[[x]],
                                 x = indicator_xy[[x]]$position_grouped_x,
                                 y = indicator_xy[[x]]$position_grouped_y)
                             }
    spread_indicator_xy <- sapply(factor_order, spread_indicator_xy_i, 
                             indicator_xy = indicator_xy, 
                             indicator_grouped = indicator_grouped,
                             simplify = FALSE)
    spread_indicator_xy <- do.call(rbind, spread_indicator_xy)
    spread_factor_xy <- data.frame(node = names(factor_coord2y),
                                      x = factor_coord2x,
                                      y = factor_coord2y)
    Nodes_xy <- rbind(spread_indicator_xy, spread_factor_xy)
    
    original_layout <- semPaths_plot$layout
    i <- match(Nodes_names, Nodes_xy$node)
    new_layout <- original_layout
    new_layout[, 1] <- Nodes_xy$x[i]
    new_layout[, 2] <- Nodes_xy$y[i]
    semPaths_plot$layout <- new_layout

    # Fix the residual
    
    residual_rotate <- lapply(seq_len(length(indicator_order)), 
            function(x, indicator_order,
                        indicator_factor,
                        factor_coord_point_to) {
                list(node = indicator_order[x], 
                     rotate = switch(factor_coord_point_to[indicator_factor[x]],
                                up    =   0,
                                right =  90,
                                down  = 180,
                                left  = -90))
              }, indicator_order = indicator_order,
                 indicator_factor = indicator_factor,
                 factor_coord_point_to = factor_coord_point_to)
    semPaths_plot <- rotate_resid(semPaths_plot, residual_rotate)
    
    factor_residual_rotate <- lapply(names(factor_coord_point_to),
          function(x, factor_coord_point_to) {
                list(node = x, 
                     rotate = switch(factor_coord_point_to[x],
                                up    = 180,
                                right = -90,
                                down  =   0,
                                left  =  90))
              }, factor_coord_point_to = factor_coord_point_to)
    semPaths_plot <- rotate_resid(semPaths_plot, factor_residual_rotate)
    
    # Position the loadings

    if ((length(loading_position) == 1) & (is.numeric(loading_position))) {
        loading_position_list <- 
            (Nodes_names[semPaths_plot$Edgelist$from] %in% factor_order) & 
            (Nodes_names[semPaths_plot$Edgelist$to] %in% indicator_order) &
                         !semPaths_plot$Edgelist$bidirectional
        semPaths_plot$graphAttributes$Edges$edge.label.position[loading_position_list] <- loading_position
      } else {
        loading_label_position <- lapply(seq_len(length(loading_position)), 
                function(x, loading_position,
                            indicator_grouped) {
                     node <- loading_position[[x]]$node
                     position <- loading_position[[x]]$position
                     out <- lapply(indicator_grouped[[node]],
                                function(y, node, position) {
                                    list(from = node,
                                           to = y,
                                           new_position = position)
                                  }, node = node, position = position
                              )
                     out
                  }, loading_position = loading_position,
                     indicator_grouped = indicator_grouped)
        loading_label_position <- do.call(c, loading_label_position) 
        semPaths_plot <- set_edge_label_position(semPaths_plot, 
                                                 loading_label_position)
      }
    semPaths_plot
  }
