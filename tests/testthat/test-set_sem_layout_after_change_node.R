library(lavaan)
library(semPlot)
library(magrittr)
mod <- 
  'f1 =~ x01 + x02 + x03
   f2 =~ x04 + x05 + x06 + x07
   f3 =~ x08 + x09 + x10
   f4 =~ x11 + x12 + x13 + x14
   f3 ~  f1 + f2
   f4 ~  f1 + f3
  '
fit_sem <- lavaan::sem(mod, sem_example)
lavaan::parameterEstimates(fit_sem)[, c("lhs", "op", "rhs", "est", "pvalue")]
p <- semPaths(fit_sem, whatLabels="est",
        sizeMan = 5,
        nCharNodes = 0, nCharEdges = 0,
        edge.width = 0.8, node.width = 0.7,
        edge.label.cex = 0.6,
        style = "ram", 
        mar = c(10,10,10,10),
        DoNotPlot = TRUE)
p2 <- change_node_label(p, list(list(node = "f1", to = "iv1"),
                                list(node = "f3", to = "Mediator"),
                                list(node = "f4", to = "dv"),
                                list(node = "x01", to = "Test Item")))
indicator_order  <- c("x04", "x05", "x06", "x07", "Test Item", "x02", "x03", 
                      "x11", "x12", "x13", "x14", "x08", "x09", "x10")
indicator_factor <- c( "f2",  "f2",  "f2",  "f2",  "iv1",  "iv1",  "iv1",  
                      "dv",  "dv",  "dv",  "dv",  "Mediator",  "Mediator",  "Mediator")
factor_layout <- matrix(c("iv1",   NA,   NA,
                            NA, "Mediator", "dv",
                          "f2",   NA,   NA), byrow = TRUE, 3, 3)
factor_point_to <- matrix(c("left",     NA,      NA,
                                NA, "down", "down",
                            "left",     NA,      NA), byrow = TRUE, 3, 3)
indicator_push <- list(list(node = "Mediator", push =   2),
                       list(node = "dv", push =   1.5))
indicator_spread <- list(list(node = "iv1", spread =   2),
                         list(node = "f2", spread =   2))
loading_position <- list(list(node = "iv1", position = .5),
                         list(node = "f2", position = .8),
                         list(node = "Mediator", position = .8))

p3 <- set_sem_layout(p2, 
                       indicator_order = indicator_order,
                       indicator_factor = indicator_factor,
                       factor_layout = factor_layout,
                       factor_point_to = factor_point_to,
                       indicator_push = indicator_push,
                       indicator_spread = indicator_spread,
                       loading_position = loading_position) %>%
         set_curve(list(list(from = "iv1", to = "f2", new_curve =  -1),
                        list(from = "iv1", to = "dv", new_curve = 1.5)))

test_that(
  "Labels after set_cfa_layout are changed as expected", {
    expect_equal(unlist(p3$graphAttributes$Nodes$labels)[c(1, 15, 17, 18)],
                     c("Test Item", "iv1", "Mediator", "dv"),
                     check.attributes = FALSE)
  })
