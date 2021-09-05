library(lavaan)
library(semPlot)

mod <- 
  'f1 =~ x01 + x02 + x03
   f2 =~ x04 + x05 + x06 + x07
   f3 =~ x08 + x09 + x10
   f4 =~ x11 + x12 + x13 + x14
  '
fit_cfa <- lavaan::sem(mod, cfa_example)
lavaan::parameterEstimates(fit_cfa)[, c("lhs", "op", "rhs", "est", "pvalue")]
p <- semPaths(fit_cfa, whatLabels="est",
        sizeMan = 2.5,
        nCharNodes = 0, nCharEdges = 0,
        edge.width = 0.8, node.width = 0.7,
        edge.label.cex = 0.6,
        style = "ram", 
        mar = c(10,10,10,10),
        DoNotPlot = TRUE)
indicator_order  <- c("x04", "x05", "x06", "x07", "x01", "x02", "x03", "x11", 
                       "x12", "x13", "x14", "x08", "x09", "x10")
indicator_factor <- c( "f2",  "f2",  "f2",  "f2",  "f1",  "f1",  "f1",  "f4", 
                       "f4",  "f4",  "f4",  "f3",  "f3",  "f3")
p2 <- change_node_label(p, label_list = list(list(node = "f1", to = "factor 1"),
                                                list(node = "f2", to = "F2"),
                                                list(node = "x03", to = "Test Item")))
indicator_factor2 <- gsub("f1", "factor 1", indicator_factor)
indicator_factor2 <- gsub("f2", "F2", indicator_factor2)
indicator_order2 <- gsub("x03", "Test Item", indicator_order)
p3 <-  set_cfa_layout(p2, indicator_order2, 
                          indicator_factor2, 
                          fcov_curve = 1.5, 
                          loading_position = .8)

test_that(
  "Labels after set_cfa_layout are changed as expected", {
    expect_equal(unlist(p3$graphAttributes$Nodes$labels)[c(3, 15, 16)],
                     c("Test Item", "factor 1", "F2"),
                     check.attributes = FALSE)
  })
