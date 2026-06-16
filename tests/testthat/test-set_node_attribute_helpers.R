test_that("set_node_attribute helpers", {

dat <- pa_example
colnames(dat) <- gsub("x3", "TheX3", colnames(dat))
colnames(dat) <- gsub("x4", "TheX4", colnames(dat))

library(lavaan)
mod_pa <-
 'x1 ~~ x2
  TheX3 ~  x1 + x2
  TheX4 ~  x1 + x2
 '
fit_pa <- lavaan::sem(mod_pa, dat)

library(semPlot)
m <- matrix(c("x1",   NA,  NA,   NA,
                NA, "TX3",  NA, "TX4",
              "x2",   NA,  NA,   NA), byrow = TRUE, 3, 4)
p_pa_abbr <- semPaths(
  fit_pa,
  whatLabels = "est",
  sizeMan = 10,
  edge.label.cex = 1.15,
  # nCharNodes = 0,
  nCharEdges = 0,
  layout = m,
  DoNotPlot = TRUE
)
# plot(p_pa)

out <- node_names_list(p_pa_abbr)
expect_in(c("x1", "x2", "TX3", "TX4"),
          out$names)
expect_in(c("x1", "x2", "TheX3", "TheX4"),
          out$names_original)
expect_in(c("x1", "x2", "TX3", "TX4"),
          out$labels)

m_no_abbr <- matrix(
             c("x1",   NA,  NA,   NA,
                NA, "TheX3",  NA, "TheX4",
              "x2",   NA,  NA,   NA), byrow = TRUE, 3, 4)
p_pa_no_abbr <- semPaths(
  fit_pa,
  whatLabels = "est",
  sizeMan = 10,
  edge.label.cex = 1.15,
  nCharNodes = 0,
  nCharEdges = 0,
  layout = m_no_abbr,
  DoNotPlot = TRUE
)
# plot(p_pa_no_abbr)
out <- node_names_list(p_pa_no_abbr)
expect_in(c("x1", "x2", "TheX3", "TheX4"),
          out$names)
expect_in(c("x1", "x2", "TheX3", "TheX4"),
          out$names_original)
expect_in(c("x1", "x2", "TheX3", "TheX4"),
          out$labels)

m_labels <- matrix(
             c("X1",   NA,  NA,   NA,
                NA, "XX3",  NA, "TheLongX4",
              "X2",   NA,  NA,   NA), byrow = TRUE, 3, 4)
p_pa_labels <- semPaths(
  fit_pa,
  whatLabels = "est",
  sizeMan = 10,
  edge.label.cex = 1.15,
  nodeLabels = c("XX3", "TheLongX4", "X1", "X2"),
  nCharEdges = 0,
  layout = m_labels,
  DoNotPlot = TRUE
)
# plot(p_pa_labels)
out <- node_names_list(p_pa_labels)
expect_in(c("X1", "X2", "XX3", "TheLongX4"),
          out$names)
expect_in(c("X1", "X2", "XX3", "TheLongX4"),
          out$names_original)
expect_in(c("X1", "X2", "XX3", "TheLongX4"),
          out$labels)

p_pa_labels_expression <- semPaths(
  fit_pa,
  whatLabels = "est",
  sizeMan = 10,
  edge.label.cex = 1.15,
  nodeLabels = c("XX3", expression(gamma), "X1", "X2"),
  nCharEdges = 0,
  # layout = m_labels,
  DoNotPlot = TRUE
)
# plot(p_pa_labels_expression)
out <- node_names_list(p_pa_labels_expression)
expect_in(c("XX3", "X1", "X2"),
          out$names[-2])
expect_in(c("X1", "X2", "XX3", "gamma"),
          out$names_original)
expect_in(c("X1", "X2", "XX3"),
          out$labels[-2])
expect_identical(
 out$labels[[2]],
 as.symbol("gamma")
)
})
