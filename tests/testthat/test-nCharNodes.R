library(lavaan)
library(semPlot)

# Holzinger & Swineford example
HS.model <- ' visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 '
fit <- cfa(HS.model, data = HolzingerSwineford1939)
lv_names <- c("visual", "textual", "speed")
abbreivated_lv_names <- abbreviate(lv_names, minlength = 4L)
# Extract two latent covariances
cov1_fit <- subset(parameterEstimates(fit),
                   lhs == lv_names[1] & op == "~~" & rhs == lv_names[2],
                   select = c(est, se, pvalue))
cov2_fit <- subset(parameterEstimates(fit),
                   lhs == lv_names[2] & op == "~~" & rhs == lv_names[3],
                   select = c(est, se, pvalue))

p1 <- semPaths(fit, whatLabels = "est", nCharNodes = 4, nDigits = 2L, DoNotPlot = TRUE)
p2 <- mark_se(p1, fit, sep = " ", digits = 2L)
p3 <- mark_sig(p1, fit)

test_that("nCharNodes gives same labels as abbreviate()", {
  expect_identical(
    abbreivated_lv_names,
    p1$graphAttributes$Nodes$labels[lv_names]
  )
  expect_identical(
    abbreivated_lv_names,
    p1$graphAttributes$Nodes$labels[lv_names]
  )
  expect_identical(
    abbreivated_lv_names,
    p1$Arguments$labels[lv_names]
  )
})

test_that("semPaths() shows the right parameter estimates", {
  lv_pos <- match(lv_names, names(p1$graphAttributes$Nodes$labels))
  cov1_pos <- with(p1$Edgelist, which(from == lv_pos[1] & to == lv_pos[2]))
  cov2_pos <- with(p1$Edgelist, which(from == lv_pos[2] & to == lv_pos[3]))
  expect_identical(p1$graphAttributes$Edges$labels[cov1_pos],
                   as.character(round(cov1_fit$est, 2L)))
  expect_identical(p1$graphAttributes$Edges$labels[cov2_pos],
                   as.character(round(cov2_fit$est, 2L)))
})

test_that("mark_se() yields a qgraph", {
  expect_is(p2, "qgraph")
})

test_that("mark_sig() yields a qgraph", {
  expect_is(p3, "qgraph")
})

test_that("mark_se() yields same number of nodes and edges", {
  expect_length(p2$graphAttributes$Nodes$labels, 12)
  expect_length(p2$graphAttributes$Edges$labels, 27)
})

test_that("mark_sig() yields same number of nodes and edges", {
  expect_length(p3$graphAttributes$Nodes$labels, 12)
  expect_length(p3$graphAttributes$Edges$labels, 27)
})

test_that("mark_se() works properly with abbreviated nodes", {
  lv_pos <- match(lv_names, names(p2$graphAttributes$Nodes$labels))
  cov1_pos <- with(p2$Edgelist, which(from == lv_pos[1] & to == lv_pos[2]))
  cov2_pos <- with(p2$Edgelist, which(from == lv_pos[2] & to == lv_pos[3]))
  expect_identical(p2$graphAttributes$Edges$labels[cov1_pos],
                   paste0(round(cov1_fit$est, 2L), " (",
                          round(cov1_fit$se, 2L), ")"))
  expect_identical(p2$graphAttributes$Edges$labels[cov2_pos],
                   paste0(round(cov2_fit$est, 2L), " (",
                          round(cov2_fit$se, 2L), ")"))
})

test_that("mark_sig() works properly with abbreviated nodes", {
  lv_pos <- match(lv_names, names(p3$graphAttributes$Nodes$labels))
  cov1_pos <- with(p3$Edgelist, which(from == lv_pos[1] & to == lv_pos[2]))
  cov2_pos <- with(p3$Edgelist, which(from == lv_pos[2] & to == lv_pos[3]))
  expect_identical(p3$graphAttributes$Edges$labels[cov1_pos],
                   paste0(round(cov1_fit$est, 2L), "***"))
  expect_identical(p3$graphAttributes$Edges$labels[cov2_pos],
                   paste0(round(cov2_fit$est, 2L), "***"))
})

