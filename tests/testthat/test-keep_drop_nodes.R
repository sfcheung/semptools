library(lavaan)
library(semPlot)

# drop nodes

# path model

mod_pa <- 
  'x1 ~~ x2
   x3 ~  x1 + x2
   x4 ~  x1 + x3
  '
fit <- lavaan::sem(mod_pa, pa_example)

semPaths(fit)

fit_no1 <- drop_nodes(semPlot::semPlotModel(fit), c("x1"))
out_no1 <- semPaths(fit_no1, "est")
fit_no2 <- drop_nodes(semPlot::semPlotModel(fit), c("x2"))
out_no2 <- semPaths(fit_no2, "est")
fit_no3 <- drop_nodes(semPlot::semPlotModel(fit), c("x3"))
out_no3 <- semPaths(fit_no3, "est")
fit_no4 <- drop_nodes(semPlot::semPlotModel(fit), c("x4"))
out_no4 <- semPaths(fit_no4, "est")

test_that("Correct nodes dropped", {
  expect_equal(
    out_no1$graphAttributes$Nodes$labels,
    c("x3", "x4", "x2"),
    check.attributes = FALSE
  )
  expect_equal(
    out_no2$graphAttributes$Nodes$labels,
    c("x3", "x4", "x1"),
    check.attributes = FALSE
  )
  expect_equal(
    out_no3$graphAttributes$Nodes$labels,
    c("x4", "x1", "x2"),
    check.attributes = FALSE
  )
  expect_equal(
    out_no4$graphAttributes$Nodes$labels,
    c("x3", "x1", "x2"),
    check.attributes = FALSE
  )
})

# SEM

mod <- 
  'f1 =~ x01 + x02 + x03
   f2 =~ x04 + x05 + x06 + x07
   f3 =~ x08 + x09 + x10
   f4 =~ x11 + x12 + x13 + x14
   f3 ~  f1 + f2
   f4 ~  f1 + f3
  '
fit <- lavaan::sem(mod, sem_example)

semPaths(fit)

fit_no_x01 <- drop_nodes(semPlot::semPlotModel(fit), c("x01"))
out_no_x01 <- semPaths(fit_no_x01, "est")
fit_no_x02 <- drop_nodes(semPlot::semPlotModel(fit), c("x02"))
out_no_x02 <- semPaths(fit_no_x02, "est")
fit_no_x08_x10 <- drop_nodes(semPlot::semPlotModel(fit), c("x08", "x10"))
out_no_x08_x10 <- semPaths(fit_no_x08_x10, "est")
fit_no_f1 <- drop_nodes(semPlot::semPlotModel(fit), c("f1"))
out_no_f1 <- semPaths(fit_no_f1, "est")
fit_no_f2_f4 <- drop_nodes(semPlot::semPlotModel(fit), c("f2", "f4"))
out_no_f2_f4 <- semPaths(fit_no_f2_f4, "est")
fit_no_f1_x13_x09 <- drop_nodes(semPlot::semPlotModel(fit), c("f1", "x13", "x09"))
out_no_f1_x13_x09 <- semPaths(fit_no_f1_x13_x09, "est")

full_labels <- sort(semPaths(fit)$graphAttributes$Nodes$labels)

test_that("Correct nodes dropped", {
  expect_equal(
    sort(out_no_x01$graphAttributes$Nodes$labels),
    sort(full_labels[!(full_labels %in% "x01")]),
    check.attributes = FALSE
  )
  expect_equal(
    sort(out_no_x02$graphAttributes$Nodes$labels),
    sort(full_labels[!(full_labels %in% "x02")]),
    check.attributes = FALSE
  )
  expect_equal(
    sort(out_no_x08_x10$graphAttributes$Nodes$labels),
    sort(full_labels[!(full_labels %in% c("x08", "x10"))]),
    check.attributes = FALSE
  )
  expect_equal(
    sort(out_no_f1$graphAttributes$Nodes$labels),
    sort(full_labels[!(full_labels %in% "f1")]),
    check.attributes = FALSE
  )
  expect_equal(
    sort(out_no_f2_f4$graphAttributes$Nodes$labels),
    sort(full_labels[!(full_labels %in% c("f2", "f4"))]),
    check.attributes = FALSE
  )
  expect_equal(
    sort(out_no_f1_x13_x09$graphAttributes$Nodes$labels),
    sort(full_labels[!(full_labels %in% c("f1", "x13", "x09"))]),
    check.attributes = FALSE
  )
})

# keep nodes

# path model

mod_pa <- 
  'x1 ~~ x2
   x3 ~  x1 + x2
   x4 ~  x1 + x3
  '
fit <- lavaan::sem(mod_pa, pa_example)

semPaths(fit)

fit_no1 <- keep_nodes(semPlot::semPlotModel(fit), c("x2", "x4", "x3"))
out_no1 <- semPaths(fit_no1, "est")
fit_no2 <- keep_nodes(semPlot::semPlotModel(fit), c("x4", "x3", "x1"))
out_no2 <- semPaths(fit_no2, "est")
fit_no3 <- keep_nodes(semPlot::semPlotModel(fit), c("x4", "x1", "x2"))
out_no3 <- semPaths(fit_no3, "est")
fit_no4 <- keep_nodes(semPlot::semPlotModel(fit), c("x2", "x1", "x3"))
out_no4 <- semPaths(fit_no4, "est")
fit_no13 <- keep_nodes(semPlot::semPlotModel(fit), c("x2", "x4"))
out_no13 <- semPaths(fit_no13, "est")

test_that("Correct nodes dropped", {
  expect_equal(
    out_no1$graphAttributes$Nodes$labels,
    c("x3", "x4", "x2"),
    check.attributes = FALSE
  )
  expect_equal(
    out_no2$graphAttributes$Nodes$labels,
    c("x3", "x4", "x1"),
    check.attributes = FALSE
  )
  expect_equal(
    out_no3$graphAttributes$Nodes$labels,
    c("x4", "x1", "x2"),
    check.attributes = FALSE
  )
  expect_equal(
    out_no4$graphAttributes$Nodes$labels,
    c("x3", "x1", "x2"),
    check.attributes = FALSE
  )
  expect_equal(
    out_no13$graphAttributes$Nodes$labels,
    c("x4", "x2"),
    check.attributes = FALSE
  )
})

# SEM

mod <- 
  'f1 =~ x01 + x02 + x03
   f2 =~ x04 + x05 + x06 + x07
   f3 =~ x08 + x09 + x10
   f4 =~ x11 + x12 + x13 + x14
   f3 ~  f1 + f2
   f4 ~  f1 + f3
  '
fit <- lavaan::sem(mod, sem_example)

semPaths(fit)

full_labels <- sort(semPaths(fit)$graphAttributes$Nodes$labels)


fit_no_x01 <- keep_nodes(semPlot::semPlotModel(fit),
                         full_labels[full_labels != "x01"])
out_no_x01 <- semPaths(fit_no_x01, "est")
fit_no_x02 <- keep_nodes(semPlot::semPlotModel(fit),
                         full_labels[full_labels != "x02"])
out_no_x02 <- semPaths(fit_no_x02, "est")
fit_no_x08_x10 <- keep_nodes(semPlot::semPlotModel(fit),
                         full_labels[!(full_labels %in% c("x08", "x10"))])
out_no_x08_x10 <- semPaths(fit_no_x08_x10, "est")
fit_no_f1 <- keep_nodes(semPlot::semPlotModel(fit),
                         full_labels[!(full_labels %in% c("f1"))])
out_no_f1 <- semPaths(fit_no_f1, "est")
fit_no_f2_f4 <- keep_nodes(semPlot::semPlotModel(fit),
                         full_labels[!(full_labels %in% c("f2", "f4"))])
out_no_f2_f4 <- semPaths(fit_no_f2_f4, "est")
fit_no_f1_x13_x09 <- keep_nodes(semPlot::semPlotModel(fit),
                         full_labels[!(full_labels %in% c("f1", "x13", "x09"))])
out_no_f1_x13_x09 <- semPaths(fit_no_f1_x13_x09, "est")


test_that("Correct nodes dropped", {
  expect_equal(
    sort(out_no_x01$graphAttributes$Nodes$labels),
    sort(full_labels[!(full_labels %in% "x01")]),
    check.attributes = FALSE
  )
  expect_equal(
    sort(out_no_x02$graphAttributes$Nodes$labels),
    sort(full_labels[!(full_labels %in% "x02")]),
    check.attributes = FALSE
  )
  expect_equal(
    sort(out_no_x08_x10$graphAttributes$Nodes$labels),
    sort(full_labels[!(full_labels %in% c("x08", "x10"))]),
    check.attributes = FALSE
  )
  expect_equal(
    sort(out_no_f1$graphAttributes$Nodes$labels),
    sort(full_labels[!(full_labels %in% "f1")]),
    check.attributes = FALSE
  )
  expect_equal(
    sort(out_no_f2_f4$graphAttributes$Nodes$labels),
    sort(full_labels[!(full_labels %in% c("f2", "f4"))]),
    check.attributes = FALSE
  )
  expect_equal(
    sort(out_no_f1_x13_x09$graphAttributes$Nodes$labels),
    sort(full_labels[!(full_labels %in% c("f1", "x13", "x09"))]),
    check.attributes = FALSE
  )
})
