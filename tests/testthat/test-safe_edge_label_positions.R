library(lavaan)
library(semPlot)

test_that("safe_edge_label_position", {

mod_pa2 <-
"
m11 ~ c1 + x1
m21 ~ c2 + m11
m22 ~ m11 + c3
y ~ m2 + m21 + m22 + x1
y1 ~ m2 + x1
"

fit <- lavaan::sem(mod_pa2,
                    do.fit = FALSE)
dat <- simulateData(parameterTable(fit),
                    sample.nobs = 500,
                    seed = 1234)
fit <- lavaan::sem(mod_pa2,
                   dat)
beta0 <- lavaan::lavInspect(fit,
                            "free")$beta

m <- auto_layout_mediation(
          fit,
          exclude = c("c1", "c2", "c3"),
          v_pos = "lower"
        )
pm <- semPlotModel(fit) |> drop_nodes(c("c1", "c2", "c3"))
# semPaths(
#           pm,
#           whatLabels = "est",
#           layout = m)

p <- semPaths(
          pm,
          whatLabels = "est",
          layout = m,
          DoNotPlot = TRUE)

pos_new <- safe_edge_label_position(
              fit,
              m
            )
if (length(pos_new) > 0) {
  p2 <- set_edge_label_position(
              p,
              pos_new
            )
}
pos_chk <- c(0.5, 0.5, 0.275, 0.275, 0.5, 0.5, 0.275, 0.5, 0.275, 0.5, 0.5,
0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5)

expect_equal(p2$graphAttributes$Edges$edge.label.position,
             pos_chk)

pos_new2 <- safe_edge_label_position(
              p,
              update_plot = FALSE
            )

expect_equal(pos_new2[names(pos_new)],
             pos_new)

p3 <- p |> safe_edge_label_position()

# plot(p3)

expect_equal(p3$graphAttributes$Edges$edge.label.position,
             p2$graphAttributes$Edges$edge.label.position)

# Use existing edge positions as the default positions

p4 <- set_edge_label_position(
            p,
            c("y1 ~ x1" = .1,
              "m22 ~ m11" = .75)
          )
# plot(p4)

p4b <- safe_edge_label_position(p4)

# plot(p4b)

e_pos <- p4b$graphAttributes$Edges$edge.label.position
expect_equal(e_pos[edge_index(p4b, "m11", "m22")],
             .75)
expect_equal(e_pos[edge_index(p4b, "x1", "y1")],
              .10)

})

test_that("no unsafe label", {

mod_pa2 <-
"
m11 ~ x1
m21 ~ m11
m22 ~ x1
y ~ m2 + m21 + m22 + x1
"

fit <- lavaan::sem(mod_pa2,
                    do.fit = FALSE)
dat <- simulateData(parameterTable(fit),
                    sample.nobs = 500,
                    seed = 1234)
fit <- lavaan::sem(mod_pa2,
                   dat)

m <- auto_layout_mediation(
          fit,
          v_pos = "lower"
        )
pm <- semPlotModel(fit)

p <- semPaths(
          pm,
          whatLabels = "est",
          layout = m,
          DoNotPlot = TRUE)

# plot(p)

p_new <- safe_edge_label_position(p)

expect_identical(p_new,
                 p)

})
