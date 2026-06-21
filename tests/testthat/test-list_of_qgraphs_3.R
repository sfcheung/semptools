library(lavaan)
library(semPlot)

test_that("list of qgraphs: auto_layout_mediation", {

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
set.seed(4321)
dat$gp <- sample(
  c("gp1", "gp2", "gp3"),
  size = nrow(dat),
  replace = TRUE
)
fit <- lavaan::sem(mod_pa2,
                   dat,
                   group = "gp",
                   meanstructure = FALSE)
fit_nogp <- lavaan::sem(mod_pa2,
                   dat,
                   meanstructure = FALSE)
pm <- semPlotModel(fit) |> drop_nodes(c("c1", "c2", "c3"))
p <- semPaths(
          pm,
          whatLabels = "est",
          DoNotPlot = TRUE
        )
pm_nogp <- semPlotModel(fit_nogp) |> drop_nodes(c("c1", "c2", "c3"))
p_nogp <- semPaths(
          pm_nogp,
          whatLabels = "est",
          DoNotPlot = TRUE
        )

# ==== auto_layout_mediation ====

m <- auto_layout_mediation(
          fit,
          exclude = c("c1", "c2", "c3"),
          v_pos = "lower",
          x = "x1",
          y = "y"
        )
m_nogp <- auto_layout_mediation(
          fit_nogp,
          exclude = c("c1", "c2", "c3"),
          v_pos = "lower",
          x = "x1",
          y = "y"
        )

expect_identical(
  m,
  m_nogp
)

p0 <- auto_layout_mediation(
          p,
          exclude = c("c1", "c2", "c3"),
          v_pos = "lower",
          x = "x1",
          y = "y"
        )
p0_nogp <- auto_layout_mediation(
          p_nogp,
          exclude = c("c1", "c2", "c3"),
          v_pos = "lower",
          x = "x1",
          y = "y"
        )

expect_identical(
  p0[[2]]$layout,
  p0_nogp$layout
)

# ==== safe_edge_label_position ====

pos_new <- safe_edge_label_position(
              fit,
              m
            )
pos_new_nogp <- safe_edge_label_position(
              fit_nogp,
              m_nogp
            )
pos_new_p <- safe_edge_label_position(
              p0,
              m
            )
pos_new_p_nogp <- safe_edge_label_position(
              p0_nogp,
              m
            )

expect_equal(
  pos_new,
  pos_new_nogp
)
expect_equal(
  pos_new_p[[1]]$graphAttributes$Edges$edge.label.position,
  pos_new_p_nogp$graphAttributes$Edges$edge.label.position
)

# ==== safe_resid_position ====

pos_new <- safe_resid_position(
              fit,
              m
            )
pos_new_nogp <- safe_resid_position(
              fit_nogp,
              m_nogp
            )
pos_new_p <- safe_resid_position(
              p0,
              m
            )
pos_new_p_nogp <- safe_resid_position(
              p0_nogp,
              m
            )

expect_equal(
  pos_new,
  pos_new_nogp
)
expect_equal(
  pos_new_p[[1]]$graphAttributes$Nodes$loopRotation,
  pos_new_p_nogp$graphAttributes$Nodes$loopRotation
)


})
