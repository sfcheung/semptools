library(lavaan)
library(semPlot)

# From the example of lavaan::cfa()
# Based on https://github.com/sfcheung/semptools/issues/162
mod <-
"
visual =~ x1 + x2 + x3
textual =~ x4 + x5 + x6
speed =~ x7 + x8 + x9
"
# Use a small sample to have variation in p-values
dat <- HolzingerSwineford1939[1:160, paste0("x", 1:9)]
dat <- data.frame(scale(dat))
dat <- lapply(dat,
              function(x) {
                  as.numeric(cut(x,
                                 breaks = c(-Inf, -1.5, 1.5, Inf)))
                })
dat <- data.frame(dat)

fit <- cfa(mod,
           dat,
           ordered = TRUE)
p0 <- semPlotModel(fit)
p <- semPlot::semPaths(fit,
                       thresholds = TRUE,
                       whatLabels = "standardized",
                       intercepts = FALSE,
                       style = "ram",
                       DoNotPlot = TRUE)

# Check mark_sig()

p1 <- mark_sig(p,
               object = fit,
               std_type = "std.all")
p1$graphAttributes$Edges$labels
if (interactive()) {
  plot(p1)
}

std <- standardizedSolution(fit)
std <- std[std$op %in% c("=~", "~~", "~"), ]

p2 <- mark_sig(p,
               ests = std)
p2$graphAttributes$Edges$labels
if (interactive()) {
  plot(p2)
}

alphas <- c("*" = .05, "**" = .01, "***" = .001)
alphas_sorted <- sort(alphas, decreasing = FALSE)
std$sig <- sapply(std$pvalue, function(x) {
                      ind <- which(x < alphas_sorted)[1]
                      ifelse(is.na(ind), "", names(ind[1]))
                    })
std$plotlabels <- paste0(formatC(std$est.std, digits = 2, format = "f"),
                         std$sig)
std$plotlabels
p2$graphAttributes$Edges$labels

test_that("check asterisks", {
expect_identical(std$plotlabels,
                 p2$graphAttributes$Edges$labels[1:24])
expect_identical(p1$graphAttributes$Edges$labels,
                 p2$graphAttributes$Edges$labels)
})

# Check mark_se()

pse1 <- mark_se(p,
                object = fit,
                std_type = "std.all")
pse1$graphAttributes$Edges$labels
if (interactive()) {
  plot(pse1)
}

std <- standardizedSolution(fit)
std <- std[std$op %in% c("=~", "~~", "~"), ]

pse2 <- mark_se(p,
                ests = std)
pse2$graphAttributes$Edges$labels
if (interactive()) {
  plot(pse2)
}

std$se_str <- formatC(std$se, digits = 2, format = "f")
std$plotlabels <- paste0(formatC(std$est.std, digits = 2, format = "f"),
                         " (",
                         std$se_str,
                         ")")
std$plotlabels
pse2$graphAttributes$Edges$labels

test_that("check se", {
expect_identical(std$plotlabels,
                 pse2$graphAttributes$Edges$labels[1:24])
expect_identical(pse1$graphAttributes$Edges$labels,
                 pse2$graphAttributes$Edges$labels)
})
