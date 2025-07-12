# semptools 0.3.1.13

## New Features

- Added quick plot functions for
  common models: `q_simple()` for
  simple mediation models, `q_serial()`
  for serial mediation models, and
  `q_parallel()` for parallel mediation
  models. (0.3.1.3, 0.3.1.4)

- Added `auto_layout_mediation()` for
  generating a layout matrix automatically.
  It can also update an exist plot of
  `semPlot::semPaths()`.
  (0.3.1.5, 0.3.1.9, 0.3.1.10)

- Added `safe_edge_label_position()` to
  reposition edge labels away from the
  intersections between paths.
  (0.3.1.6)

- Added `safe_resid_position()` to
  reposition the residual (or R-square)
  of a node away from paths connected to
  this node. (0.3.1.7)

- Added `move_node()` to move a node
  in a plot, for adjusting the layout
  of a plot after it has been generated.
  (0.3.1.13)

## Improvement

- Updated `mark_sig()` and `mark_se()`
  to support plots of multigroup models.
  (0.3.1.1)

- Updated `add_rsq()` to support plots
  with structural paths only.
  (0.3.1.2)

- Updated `mark_sig()` to process
  R-squares if *p*-values are provided
  by users. (0.3.1.8)

## Miscellaneous

- Update to depend on R 4.1.0 or above.
  (0.3.1.11)

# semptools 0.3.1

## Improvement

- Updated `mark_sig()` and `mark_se()`
  to preliminarily support plots with
  intercepts. (0.3.0.1)

- Updated `add_rsq()` to support plots
  with intercepts. (0.3.0.2)

## Bug Fixes

- The functions `mark_sig()` and `mark_se()`
  now work for models with ordinal
  variables. (0.3.0.1)

# semptools 0.3.0

## New Features

- Added `set_edge_attribute()` to
  change any attribute of the edges.
  (0.2.10.2)

- Added `set_node_attribute()` to
  change any attribute of the nodes.
  (0.2.10.3)

- Added `set_edge_color()` to
  change the colors of edges.
  (0.2.10.2)

- Added `rescale_layout()` to expand
  the plot to fit the rectangle bounded
  by -1 and 1 vertically and
  horizontally. (0.2.11.1)

- Added `add_rsq()` to add R-squares
  to endogenous variables. They will
  replace the residual variances in
  the plot. (0.2.11.4)

## Improvement

- Updated `mark_sig()` to accept a
  data frame with the *p*-values. Users
  can supply *p*-values computed by
  other functions. (0.2.11.3)

- Updated `mark_se()` to accept a
  data frame with the standard errors.
  Users can supply standard errors
  computed by other functions. (0.2.11.3)

- Updated `mark_sig()` to use the *p*-values
  from `lavaan::standardizedSolution()`.
  Users need to explicitly request
  standardized solution *p*-values by
  setting the argument `std_type`.
  (0.2.11.3)

- Updated `mark_se()` to use the
  standard errors
  from `lavaan::standardizedSolution()`.
  Users need to explicitly request
  standardized solution standard errors
  by setting the argument `std_type`.
  (0.2.11.3)

- Functions that change the attributes
  of an edge, such as `set_edge_label_position()`,
  should now supports factor loadings by
  using the `=~` operator when specifying
  the edge. Previously, we needed to specify
  a loading as if it were a regression
  path (e.g., `x1 ~ f1`). (0.2.11.5)

## Miscellaneous

- Revised `change_node_label()` to
  address an issue with `plot.qgraph()`.
  `label.cex` should now be used as
  expected. (0.2.10.1)

- Fixed an R CMD check issue with
  some links in Rd files. (0.2.10.4)

- Start to use Use 0.X.0 for each initial
  submission to CRAN. (0.3.0)

# semptools 0.2.10

## New Features

- Added `auto_factor_point_to()` for
  creating the `factor_point_to` matrix.
  Revised `set_sem_layout()` to allow
  users to use a named vector for
  the `factor_point_to` argument. (0.2.9.15)
- Added `auto_indicator_order()` and
  `lavaan_indicator_order()` for setting
  indicator order in `set_sem_layout()`
  and `set_cfa_layout()`. Can handle
  nodes with labels changed. (0.2.9.18, 0.2.9.24)
- Revised `set_cfa_layout()` and
  `set_sem_layout()` to set
  `indicator_order` and
  `indicator_factor` automatically if
  not supplied. Node labels must be
  string for this option to work. (0.2.9.18, 0.2.9.23)
- Added an `pkgdown` articles on setting
  the layout for a model with both latent
  factors and exogenous observed variables. (0.2.9.25)

## Bug Fixes

- Fixed `set_cfa_layout()` to work for
  LISREL-style graphs. (0.2.9.13)
- Fixed `set_cfa_layout()` to work for
  a model without factor covariances
  (e.g., `exoCov = FALSE` when calling
  `semPlot::semPaths()`). (0.2.9.14)
- Fixed a bug in `auto_factor_point_to()`:
  Cells with no direction specified is now
  set to `NA`. (0.2.9.21)

## Miscellaneous

- Added an R CMD check for noSuggests. (0.2.9.12)
- Fixed a bug in the setting for `pkgdown`. (0.2.9.16)
- Add `DoNotPlot = TRUE` in all tests to
  prevent `semPlot::semPaths()` from
  plotting the graphs in the tests. (0.2.9.17, 0.2.9.20)
- Added the helper `add_object()`. (0.2.9.18)
- Removed `dplyr` functions from the code
  and removed `dplyr` from `Imports`. (0.2.9.19)
- Removed the check for factors with
  no direction specified in `auto_factor_point_to()`.
  The "factor" may be a manifest variable without
  indicators. (0.2.9.20)
- Added two internal helpers to check
  node labels (labels changed?
  labels non-string?). (0.2.9.22)
- Removed the mention of `change_node_label2`,
  which was not exported, from the help page. (0.2.9.26, 0.2.9.27)
- Made the warning and error messages of
  `set_cfa_layout()` and `set_sem_layout()`
  more informative. (0.2.9.28)

# semptools 0.2.9.11

- Used the native functions of `semPlot` (`semPlot::man()` and `semPlot::lat()`)
  to check nodes in `drop_nodes()` and `keep_nodes()`. As a consequence,
  the `semPlot` package is now in the Import section. (More native functions
  will be used in the future to ensure compatibility.) (0.2.9.11)


# semptools 0.2.9.10

- Updated pkgdown site.

# semptools 0.2.9.9

- Added support for 2nd order factor (see `vignette("second_order")`). (0.2.9.7)
- Fixed doc due to Roxygen updated to 7.2.1. (0.2.9.7)
- Update the GitHub actions. (0.2.9.8)
- Fixed doc due to Roxygen updated to 7.2.3. (0.2.9.9)
- Fixed `set_curve()`. It should now work for bidirectional
  edges regardless of the order of the nodes in the specification. (0.2.9.9)

# semptools 0.2.9.6

- Fixed several problems with `pkgdown` setting.

- `Roxygen` updated to 7.2.0 and some man pages are updated accordingly.

# semptools 0.2.9.5

- Updated `set_sem_layout()` to support observed exogenous variables.

# semptools 0.2.9.4

- Updated `drop_nodes()`: It now works with output without a covariance matrix
  (e.g, a model generated from `lavaan::lavaanify()` without data).

# semptools 0.2.9.3

- Minor fixes to errors in CRAN checks.

# semptools 0.2.9.2

- Minor fixes to the DESCRIPTION file and examples.

# semptools 0.2.9.1

## Bug Fix

- Fixed a bug in `set_sem_layout()`. Failed to work with models in which all
factors have the same number of indicators.

## Misc

- Reformatted the help sections of the functions (wrap the text for readability).
  This has no impact on uses and the generated help files.

# semptools 0.2.9

- Added `drop_nodes()` and `keep_nodes()`. They process a `semPlotModel` object,
  which is generated by `semPlot::semPlotModel()`, drop or keep selected nodes
  (e.g., observed variables, latent factors) from the object. The result can
  then be passed to `semPlot::semPaths()` to draw a diagram without the dropped
  nodes.

- Add `layout_matrix`. A helper function for creating a layout matrix to be used
  by `semPlot::semPaths()`. Users specify the positions of nodes and the function
  will create the matrix accordingly.

- `mark_sig()`, `mark_se()`, and `set_cfa_layout()` will raise an error if the
  `semPlot::semPaths` object has intercepts terms. These function do not support
  plots with intercept terms yet.

- Updated `change_node_label()` to support named vectors.

- Added `vignette("layout_matrix")` to explain how layout matrix is used in
  `semPlot::semPaths()`, and how `layout_matrix()` can be used to construct the
   layout matrix.

- Updated the vignettes to use named vectors instead of "list of named lists"
  in some functions.

- Updated `vignette("semptools")` to introduce `change_node_label()`.

- `magrittr` is no longer required for installing the package.

# semptools 0.2.8.1

- Added `change_node_label()` for changing the labels of nodes. Several other functions
  were modified to adapt for this function.

- Added `to_list_of_lists()` for converting a named vector to a list of lists. Specifying
  a list of lists is necessary in some cases because the a label may not be string (e.g.,
  it may be an expression). However, in most cases, all elements are strings or numbers
  and so a named vector will do. This function is to be used internally by other functions,
  not to be used by users.

# semptools 0.2.8

- Fix a bug in `set_cfa_layout()`. It now will not raise an error for one-factor models.

- Fix some typo errors in documentation pages.

# semptools 0.2.7

- Import the pipe operator from `magrittr` so users no need to load the package themselves.

# semptools 0.2.6

- Update the documentation of `mark_sig()` and `mark_se()` to emphasize that
 currently they require a `lavaan` output.

# semptools 0.2.5

- Used `pkgdown` to build a site. The first draft, with minimal customization.

# semptools 0.2.4

- Alpha release. Ready for testing.
