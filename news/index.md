# Changelog

## semptools 0.3.3.19

### Improvement

- Updated
  [`set_edge_attribute()`](https://sfcheung.github.io/semptools/reference/set_edge_attribute.md)
  to skip edges not in a plot, without throwing an error. (0.3.3.2)

- Added the argument `check_direction` to
  [`set_edge_attribute()`](https://sfcheung.github.io/semptools/reference/set_edge_attribute.md)
  and related functions. If set to `FALSE`, then the direction of an
  edge will be ignored. That is, both `y ~ x` and `y ~~ x` will denote
  edges `y <- x`, `y -> x`, and `y <-> x`. (0.3.3.3)

- Updated
  [`add_rsq()`](https://sfcheung.github.io/semptools/reference/add_rsq.md),
  [`mark_se()`](https://sfcheung.github.io/semptools/reference/mark_se.md),
  [`mark_ci()`](https://sfcheung.github.io/semptools/reference/mark_se.md),
  and
  [`mark_sig()`](https://sfcheung.github.io/semptools/reference/mark_sig.md)
  to store the fit object in the plot. When `object` is not set but has
  been stored in a plot, these functions will retrieve the stored fit
  object. This makes it not necessary to supply the same fit object in
  all calls to these functions. (0.3.3.4)

- Updated
  [`layout_matrix()`](https://sfcheung.github.io/semptools/reference/layout_matrix.md)
  to ensure that the output always has at least three columns. (0.3.3.5)

- Updated
  [`set_node_attribute()`](https://sfcheung.github.io/semptools/reference/set_node_attribute.md)
  to set the attributes of all nodes using one value. (0.3.3.6)

- Updated
  [`set_edge_attribute()`](https://sfcheung.github.io/semptools/reference/set_edge_attribute.md)
  to set the attributes of all nodes using one value. (0.3.3.7)

- Added
  [`set_node_color()`](https://sfcheung.github.io/semptools/reference/set_node_color.md),
  [`set_node_height()`](https://sfcheung.github.io/semptools/reference/set_node_size.md),
  [`set_node_label_color()`](https://sfcheung.github.io/semptools/reference/set_node_label.md),
  [`set_node_label_size()`](https://sfcheung.github.io/semptools/reference/set_node_label.md),
  [`set_node_shape()`](https://sfcheung.github.io/semptools/reference/set_node_size.md),
  `set_node_sizes()`,
  [`set_node_width()`](https://sfcheung.github.io/semptools/reference/set_node_size.md).
  (0.3.3.8)

- Removed `nChar = 0` in some tests to allow testing functions with
  abbreviated names. Also revised some functions to make them work for
  both the original names of nodes and their display names, which may be
  abbreviated. (0.3.3.9)

- Updated
  [`set_node_attribute()`](https://sfcheung.github.io/semptools/reference/set_node_attribute.md)
  to use new helpers, so that it is easier to work with original node
  names, processed (e.g., abbreviated) node names, and labels (converted
  to string, if necessary). (0.3.3.10)

- Updated node functions to use
  [`set_node_attribute()`](https://sfcheung.github.io/semptools/reference/set_node_attribute.md)
  as applicable. (0.3.3.11)

- Added
  [`get_node_attribute()`](https://sfcheung.github.io/semptools/reference/get_node_attribute.md).
  (0.3.3.12)

- Added
  [`get_edge_attribute()`](https://sfcheung.github.io/semptools/reference/get_edge_attribute.md).
  (0.3.3.13)

- Added
  [`set_edge_label()`](https://sfcheung.github.io/semptools/reference/set_edge_label_attributes.md)
  and
  [`set_edge_label_bg()`](https://sfcheung.github.io/semptools/reference/set_edge_label_attributes.md).
  (0.3.3.14)

- Added
  [`set_edge_line_type()`](https://sfcheung.github.io/semptools/reference/set_edge_line_type.md).
  (0.3.3.14)

- Added the `how` argument to
  [`set_edge_attribute()`](https://sfcheung.github.io/semptools/reference/set_edge_attribute.md),
  [`set_node_attribute()`](https://sfcheung.github.io/semptools/reference/set_node_attribute.md),
  and friends (if applicable). The default mode is `how = "ratio"` if
  applicable. (0.3.3.14)

- Added
  [`set_edge_line_width()`](https://sfcheung.github.io/semptools/reference/set_edge_line_width.md).
  (0.3.3.14)

- Added
  [`set_edge_label_size()`](https://sfcheung.github.io/semptools/reference/set_edge_label_attributes.md).
  (0.3.3.14)

- Added
  [`set_node_border_width()`](https://sfcheung.github.io/semptools/reference/set_node_border.md),
  and
  [`set_node_border_color()`](https://sfcheung.github.io/semptools/reference/set_node_border.md).
  (0.3.3.14)

- Added
  [`set_graph_margins()`](https://sfcheung.github.io/semptools/reference/set_graph_attributes.md),
  [`node_labels_equal_scale()`](https://sfcheung.github.io/semptools/reference/set_graph_attributes.md),
  and
  [`set_node_labels_equal_scale()`](https://sfcheung.github.io/semptools/reference/set_graph_attributes.md).
  (0.3.3.15)

- Updated these functions to support a list of `qgraph` objects:
  [`set_edge_attribute()`](https://sfcheung.github.io/semptools/reference/set_edge_attribute.md),
  [`set_node_attribute()`](https://sfcheung.github.io/semptools/reference/set_node_attribute.md),
  [`rotate_resid()`](https://sfcheung.github.io/semptools/reference/rotate_resid.md),
  [`set_curve()`](https://sfcheung.github.io/semptools/reference/set_curve.md),
  [`set_edge_color()`](https://sfcheung.github.io/semptools/reference/set_edge_color.md),
  [`set_edge_label()`](https://sfcheung.github.io/semptools/reference/set_edge_label_attributes.md),
  [`set_edge_label_bg()`](https://sfcheung.github.io/semptools/reference/set_edge_label_attributes.md),
  [`set_edge_label_position()`](https://sfcheung.github.io/semptools/reference/set_edge_label_attributes.md),
  [`set_edge_label_size()`](https://sfcheung.github.io/semptools/reference/set_edge_label_attributes.md),
  [`set_edge_line_type()`](https://sfcheung.github.io/semptools/reference/set_edge_line_type.md),
  [`set_edge_line_width()`](https://sfcheung.github.io/semptools/reference/set_edge_line_width.md),
  [`set_node_border_color()`](https://sfcheung.github.io/semptools/reference/set_node_border.md),
  [`set_node_border_width()`](https://sfcheung.github.io/semptools/reference/set_node_border.md),
  [`set_node_color()`](https://sfcheung.github.io/semptools/reference/set_node_color.md),
  [`set_node_height()`](https://sfcheung.github.io/semptools/reference/set_node_size.md),
  [`set_node_label_color()`](https://sfcheung.github.io/semptools/reference/set_node_label.md),
  [`set_node_label_size()`](https://sfcheung.github.io/semptools/reference/set_node_label.md),
  [`set_node_shape()`](https://sfcheung.github.io/semptools/reference/set_node_size.md),
  [`set_node_size()`](https://sfcheung.github.io/semptools/reference/set_node_size.md),
  [`set_node_width()`](https://sfcheung.github.io/semptools/reference/set_node_size.md),
  [`move_node()`](https://sfcheung.github.io/semptools/reference/move_node.md),
  [`node_labels_equal_scale()`](https://sfcheung.github.io/semptools/reference/set_graph_attributes.md),
  [`rescale_layout()`](https://sfcheung.github.io/semptools/reference/rescale_layout.md),
  [`set_graph_margins()`](https://sfcheung.github.io/semptools/reference/set_graph_attributes.md),
  [`set_node_labels_equal_scale()`](https://sfcheung.github.io/semptools/reference/set_graph_attributes.md),
  and
  [`change_node_label()`](https://sfcheung.github.io/semptools/reference/change_node_label.md).
  (0.3.3.16)

- Updated
  [`get_node_attribute()`](https://sfcheung.github.io/semptools/reference/get_node_attribute.md)
  and
  [`get_edge_attribute()`](https://sfcheung.github.io/semptools/reference/get_edge_attribute.md)
  to work with a list of `qgraph` objects, though they will only get the
  attribute values from the first object. (0.3.3.16)

- Updated these functions to support a list of `qgraph` objects:
  [`add_rsq()`](https://sfcheung.github.io/semptools/reference/add_rsq.md),
  [`auto_layout_mediation()`](https://sfcheung.github.io/semptools/reference/auto_layout_mediation.md),
  [`safe_edge_label_position()`](https://sfcheung.github.io/semptools/reference/safe_edge_label_position.md),
  [`safe_resid_position()`](https://sfcheung.github.io/semptools/reference/safe_resid_position.md),
  [`set_cfa_layout()`](https://sfcheung.github.io/semptools/reference/set_cfa_layout.md),
  [`set_sem_layout()`](https://sfcheung.github.io/semptools/reference/set_sem_layout.md).
  (0.3.3.17)

### Miscellaneous

- Updated to `roxygen2` 8.0.0. (0.3.3.1)

- Updated
  [`set_curve()`](https://sfcheung.github.io/semptools/reference/set_curve.md)
  and
  [`set_edge_label_position()`](https://sfcheung.github.io/semptools/reference/set_edge_label_attributes.md),
  to use
  [`set_edge_attribute()`](https://sfcheung.github.io/semptools/reference/set_edge_attribute.md)
  internally. (0.3.3.2)

- Updated some pages in the `pkgdown` site. (0.3.3.18)

- Fixed a multigroup bug in
  [`mark_sig()`](https://sfcheung.github.io/semptools/reference/mark_sig.md).
  (0.3.3.19)

## semptools 0.3.3

CRAN release: 2026-03-17

### New Features

- Added
  [`mark_ci()`](https://sfcheung.github.io/semptools/reference/mark_se.md)
  for adding confidence intervals to a plot. (0.3.2.2)

### Improvement

- Updated to treat rectangles as manifest variables and ellipses as
  latent variables (0.3.2.1)

- Updated
  [`set_edge_label_position()`](https://sfcheung.github.io/semptools/reference/set_edge_label_attributes.md)
  to handle bidirectional edges specified in an order different from
  that in `lavaan`’s parameter table. (0.3.2.3)

## semptools 0.3.2

CRAN release: 2025-07-12

### New Features

- Added quick plot functions for common models:
  [`q_simple()`](https://sfcheung.github.io/semptools/reference/quick_sem_plot.md)
  for simple mediation models,
  [`q_serial()`](https://sfcheung.github.io/semptools/reference/quick_sem_plot.md)
  for serial mediation models, and
  [`q_parallel()`](https://sfcheung.github.io/semptools/reference/quick_sem_plot.md)
  for parallel mediation models. (0.3.1.3, 0.3.1.4)

- Added
  [`auto_layout_mediation()`](https://sfcheung.github.io/semptools/reference/auto_layout_mediation.md)
  for generating a layout matrix automatically. It can also update an
  exist plot of
  [`semPlot::semPaths()`](https://rdrr.io/pkg/semPlot/man/semPaths.html).
  (0.3.1.5, 0.3.1.9, 0.3.1.10)

- Added
  [`safe_edge_label_position()`](https://sfcheung.github.io/semptools/reference/safe_edge_label_position.md)
  to reposition edge labels away from the intersections between paths.
  (0.3.1.6)

- Added
  [`safe_resid_position()`](https://sfcheung.github.io/semptools/reference/safe_resid_position.md)
  to reposition the residual (or R-square) of a node away from paths
  connected to this node. (0.3.1.7)

- Added
  [`move_node()`](https://sfcheung.github.io/semptools/reference/move_node.md)
  to move a node in a plot, for adjusting the layout of a plot after it
  has been generated. (0.3.1.13)

### Improvement

- Updated
  [`mark_sig()`](https://sfcheung.github.io/semptools/reference/mark_sig.md)
  and
  [`mark_se()`](https://sfcheung.github.io/semptools/reference/mark_se.md)
  to support plots of multigroup models. (0.3.1.1)

- Updated
  [`add_rsq()`](https://sfcheung.github.io/semptools/reference/add_rsq.md)
  to support plots with structural paths only. (0.3.1.2)

- Updated
  [`mark_sig()`](https://sfcheung.github.io/semptools/reference/mark_sig.md)
  to process R-squares if *p*-values are provided by users. (0.3.1.8)

### Miscellaneous

- Update to depend on R 4.1.0 or above. (0.3.1.11)

## semptools 0.3.1

CRAN release: 2024-11-09

### Improvement

- Updated
  [`mark_sig()`](https://sfcheung.github.io/semptools/reference/mark_sig.md)
  and
  [`mark_se()`](https://sfcheung.github.io/semptools/reference/mark_se.md)
  to preliminarily support plots with intercepts. (0.3.0.1)

- Updated
  [`add_rsq()`](https://sfcheung.github.io/semptools/reference/add_rsq.md)
  to support plots with intercepts. (0.3.0.2)

### Bug Fixes

- The functions
  [`mark_sig()`](https://sfcheung.github.io/semptools/reference/mark_sig.md)
  and
  [`mark_se()`](https://sfcheung.github.io/semptools/reference/mark_se.md)
  now work for models with ordinal variables. (0.3.0.1)

## semptools 0.3.0

CRAN release: 2024-10-21

### New Features

- Added
  [`set_edge_attribute()`](https://sfcheung.github.io/semptools/reference/set_edge_attribute.md)
  to change any attribute of the edges. (0.2.10.2)

- Added
  [`set_node_attribute()`](https://sfcheung.github.io/semptools/reference/set_node_attribute.md)
  to change any attribute of the nodes. (0.2.10.3)

- Added
  [`set_edge_color()`](https://sfcheung.github.io/semptools/reference/set_edge_color.md)
  to change the colors of edges. (0.2.10.2)

- Added
  [`rescale_layout()`](https://sfcheung.github.io/semptools/reference/rescale_layout.md)
  to expand the plot to fit the rectangle bounded by -1 and 1 vertically
  and horizontally. (0.2.11.1)

- Added
  [`add_rsq()`](https://sfcheung.github.io/semptools/reference/add_rsq.md)
  to add R-squares to endogenous variables. They will replace the
  residual variances in the plot. (0.2.11.4)

### Improvement

- Updated
  [`mark_sig()`](https://sfcheung.github.io/semptools/reference/mark_sig.md)
  to accept a data frame with the *p*-values. Users can supply
  *p*-values computed by other functions. (0.2.11.3)

- Updated
  [`mark_se()`](https://sfcheung.github.io/semptools/reference/mark_se.md)
  to accept a data frame with the standard errors. Users can supply
  standard errors computed by other functions. (0.2.11.3)

- Updated
  [`mark_sig()`](https://sfcheung.github.io/semptools/reference/mark_sig.md)
  to use the *p*-values from
  [`lavaan::standardizedSolution()`](https://rdrr.io/pkg/lavaan/man/standardizedSolution.html).
  Users need to explicitly request standardized solution *p*-values by
  setting the argument `std_type`. (0.2.11.3)

- Updated
  [`mark_se()`](https://sfcheung.github.io/semptools/reference/mark_se.md)
  to use the standard errors from
  [`lavaan::standardizedSolution()`](https://rdrr.io/pkg/lavaan/man/standardizedSolution.html).
  Users need to explicitly request standardized solution standard errors
  by setting the argument `std_type`. (0.2.11.3)

- Functions that change the attributes of an edge, such as
  [`set_edge_label_position()`](https://sfcheung.github.io/semptools/reference/set_edge_label_attributes.md),
  should now supports factor loadings by using the `=~` operator when
  specifying the edge. Previously, we needed to specify a loading as if
  it were a regression path (e.g., `x1 ~ f1`). (0.2.11.5)

### Miscellaneous

- Revised
  [`change_node_label()`](https://sfcheung.github.io/semptools/reference/change_node_label.md)
  to address an issue with `plot.qgraph()`. `label.cex` should now be
  used as expected. (0.2.10.1)

- Fixed an R CMD check issue with some links in Rd files. (0.2.10.4)

- Start to use Use 0.X.0 for each initial submission to CRAN. (0.3.0)

## semptools 0.2.10

CRAN release: 2023-10-15

### New Features

- Added
  [`auto_factor_point_to()`](https://sfcheung.github.io/semptools/reference/auto_factor_point_to.md)
  for creating the `factor_point_to` matrix. Revised
  [`set_sem_layout()`](https://sfcheung.github.io/semptools/reference/set_sem_layout.md)
  to allow users to use a named vector for the `factor_point_to`
  argument. (0.2.9.15)
- Added
  [`auto_indicator_order()`](https://sfcheung.github.io/semptools/reference/auto_indicator_order.md)
  and
  [`lavaan_indicator_order()`](https://sfcheung.github.io/semptools/reference/lavaan_indicator_order.md)
  for setting indicator order in
  [`set_sem_layout()`](https://sfcheung.github.io/semptools/reference/set_sem_layout.md)
  and
  [`set_cfa_layout()`](https://sfcheung.github.io/semptools/reference/set_cfa_layout.md).
  Can handle nodes with labels changed. (0.2.9.18, 0.2.9.24)
- Revised
  [`set_cfa_layout()`](https://sfcheung.github.io/semptools/reference/set_cfa_layout.md)
  and
  [`set_sem_layout()`](https://sfcheung.github.io/semptools/reference/set_sem_layout.md)
  to set `indicator_order` and `indicator_factor` automatically if not
  supplied. Node labels must be string for this option to work.
  (0.2.9.18, 0.2.9.23)
- Added an `pkgdown` articles on setting the layout for a model with
  both latent factors and exogenous observed variables. (0.2.9.25)

### Bug Fixes

- Fixed
  [`set_cfa_layout()`](https://sfcheung.github.io/semptools/reference/set_cfa_layout.md)
  to work for LISREL-style graphs. (0.2.9.13)
- Fixed
  [`set_cfa_layout()`](https://sfcheung.github.io/semptools/reference/set_cfa_layout.md)
  to work for a model without factor covariances (e.g., `exoCov = FALSE`
  when calling
  [`semPlot::semPaths()`](https://rdrr.io/pkg/semPlot/man/semPaths.html)).
  (0.2.9.14)
- Fixed a bug in
  [`auto_factor_point_to()`](https://sfcheung.github.io/semptools/reference/auto_factor_point_to.md):
  Cells with no direction specified is now set to `NA`. (0.2.9.21)

### Miscellaneous

- Added an R CMD check for noSuggests. (0.2.9.12)
- Fixed a bug in the setting for `pkgdown`. (0.2.9.16)
- Add `DoNotPlot = TRUE` in all tests to prevent
  [`semPlot::semPaths()`](https://rdrr.io/pkg/semPlot/man/semPaths.html)
  from plotting the graphs in the tests. (0.2.9.17, 0.2.9.20)
- Added the helper
  [`add_object()`](https://sfcheung.github.io/semptools/reference/add_object.md).
  (0.2.9.18)
- Removed `dplyr` functions from the code and removed `dplyr` from
  `Imports`. (0.2.9.19)
- Removed the check for factors with no direction specified in
  [`auto_factor_point_to()`](https://sfcheung.github.io/semptools/reference/auto_factor_point_to.md).
  The “factor” may be a manifest variable without indicators. (0.2.9.20)
- Added two internal helpers to check node labels (labels changed?
  labels non-string?). (0.2.9.22)
- Removed the mention of `change_node_label2`, which was not exported,
  from the help page. (0.2.9.26, 0.2.9.27)
- Made the warning and error messages of
  [`set_cfa_layout()`](https://sfcheung.github.io/semptools/reference/set_cfa_layout.md)
  and
  [`set_sem_layout()`](https://sfcheung.github.io/semptools/reference/set_sem_layout.md)
  more informative. (0.2.9.28)

## semptools 0.2.9.11

CRAN release: 2023-05-31

- Used the native functions of `semPlot`
  ([`semPlot::man()`](https://rdrr.io/pkg/semPlot/man/edits.html) and
  [`semPlot::lat()`](https://rdrr.io/pkg/semPlot/man/edits.html)) to
  check nodes in
  [`drop_nodes()`](https://sfcheung.github.io/semptools/reference/keep_drop_nodes.md)
  and
  [`keep_nodes()`](https://sfcheung.github.io/semptools/reference/keep_drop_nodes.md).
  As a consequence, the `semPlot` package is now in the Import section.
  (More native functions will be used in the future to ensure
  compatibility.) (0.2.9.11)

## semptools 0.2.9.10

- Updated pkgdown site.

## semptools 0.2.9.9

- Added support for 2nd order factor (see
  [`vignette("second_order")`](https://sfcheung.github.io/semptools/articles/second_order.md)).
  (0.2.9.7)
- Fixed doc due to Roxygen updated to 7.2.1. (0.2.9.7)
- Update the GitHub actions. (0.2.9.8)
- Fixed doc due to Roxygen updated to 7.2.3. (0.2.9.9)
- Fixed
  [`set_curve()`](https://sfcheung.github.io/semptools/reference/set_curve.md).
  It should now work for bidirectional edges regardless of the order of
  the nodes in the specification. (0.2.9.9)

## semptools 0.2.9.6

CRAN release: 2022-08-25

- Fixed several problems with `pkgdown` setting.

- `Roxygen` updated to 7.2.0 and some man pages are updated accordingly.

## semptools 0.2.9.5

- Updated
  [`set_sem_layout()`](https://sfcheung.github.io/semptools/reference/set_sem_layout.md)
  to support observed exogenous variables.

## semptools 0.2.9.4

- Updated
  [`drop_nodes()`](https://sfcheung.github.io/semptools/reference/keep_drop_nodes.md):
  It now works with output without a covariance matrix (e.g, a model
  generated from
  [`lavaan::lavaanify()`](https://rdrr.io/pkg/lavaan/man/model.syntax.html)
  without data).

## semptools 0.2.9.3

CRAN release: 2021-10-11

- Minor fixes to errors in CRAN checks.

## semptools 0.2.9.2

CRAN release: 2021-09-27

- Minor fixes to the DESCRIPTION file and examples.

## semptools 0.2.9.1

### Bug Fix

- Fixed a bug in
  [`set_sem_layout()`](https://sfcheung.github.io/semptools/reference/set_sem_layout.md).
  Failed to work with models in which all factors have the same number
  of indicators.

### Misc

- Reformatted the help sections of the functions (wrap the text for
  readability). This has no impact on uses and the generated help files.

## semptools 0.2.9

- Added
  [`drop_nodes()`](https://sfcheung.github.io/semptools/reference/keep_drop_nodes.md)
  and
  [`keep_nodes()`](https://sfcheung.github.io/semptools/reference/keep_drop_nodes.md).
  They process a `semPlotModel` object, which is generated by
  [`semPlot::semPlotModel()`](https://rdrr.io/pkg/semPlot/man/semPlotModel.html),
  drop or keep selected nodes (e.g., observed variables, latent factors)
  from the object. The result can then be passed to
  [`semPlot::semPaths()`](https://rdrr.io/pkg/semPlot/man/semPaths.html)
  to draw a diagram without the dropped nodes.

- Add `layout_matrix`. A helper function for creating a layout matrix to
  be used by
  [`semPlot::semPaths()`](https://rdrr.io/pkg/semPlot/man/semPaths.html).
  Users specify the positions of nodes and the function will create the
  matrix accordingly.

- [`mark_sig()`](https://sfcheung.github.io/semptools/reference/mark_sig.md),
  [`mark_se()`](https://sfcheung.github.io/semptools/reference/mark_se.md),
  and
  [`set_cfa_layout()`](https://sfcheung.github.io/semptools/reference/set_cfa_layout.md)
  will raise an error if the
  [`semPlot::semPaths`](https://rdrr.io/pkg/semPlot/man/semPaths.html)
  object has intercepts terms. These function do not support plots with
  intercept terms yet.

- Updated
  [`change_node_label()`](https://sfcheung.github.io/semptools/reference/change_node_label.md)
  to support named vectors.

- Added
  [`vignette("layout_matrix")`](https://sfcheung.github.io/semptools/articles/layout_matrix.md)
  to explain how layout matrix is used in
  [`semPlot::semPaths()`](https://rdrr.io/pkg/semPlot/man/semPaths.html),
  and how
  [`layout_matrix()`](https://sfcheung.github.io/semptools/reference/layout_matrix.md)
  can be used to construct the layout matrix.

- Updated the vignettes to use named vectors instead of “list of named
  lists” in some functions.

- Updated
  [`vignette("semptools")`](https://sfcheung.github.io/semptools/articles/semptools.md)
  to introduce
  [`change_node_label()`](https://sfcheung.github.io/semptools/reference/change_node_label.md).

- `magrittr` is no longer required for installing the package.

## semptools 0.2.8.1

- Added
  [`change_node_label()`](https://sfcheung.github.io/semptools/reference/change_node_label.md)
  for changing the labels of nodes. Several other functions were
  modified to adapt for this function.

- Added
  [`to_list_of_lists()`](https://sfcheung.github.io/semptools/reference/to_list_of_lists.md)
  for converting a named vector to a list of lists. Specifying a list of
  lists is necessary in some cases because the a label may not be string
  (e.g., it may be an expression). However, in most cases, all elements
  are strings or numbers and so a named vector will do. This function is
  to be used internally by other functions, not to be used by users.

## semptools 0.2.8

- Fix a bug in
  [`set_cfa_layout()`](https://sfcheung.github.io/semptools/reference/set_cfa_layout.md).
  It now will not raise an error for one-factor models.

- Fix some typo errors in documentation pages.

## semptools 0.2.7

- Import the pipe operator from `magrittr` so users no need to load the
  package themselves.

## semptools 0.2.6

- Update the documentation of
  [`mark_sig()`](https://sfcheung.github.io/semptools/reference/mark_sig.md)
  and
  [`mark_se()`](https://sfcheung.github.io/semptools/reference/mark_se.md)
  to emphasize that currently they require a `lavaan` output.

## semptools 0.2.5

- Used `pkgdown` to build a site. The first draft, with minimal
  customization.

## semptools 0.2.4

- Alpha release. Ready for testing.
