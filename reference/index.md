# Package index

## Main Functions

Functions for processing a plot

- [`mark_sig()`](https://sfcheung.github.io/semptools/reference/mark_sig.md)
  : Mark Parameter Estimates (Edge Labels) Based on p-Value
- [`mark_se()`](https://sfcheung.github.io/semptools/reference/mark_se.md)
  [`mark_ci()`](https://sfcheung.github.io/semptools/reference/mark_se.md)
  : Add Standard Error/Confidence Interval Estimates to Parameter
  Estimates (Edge Labels)
- [`add_rsq()`](https://sfcheung.github.io/semptools/reference/add_rsq.md)
  : Add R-Squares to Endogenous Variables
- [`set_curve()`](https://sfcheung.github.io/semptools/reference/set_curve.md)
  : Bend or Straighten Selected edges
- [`set_edge_color()`](https://sfcheung.github.io/semptools/reference/set_edge_color.md)
  : Set the Colors of Selected Edges
- [`set_edge_label_position()`](https://sfcheung.github.io/semptools/reference/set_edge_label_position.md)
  : Set the positions of edge labels of selected edges
- [`safe_edge_label_position()`](https://sfcheung.github.io/semptools/reference/safe_edge_label_position.md)
  : Adjust Edge Label Positions to Avoid Overlapping Labels
- [`rotate_resid()`](https://sfcheung.github.io/semptools/reference/rotate_resid.md)
  : Rotate the residuals of selected nodes
- [`safe_resid_position()`](https://sfcheung.github.io/semptools/reference/safe_resid_position.md)
  : Adjust Residual Positions
- [`change_node_label()`](https://sfcheung.github.io/semptools/reference/change_node_label.md)
  : Change node labels
- [`drop_nodes()`](https://sfcheung.github.io/semptools/reference/keep_drop_nodes.md)
  [`keep_nodes()`](https://sfcheung.github.io/semptools/reference/keep_drop_nodes.md)
  : Keep or drop nodes
- [`set_edge_attribute()`](https://sfcheung.github.io/semptools/reference/set_edge_attribute.md)
  : Set the Attributes of Selected Edges
- [`set_node_attribute()`](https://sfcheung.github.io/semptools/reference/set_node_attribute.md)
  : Set the Attributes of Selected Nodes

## Layouts

Main functions for setting the layout

- [`set_cfa_layout()`](https://sfcheung.github.io/semptools/reference/set_cfa_layout.md)
  : Configure the layout of factors of a CFA graph by semPaths

- [`set_sem_layout()`](https://sfcheung.github.io/semptools/reference/set_sem_layout.md)
  :

  Configure the layout of factors of an SEM graph by
  [semPlot::semPaths](https://rdrr.io/pkg/semPlot/man/semPaths.html)

## Quick Plots

Simple-to-use functions for common models

- [`quick_parallel_mediation()`](https://sfcheung.github.io/semptools/reference/quick_sem_plot.md)
  [`q_parallel()`](https://sfcheung.github.io/semptools/reference/quick_sem_plot.md)
  [`quick_serial_mediation()`](https://sfcheung.github.io/semptools/reference/quick_sem_plot.md)
  [`q_serial()`](https://sfcheung.github.io/semptools/reference/quick_sem_plot.md)
  [`quick_simple_mediation()`](https://sfcheung.github.io/semptools/reference/quick_sem_plot.md)
  [`q_simple()`](https://sfcheung.github.io/semptools/reference/quick_sem_plot.md)
  : Quick Plots of Common Models

## Layout Helpers

Helpers for setting the layout

- [`layout_matrix()`](https://sfcheung.github.io/semptools/reference/layout_matrix.md)
  : Create the layout matrix for semPaths
- [`rescale_layout()`](https://sfcheung.github.io/semptools/reference/rescale_layout.md)
  : Rescale the Layout
- [`auto_indicator_order()`](https://sfcheung.github.io/semptools/reference/auto_indicator_order.md)
  : Determine the Order of Indicators Automatically
- [`lavaan_indicator_order()`](https://sfcheung.github.io/semptools/reference/lavaan_indicator_order.md)
  : Determine the Order of Indicators Using a 'lavaan' Model Syntax
- [`auto_factor_point_to()`](https://sfcheung.github.io/semptools/reference/auto_factor_point_to.md)
  : Create a Matrix for 'factor_point_to'
- [`auto_layout_mediation()`](https://sfcheung.github.io/semptools/reference/auto_layout_mediation.md)
  : Set the Layout of a Mediation Model Automatically
- [`move_node()`](https://sfcheung.github.io/semptools/reference/move_node.md)
  : Move Nodes in a Plot

## Other Helpers

- [`is_dv_residvar()`](https://sfcheung.github.io/semptools/reference/is_dv_residvar.md)
  : Identify dependent Variable residual variance
- [`to_list_of_lists()`](https://sfcheung.github.io/semptools/reference/to_list_of_lists.md)
  : Convert a named vector to a list of lists
- [`add_object()`](https://sfcheung.github.io/semptools/reference/add_object.md)
  : Add a Fit Object to a 'qgraph' Object

## Datasets

Datasets used in examples and vignettes.

- [`cfa_example`](https://sfcheung.github.io/semptools/reference/cfa_example.md)
  : Sample dataset pa_example
- [`pa_example`](https://sfcheung.github.io/semptools/reference/pa_example.md)
  : Sample dataset pa_example
- [`pa_example_3covs`](https://sfcheung.github.io/semptools/reference/pa_example_3covs.md)
  : Sample dataset pa_example_3covs
- [`sem_2nd_order_example`](https://sfcheung.github.io/semptools/reference/sem_2nd_order_example.md)
  : Sample dataset sem_2nd_order_example
- [`sem_example`](https://sfcheung.github.io/semptools/reference/sem_example.md)
  : Sample dataset sem_example
