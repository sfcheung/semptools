# Convert a named vector to a list of lists

Convert a named vector to a list of lists, to be used by various
functions in
[`semptools`](https://sfcheung.github.io/semptools/reference/semptools-package.md).

## Usage

``` r
to_list_of_lists(input, name1 = NULL, name2 = NULL, name3 = NULL)
```

## Arguments

- input:

  A named vector

- name1:

  The name for the first element in the list-in-list. Default is `NULL`.

- name2:

  The name for the second element in the list-in-list. Defaultis `NULL`.

- name3:

  The name for the third element in the list-in-list. Default is `NULL`.
  If this argument is not `NULL`, the names of the vector elements will
  be split using `lavaan` syntax (by calling
  [`lavaan::lavParseModelString()`](https://rdrr.io/pkg/lavaan/man/model.syntax.html)),
  and the right-hand side (`rhs`) and left-hand side (`lhs`) of each
  element will be assigned to `name1` and `name2`, respectively.

## Value

A list of lists.

## Details

This function is not to be used by users, but to be used internally by
other functions of
[`semptools`](https://sfcheung.github.io/semptools/reference/semptools-package.md).

## Examples

``` r
x <- c("x1 ~~ x2" = -1, "x4 ~ x1" = 1)
to_list_of_lists(x, name1 = "from", name2 = "to", name3 = "new_curve")
#> [[1]]
#> [[1]]$from
#> [1] "x2"
#> 
#> [[1]]$to
#> [1] "x1"
#> 
#> [[1]]$new_curve
#> [1] -1
#> 
#> 
#> [[2]]
#> [[2]]$from
#> [1] "x1"
#> 
#> [[2]]$to
#> [1] "x4"
#> 
#> [[2]]$new_curve
#> [1] 1
#> 
#> 
#list(list(from = "x1", to = "x2", new_curve = -1),
#     list(from = "x1", to = "x4", new_curve =  1))

y <- c(x1 = 0, x2 = 180, x3 = 140, x4 = 140)
to_list_of_lists(y, name1 = "node", name2 = "rotate")
#> [[1]]
#> [[1]]$node
#> [1] "x1"
#> 
#> [[1]]$rotate
#> [1] 0
#> 
#> 
#> [[2]]
#> [[2]]$node
#> [1] "x2"
#> 
#> [[2]]$rotate
#> [1] 180
#> 
#> 
#> [[3]]
#> [[3]]$node
#> [1] "x3"
#> 
#> [[3]]$rotate
#> [1] 140
#> 
#> 
#> [[4]]
#> [[4]]$node
#> [1] "x4"
#> 
#> [[4]]$rotate
#> [1] 140
#> 
#> 
#list(list(node = "x1", rotate =   0),
#     list(node = "x2", rotate = 180),
#     list(node = "x3", rotate = 140),
#     list(node = "x4", rotate = 140))
```
