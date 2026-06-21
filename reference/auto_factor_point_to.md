# Create a Matrix for 'factor_point_to'

Use a named vector or named arguments to create a matrix of the
directions of indicators of factors.

## Usage

``` r
auto_factor_point_to(factor_layout, ...)
```

## Arguments

- factor_layout:

  A layout matrix to be used in the `layout` argument, such as one
  created by
  [`layout_matrix()`](https://sfcheung.github.io/semptools/reference/layout_matrix.md).

- ...:

  Additional arguments. If the first argument is not named, then it
  should be a named vector of directions, names being the names of the
  factors, and directions can be one of these values: `"up"`, `"down"`,
  `"left"`, `"right"`. Other arguments are ignored. If the arguments are
  named, then the names of the arguments are the names of the factors,
  and the argument values are the direction for the factors.

## Value

A character matrix of the same dimension as `factor_layout`. The cells
of factor names are replaced by the directions to place their
indicators.

## Details

A helper function to make it easier to create the matrix used by
[`set_sem_layout()`](https://sfcheung.github.io/semptools/reference/set_sem_layout.md)
to indicate where the indicators of each factor should be positioned.

It works in two modes. If the first argument is a named vector, such as
`c(f1 = "up", f2 = "down")`, then this vector will be used to create the
direction matrix.

If the arguments are named, such as
`auto_factor_point_to(factor_layout, f1 = "up", f2 = "down"`, then the
names are treated as the factor names, and the values of the arguments
are treated as the directions.

The matrix created can then be used for the argument `factor_point_to`
in
[`set_sem_layout()`](https://sfcheung.github.io/semptools/reference/set_sem_layout.md).

## See also

[`set_sem_layout()`](https://sfcheung.github.io/semptools/reference/set_sem_layout.md)

## Examples

``` r
factor_layout <- matrix(c("f1",   NA,   NA,
                            NA, "f3", "f4",
                          "f2",   NA,   NA), byrow = TRUE, 3, 3)
factor_point_to <- auto_factor_point_to(factor_layout,
                                        f1 = "left",
                                        f2 = "left",
                                        f3 = "down",
                                        f4 = "down")
factor_point_to
#>      [,1]   [,2]   [,3]  
#> [1,] "left" NA     NA    
#> [2,] NA     "down" "down"
#> [3,] "left" NA     NA    
```
