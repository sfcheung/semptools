# Sample dataset pa_example_3covs

A sample dataset for fitting a path analysis model, with three control
variables.

## Usage

``` r
pa_example_3covs
```

## Format

An object of class `data.frame` with 100 rows and 7 columns.

## Details

Four variables (`x1` to `x4`), and three control variables (`cov1`,
`cov2`, `cov3`), 100 cases.

Sample model to fit (in
[lavaan::model.syntax](https://rdrr.io/pkg/lavaan/man/model.syntax.html)
notation)

    mod <-
    '
    x3 ~  x1 + x2 + cov1 +cov2 + cov3
    x4 ~  x1 + x3 + cov1 +cov2 + cov3
    '
