# Sample dataset pa_example

A sample dataset for fitting a path analysis model.

## Usage

``` r
pa_example
```

## Format

An object of class `data.frame` with 100 rows and 4 columns.

## Details

Four variables (x1 to x4), 100 cases.

Sample model to fit (in
[lavaan::model.syntax](https://rdrr.io/pkg/lavaan/man/model.syntax.html)
notation)

    mod <-
     'x1 ~~ x2
      x3 ~  x1 + x2
      x4 ~  x1 + x3
     '
