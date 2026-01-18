# Sample dataset sem_example

A sample dataset for fitting a latent variable model.

## Usage

``` r
sem_example
```

## Format

An object of class `data.frame` with 200 rows and 14 columns.

## Details

Fourteen variables (x01 to x14), 100 cases.

Sample model to fit (in
[lavaan::model.syntax](https://rdrr.io/pkg/lavaan/man/model.syntax.html)
notation)

    mod <-
     'f1 =~ x01 + x02 + x03
      f2 =~ x04 + x05 + x06 + x07
      f3 =~ x08 + x09 + x10
      f4 =~ x11 + x12 + x13 + x14
      f3 ~  f1 + f2
      f4 ~  f1 + f3
     '
