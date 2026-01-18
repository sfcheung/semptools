# Sample dataset sem_2nd_order_example

A sample dataset for fitting a latent variable model with two 2nd-order
factors.

## Usage

``` r
sem_2nd_order_example
```

## Format

An object of class `data.frame` with 500 rows and 21 columns.

## Details

Twenty one variables (x01 to x21), 500 cases.

Sample model to fit (in
[lavaan::model.syntax](https://rdrr.io/pkg/lavaan/man/model.syntax.html)
notation)

    mod <-
     'f1 =~ x01 + x02 + x03
      f2 =~ x04 + x05 + x06 + x07
      f3 =~ x08 + x09 + x10
      f4 =~ x11 + x12 + x13 + x14
      f5 =~ x15 + x16 + x17 + x18
      f6 =~ x19 + x20 + x21
      f21 =~ 1*f1 + f3 + f4
      f22 =~ 1*f2 + f5 + f6
      f22 ~ f21
    '
