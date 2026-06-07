# Determine the Order of Indicators Using a 'lavaan' Model Syntax

Determine the order of indicators and match indicators and factors based
on a 'lavaan' model syntax.

## Usage

``` r
lavaan_indicator_order(model_syntax)
```

## Arguments

- model_syntax:

  A string that should be a model specified in `lavaan` model syntax.
  Only the factor structure (operator `=~`) in the model will be used.

## Value

A named character vector. The values are the indicators in the model
syntax. The names are the latent factors the indicators loaded on.

## Details

It generates a named vector for the argument `indicator_order` of
[`set_cfa_layout()`](https://sfcheung.github.io/semptools/reference/set_cfa_layout.md)
and
[`set_sem_layout()`](https://sfcheung.github.io/semptools/reference/set_sem_layout.md)
using a `lavaan` model syntax.

A variable is considered an indicator if it is on the right-hand side of
the operator `=~`.

If an indicator loaded on more than one latent variable, it will only be
matched to one of them, determined by the order of appearance in the
internal storage.

## See also

[`set_sem_layout()`](https://sfcheung.github.io/semptools/reference/set_sem_layout.md)
and
[`set_cfa_layout()`](https://sfcheung.github.io/semptools/reference/set_cfa_layout.md).

## Examples

``` r

mod <-
  'f1 =~ x01 + x02 + x03 + x06
   f4 =~ x11 + x12 + x13 + x14
   f2 =~ x04 + x05 + x06 + x07
   f3 =~ x08 + x09 + x10 + x03
  '
lavaan_indicator_order(mod)
#>    f1    f1    f1    f1    f4    f4    f4    f4    f2    f2    f2    f3    f3 
#> "x01" "x02" "x03" "x06" "x11" "x12" "x13" "x14" "x04" "x05" "x07" "x08" "x09" 
#>    f3 
#> "x10" 

mod <-
  'f1 =~ x01 + x02 + x03 + x06
   f3 =~ x08 + x09 + x10 + x03
   f2 =~ x04 + x05 + x06 + x07
   f4 =~ x11 + x12 + x13 + x14
   f3 ~ f1 + f2
   f4 ~ f3
  '
lavaan_indicator_order(mod)
#>    f1    f1    f1    f1    f3    f3    f3    f2    f2    f2    f4    f4    f4 
#> "x01" "x02" "x03" "x06" "x08" "x09" "x10" "x04" "x05" "x07" "x11" "x12" "x13" 
#>    f4 
#> "x14" 
```
