# Identify dependent Variable residual variance

Check which parameters in a lavaan output are the residual variance of a
dependent variable.

## Usage

``` r
is_dv_residvar(lavaan_out)
```

## Arguments

- lavaan_out:

  A [lavaan::lavaan](https://rdrr.io/pkg/lavaan/man/lavaan-class.html)
  object.

## Value

A boolean vector with length equal to the number of rows in the lavaan
output.

## Details

Check which parameters in a lavaan output are the variance of a
dependent variable. Indicators of a latent variable will be excluded.

## Examples

``` r

mod <-
 'x1 ~~ x2
  x3 ~  x1 + x2
  x4 ~  x1 + x3
 '
fit_pa <- lavaan::sem(mod, pa_example)
is_dv_residvar(fit_pa)
#> x1 ~~ x2  x3 ~ x1  x3 ~ x2  x4 ~ x1  x4 ~ x3 x3 ~~ x3 x4 ~~ x4 x1 ~~ x1 
#>    FALSE    FALSE    FALSE    FALSE    FALSE     TRUE     TRUE    FALSE 
#> x2 ~~ x2 
#>    FALSE 

mod <-
 'f1 =~ x01 + x02 + x03
  f2 =~ x04 + x05 + x06 + x07
  f3 =~ x08 + x09 + x10
  f4 =~ x11 + x12 + x13 + x14
 '
fit_cfa <- lavaan::cfa(mod, cfa_example)
is_dv_residvar(fit_cfa)
#>  f1 =~ x01  f1 =~ x02  f1 =~ x03  f2 =~ x04  f2 =~ x05  f2 =~ x06  f2 =~ x07 
#>      FALSE      FALSE      FALSE      FALSE      FALSE      FALSE      FALSE 
#>  f3 =~ x08  f3 =~ x09  f3 =~ x10  f4 =~ x11  f4 =~ x12  f4 =~ x13  f4 =~ x14 
#>      FALSE      FALSE      FALSE      FALSE      FALSE      FALSE      FALSE 
#> x01 ~~ x01 x02 ~~ x02 x03 ~~ x03 x04 ~~ x04 x05 ~~ x05 x06 ~~ x06 x07 ~~ x07 
#>      FALSE      FALSE      FALSE      FALSE      FALSE      FALSE      FALSE 
#> x08 ~~ x08 x09 ~~ x09 x10 ~~ x10 x11 ~~ x11 x12 ~~ x12 x13 ~~ x13 x14 ~~ x14 
#>      FALSE      FALSE      FALSE      FALSE      FALSE      FALSE      FALSE 
#>   f1 ~~ f1   f2 ~~ f2   f3 ~~ f3   f4 ~~ f4   f1 ~~ f2   f1 ~~ f3   f1 ~~ f4 
#>      FALSE      FALSE      FALSE      FALSE      FALSE      FALSE      FALSE 
#>   f2 ~~ f3   f2 ~~ f4   f3 ~~ f4 
#>      FALSE      FALSE      FALSE 

mod <-
 'f1 =~ x01 + x02 + x03
  f2 =~ x04 + x05 + x06 + x07
  f3 =~ x08 + x09 + x10
  f4 =~ x11 + x12 + x13 + x14
  f3 ~  f1 + f2
  f4 ~  f1 + f3
 '
fit_sem <- lavaan::sem(mod, sem_example)
is_dv_residvar(fit_sem)
#>  f1 =~ x01  f1 =~ x02  f1 =~ x03  f2 =~ x04  f2 =~ x05  f2 =~ x06  f2 =~ x07 
#>      FALSE      FALSE      FALSE      FALSE      FALSE      FALSE      FALSE 
#>  f3 =~ x08  f3 =~ x09  f3 =~ x10  f4 =~ x11  f4 =~ x12  f4 =~ x13  f4 =~ x14 
#>      FALSE      FALSE      FALSE      FALSE      FALSE      FALSE      FALSE 
#>    f3 ~ f1    f3 ~ f2    f4 ~ f1    f4 ~ f3 x01 ~~ x01 x02 ~~ x02 x03 ~~ x03 
#>      FALSE      FALSE      FALSE      FALSE      FALSE      FALSE      FALSE 
#> x04 ~~ x04 x05 ~~ x05 x06 ~~ x06 x07 ~~ x07 x08 ~~ x08 x09 ~~ x09 x10 ~~ x10 
#>      FALSE      FALSE      FALSE      FALSE      FALSE      FALSE      FALSE 
#> x11 ~~ x11 x12 ~~ x12 x13 ~~ x13 x14 ~~ x14   f1 ~~ f1   f2 ~~ f2   f3 ~~ f3 
#>      FALSE      FALSE      FALSE      FALSE      FALSE      FALSE       TRUE 
#>   f4 ~~ f4   f1 ~~ f2 
#>       TRUE      FALSE 
```
