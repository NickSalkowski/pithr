## What is a *pith*?

One definition of *pith* is *the essence of something*.  When used as a verb, 
*pith* means *to remove the pith from*.  In the context of the pithr package, 
a *pith* is a simple summary plot of the data contained in an object.  That 
is, the *pith* of your data is the *essence* of your data.

## Installing the pithr package

pithr is not available on CRAN, but it is available on GitHub.  To install 
pithr directly from GitHub, first install the [devtools](https://github.com/hadley/devtools) package, then use `devtools::install_github`.

```
install.packages("devtools")
devtools::install_github("NickSalkowski/pithr")
```

## Using pithr

The most basic command in the pithr package is `pith`.  To generate a pith 
plot, use the `pith` function.

```{r}
library(pithr)
pith(iris$Sepal.Length)
```

`pith` produces different plots depending on the type of data.

```{r}
set.seed(1234)
X_int <- rpois(n = 100, lambda = 5)
pith(X_int)
X_num <- rnorm(n = 100, mean = 5, sd = sqrt(5))
pith(X_num)
X_char <- LETTERS[X_int]
pith(X_char)
X_fact <- factor(X_char)
pith(X_fact)
X_log <- as.logical(rbinom(100, 1, 0.625))
pith(X_log)
X_Date <- as.Date(
  sample.int(10000, size = 100, replace = TRUE), 
  origin = "1970-01-01")
pith(X_Date)
```

Matrices and arrays are collapsed into vectors by `pith`.  
```{r}
X_mat <- matrix(
  rpois(400, 100),
  ncol = 20)
pith(X_mat)
```

If `pith` is given a list or data.frame, each element or column is summarized separately.

```{r}
pith(cars)
```

So, `pith` handles a wide variety of data types and structures, producing frequency plots or histograms, depending on the data itself.

## Non-Finite Values

`pith` separates non-finite values, and presents their frequencies separately.
```{r}
X_char_NA <- c(X_char, rep(NA, 25))
pith(X_char_NA)
X_num_NA <- c(X_num, 
              rep(c(-Inf, Inf), times = c(5, 10)),
              rep(c(NaN, NA), times = c(15, 20)))
pith(X_num_NA)
```

## Arguments

`pith` accepts several arguments that help control the plot.

- plot If TRUE, a plot is produced.
- xname The "name" of the data.  This is used to produce the main plot title.
- breaks, include.lowest, right These arguments are passed to the `hist` 
  function, if a histogram is produced.
- ... Additional plot arguments.

```{r}
pith(X_num_NA, xname = "Numeric Vector Example", breaks = 5, las = 1)
```

## Pithy Returns

`pith` invisibly returns a pith class object, which is a list that contains 
summary statistics for the data.  There are some situations where it would 
be useful to return the data object inself, instead.  `pithy` is a convenience 
function that calls `pith`, but returns the object.

```{r}
X_unif <- pithy(runif(100))
```

This behavior is particularly handy when used with the 
[magrittr](https://github.com/smbache/magrittr) and 
[dplyr](https://github.com/hadley/dplyr) packages.

```{r}
library(magrittr)
X_gamma <- rgamma(100, 2, 2) %>%
  pithy
library(dplyr)
iris %>%
  select(Sepal.Length) %>%
  pithy %>%
  transmute(SLsq = Sepal.Length ^ 2) %>%
  pith
```

pithr also contains helper functions `filter_pithy`, `select_pithy`, 
`mutate_pithy`, and `transmute_pithy` that filter, select, mutate, and 
transmute data sets before calling `pithy`.  Just like `pithy`, they 
return the original data set (before any filtering, selecting, or mutating).

```{r}
iris %>%
  select_pithy(Petal.Length) %>%
  transmute_pithy(PLsq = Petal.Length ^ 2) %>%
  tbl_df
```
