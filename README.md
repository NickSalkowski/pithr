# pithr

Quick graphical data summaries!

### What is pithr?

**pithr** is an R package built to generate quick graphical data summaries.  That is, to generate the graphical equivalent of the **summary()** function.  These summaries are called *piths*. **pithr** is intended for exploratory data analysis, debugging, or just checking that your data manipulation worked.

### Piths?

*Pith*, when used as a noun, can mean *the essence of something*.  *Pith*, when used as a verb, can mean *to remove the pith*.  Since pithr is built to provide descriptive summary plots of data, the vocabulary seemed appropriate.  And *pith* is just four letters long, and I don't like unnecessary typing.

### Installing **pithr**

**pithr** is only available on GitHub, for now.  The easiest way is to install using the **install_github()** function in the **devtools** package:
```
install.packages("devtools")
devtools::install_github("NickSalkowski/pithr")
```

### Using **pithr**

**pithr** is designed to be easy to use.  Just provide a factor, character, logical, integer, or numeric vector to **pith()**. 
```
library(pithr)
pith(iris$Sepal.Length)
```
If you want proportions of densities instead of frequencies, set **freq** to **FALSE**. 
```
pith(iris$Sepal.Length, freq = FALSE)
```

If you provide a list or data.frame to **pith()**, you will get piths for each element of the list or data.frame (as long as the elements are data vectors).  For example:

```
pith(iris)
```

**pith()** has arguments **col** and **border** that let you specify colors for the plot.  You can specify a different color or border for the bar associate with **NA** values if you want.

If you want the **pith** object without the plot, just set **plot** equal to **FALSE**.

```
iris_pith <- pith(iris, plot = FALSE)
```
### **pithy()**

**pithy()** is a function that generates the same plots as **pith()** (as long as plot = TRUE, of course), but instead of returning a **pith**, it returns the original data.  So, pithy may actually be more useful, since it can be added expressions without changing the results.  This could be useful when using chained expressions with the **dplyr** or **magrittr** packages.

```
head(pithy(cars))
library(dplyr)
iris %>%
  tbl_df %>%
  select(Sepal.Length, Species) %>%
  pithy %>%
  filter(Species == 'virginica')   
```

### **NA** Values

One of the important features of **pith()** is that it handles missing values, when they are present.  The proportions or densities are scaled to take into account the proportion of **NA** values in the data.

```
X <- rnorm(80)
pith(X, freq = FALSE)
XNA <- c(X, rep(NA, 20))
pith(XNA, freq = FALSE)
```

### **dplyr** functionality

**pithr** provides a set of **dplyr** helper functions for **pithy()**.  **filter_pithy()** filters a data set, generates the plots, then returns the original pre-filter data set.  **select_pithy()**, **mutate_pithy()**, and **transmute_pithy** do the same thing for **select()**, **mutate()**, and **transmute()**, repectively.  These functions are intended to facilitate inserting short plotting detours into a chained **dplyr** expression, without changing the overall result.

```
iris %>%
  tbl_df %>%
  select_pithy(Petal.Width) %>%
  transmute_pithy(SL2 = Sepal.Length^2)
```
Which is similar to:

```
library(magrittr)
iris %>% tbl_df %T>% {
  select(., Petal.Width) %>%
  pith} %T>% {
  transmute(., SL2 = Sepal.Length^2) %>% pith}
```

Note, I haven't figured out how to make **pith()** pay attention to any grouping variables in a tbl_df, yet.

### Base Graphics???

**ggplot2** is great, but I'm a lot more familiar with using base graphics, so I used base graphics.  Since **pithr** is meant for generating plots for *exploration* and *debugging*, beauty wasn't a high priority.