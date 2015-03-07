# pithr

Quick graphical data summaries!

### What is pithr?

**pithr** is an R package built to generate quick graphical data summaries.  That is, to generate the graphical equivalent of the **summary()** function.  These summaries are called *piths*.

### Piths?

*Pith*, when used as a noun, can mean *the essence*.  *Pith*, when used as a verb, can mean *to remove the pith*.  Since pithr is built to provide descriptive summary plots of data, the vocabulary seemed appropriate.  And *pith* is just four letters long, and I don't like unnecessary typing.

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

### **NA** Values

One of the important features of **pith()** is that it handles missing values, when they are present.  The proportions or densities are scaled to take into account the proportion of **NA** values in the data.

```
X <- rnorm(80)
pith(X, freq = FALSE)
XNA <- c(X, rep(NA, 20))
pith(XNA, freq = FALSE)
```

### **dplyr** functionality

**pithr** has a special function **pselect()** that first runs **dplyr::select()**, then **pith()**, then *returns the data.frame*.  Because the data.frame (or tbl or tbl_df) is returned, the chain of operations can continue.

```
library(dplyr)
iris %>% tbl_df %>% pselect(Sepal.Length, Sepal.Width, .pith = list(freq = FALSE, col = "red"))
```
Which is similar to:
```
library(magrittr)
iris %>% tbl_df %>% select(Sepal.Length, Sepal.Width) %T>% pith(freq = FALSE, col = "red")
```

Note, I haven't figured out how to make **pith()** pay attention to any grouping variables in a tbl_df, yet.