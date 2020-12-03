library(eat)

# ============
# CODE STYLING
# ============

library(styler)
style_pkg()

# ============
# CODE UPDATING
# ============

library(devtools)
library(testthat)

# Step 1.
# Edit one or more files below R/.

# Step 2.
devtools::document()

# Step 3.
devtools::load_all()

# Step 4.
# Run some examples interactively.

load("C:/Users/victo/Desktop/UNIVERSIDAD/LIBRERIA/EAT/eat/data/PISAindex.rda")

x <- 7:19
y <- 4:6

tree <- EAT(PISAindex, x = x, y = y)

#################################################

EAT_plot(tree, PISAindex, x = x, y = y)

M_Breiman(PISAindex, tree, x, y)

x <- 18
y <- 5

tree <- EAT(PISAindex, x = x, y = y)

frontier(tree, PISAindex, x, y)

# Step 5.
devtools::test() # (or test_file())

# Step 6.
devtools::check()

# ==========================
# ADD PACKAGE TO DESCRIPTION
# ==========================

usethis::use_package("dplyr")
usethis::use_package("dplyr", "Suggests")

# ==========
# VERSIONING
# ==========

# Cambios_mayores.cambios_menores.parche

# ===================
# ADD DATA TO PACKAGE
# ===================

library(readxl)
PISAindex <- read_excel("C:/Users/victo/Desktop/UNIVERSIDAD/LIBRERIA/BORRAR/datooooooooos/PISAindex.xlsx")

PISAindex <- PISAindex[-1, ]

PISAindex <- as.data.frame(PISAindex)

rownames(PISAindex) <- PISAindex$Code

PISAindex <- PISAindex[, -1]

usethis::use_data(PISAindex, overwrite = T)

# ===================
# GENERATE VIGNETTES
# ===================

devtools::build_vignettes()

# ====
# CODE
# ====

library(corrplot)

corrplot(cor(PISAindex[-33, 3:18]), method="number",
         type="upper", order="hclust", addCoef.col = "black",
         tl.col="black", tl.srt=45,
         diag=FALSE
)

```{r descriptive, echo = F, fig.width = 8, fig.height = 8, fig.align = 'center', eval = F}

library(ggplot2)

g1 <- ggplot(PISAindex, aes(x = S_PISA)) +
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white")+
  geom_density(alpha = .2, fill = "#2DEE88") +
  xlab("Science PISA")

g2 <- ggplot(PISAindex, aes(x = R_PISA)) +
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white") +
  geom_density(alpha = .2, fill = "#2DEE88") +
  xlab("Reading PISA")

g3 <- ggplot(PISAindex, aes(x = M_PISA)) +
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white") +
  geom_density(alpha = .2, fill = "#2DEE88") +
  xlab("Mathematics PISA")

cowplot::plot_grid(g1, g2, g3, nrow = 2)

```

library(eat)
library(readxl)
data <- read_excel("C:/Users/victo/Desktop/UNIVERSIDAD/LIBRERIA/test.xlsx")
x <- 1:2
y <- 3:4

tree <- EAT(data, x, y)

m <- efficiency_scores(data, tree, x, y, "EAT_BCC_output")

efficiency_barplot(m, data, tree, x, y)
efficiency_density(m, data, tree, x, y)

m <- EAT(PISAindex, x, y)


library(caret)

m <- knn3(x = data[, 1:2],
     y = factor("A"))
