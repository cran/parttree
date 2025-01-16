## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE,
  out.width = "90%",
  # fig.width = 8,
  # dpi = 300,
  asp = 0.625
)

## ----setup--------------------------------------------------------------------
library(parttree)  # This package
library(rpart)     # For fitting decisions trees

# install.packages("palmerpenguins")
data("penguins", package = "palmerpenguins")
head(penguins)

## ----thread_control, echo=-1--------------------------------------------------
data.table::setDTthreads(2)

## ----tree---------------------------------------------------------------------
tree = rpart(species ~ flipper_length_mm + bill_length_mm, data = penguins)
tree

## ----rpart_plot---------------------------------------------------------------
plot(tree, compress = TRUE)
text(tree, use.n = TRUE)

## ----penguin_cl_plot----------------------------------------------------------
ptree = parttree(tree)
plot(ptree)

## ----ptree--------------------------------------------------------------------
# See also `attr(ptree, "parttree")`
ptree

## ----penguin_cl_plot_custom---------------------------------------------------
plot(ptree, pch = 16, palette = "classic", alpha = 0.75, grid = TRUE)

## ----penguin_reg_plot---------------------------------------------------------
tree_cont = rpart(body_mass_g ~ flipper_length_mm + bill_length_mm, data = penguins)

tree_cont |>
  parttree() |>
  plot(pch = 16, palette = "viridis")

## ----penguin_ctree_plot-------------------------------------------------------
library(partykit)

ctree(species ~ flipper_length_mm + bill_length_mm, data = penguins) |>
  parttree() |>
  plot(pch = 16, palette = "classic", alpha = 0.5)

## ----titanic_plot-------------------------------------------------------------
set.seed(123) ## For consistent jitter

library(parsnip)
library(titanic) ## Just for a different data set

titanic_train$Survived = as.factor(titanic_train$Survived)

## Build our tree using parsnip (but with rpart as the model engine)
ti_tree =
  decision_tree() |>
  set_engine("rpart") |>
  set_mode("classification") |>
  fit(Survived ~ Pclass + Age, data = titanic_train)
## Now pass to parttree and plot
ti_tree |>
  parttree() |>
  plot(pch = 16, jitter = TRUE, palette = "dark", alpha = 0.7)

## ----penguin_cl_ggplot2-------------------------------------------------------
library(ggplot2)
theme_set(theme_linedraw())

## re-using the tree model object from above...
ggplot(data = penguins, aes(x = flipper_length_mm, y = bill_length_mm)) +
  geom_point(aes(col = species)) +
  geom_parttree(data = tree, aes(fill=species), alpha = 0.1)

## ----penguin_reg_ggplot2------------------------------------------------------
## re-using the tree_cont model object from above...
ggplot(data = penguins, aes(x = flipper_length_mm, y = bill_length_mm)) +
  geom_parttree(data = tree_cont, aes(fill=body_mass_g), alpha = 0.3) +
  geom_point(aes(col = body_mass_g)) + 
  scale_colour_viridis_c(aesthetics = c('colour', 'fill')) # NB: Set colour + fill together

## ----ptree_redux--------------------------------------------------------------
# ptree = parttree(tree)
ptree

## ----penguin_cl_ggplot_mismatch-----------------------------------------------
## First, redo our first plot but this time switch the x and y variables
p3 = ggplot(
  data = penguins, 
  aes(x = bill_length_mm, y = flipper_length_mm) ## Switched!
  ) +
  geom_point(aes(col = species))

## Add on our tree (and some preemptive titling..)
p3 +
  geom_parttree(data = tree, aes(fill = species), alpha = 0.1) +
  labs(
    title = "Oops!",
    subtitle = "Looks like a mismatch between our x and y axes..."
  )

## ----penguin_cl_ggplot_mismatch_flip------------------------------------------
p3 +
  geom_parttree(
    data = tree, aes(fill = species), alpha = 0.1,
    flip = TRUE  ## Flip the orientation
  ) +
  labs(title = "That's better")

