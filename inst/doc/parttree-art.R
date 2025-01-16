## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  out.width = "90%",
  # fig.width = 8,
  # dpi = 300,
  asp = 0.625,
  global.par = TRUE
)

## ----setup, message=FALSE-----------------------------------------------------
library(parttree) # This package
library(rpart)    # For decision trees
library(magick)   # For reading and manipulating images
library(imager)   # Another image library, with some additional features

op = par(mar = c(0,0,0,0)) # Remove plot margins

## ----thread_control, echo=-1--------------------------------------------------
data.table::setDTthreads(2)
magick:::magick_threads(2)

## ----peale-download-----------------------------------------------------------
# Download the image
rosalba = image_read("https://upload.wikimedia.org/wikipedia/commons/a/aa/Rembrandt_Peale_-_Portrait_of_Rosalba_Peale_-_Google_Art_Project.jpg")

# Crop around the eyes
rosalba = image_crop(rosalba, "850x400+890+1350")
# rosalba = image_crop(rosalba, "750x350+890+1350")

# Convert to cimg (better for in-memory manipulation)
rosalba = magick2cimg(rosalba)

# Display
rosalba
plot(rosalba, axes = FALSE)

## ----rosalba_df---------------------------------------------------------------
# Coerce to data frame
rosalba_df = as.data.frame(rosalba)

# Round color values to ease up work for decision tree
rosalba_df$value = round(rosalba_df$value, 4)

head(rosalba_df)

## ----rosalba_ccs--------------------------------------------------------------
rosalba_ccs = split(rosalba_df, rosalba_df$cc)

# We have a list of three DFs by colour channel. Uncomment if you want to see:
# str(rosalba_css)

## ----trees--------------------------------------------------------------------
## Start creating regression tree for each color channel. We'll adjust some
## control parameters to give us the "right" amount of resolution in the final
## plots.
trees = lapply(
  rosalba_ccs, 
  # function(d) rpart(value ~ x + y, data=d, control=list(cp=0.00001, maxdepth=20))
  function(d) rpart(value ~ x + y, data=d, control=list(cp=0.00002, maxdepth=20))
  )

## ----pred_trees---------------------------------------------------------------
pred = lapply(trees, predict) # get predictions for each tree

## ----pred_img-----------------------------------------------------------------
# The pred object is a list, so we convert it to a vector before overwriting the
# value column of the original data frame
rosalba_df$value = do.call("c", pred)

# Convert back into a cimg object, with the predictions providing pixel values
pred_img = as.cimg(rosalba_df)

## ----rosalba_abstract---------------------------------------------------------
# get a list of parttree data frames (one for each tree)
pts = lapply(trees, parttree)

## first plot the downscaled image...
plot(pred_img, axes = FALSE)
## ... then layer the partitions as a series of rectangles
lapply(
  pts, 
  function(pt) plot(
    pt, raw = FALSE, add = TRUE, expand = FALSE,
    fill_alpha = NULL, lwd = 0.1, border = "grey15"
  )
)

## ----rosalba_abstract_gg------------------------------------------------------
library(ggplot2)
ggplot() +
  annotation_raster(pred_img,  ymin=-Inf, ymax=Inf, xmin=-Inf, xmax=Inf) +
  lapply(trees, function(d) geom_parttree(data = d, lwd = 0.05, col = "grey15")) +
  scale_x_continuous(limits=c(0, max(rosalba_df$x)), expand=c(0,0)) +
  scale_y_reverse(limits=c(max(rosalba_df$y), 0), expand=c(0,0)) +
  coord_fixed(ratio = Reduce(x = dim(rosalba)[2:1], f = "/") * 2) +
  theme_void()

## ----bonzai-------------------------------------------------------------------
bonzai = load.image("https://upload.wikimedia.org/wikipedia/commons/thumb/0/0c/Japanese_Zelkova_bonsai_16%2C_30_April_2012.JPG/480px-Japanese_Zelkova_bonsai_16%2C_30_April_2012.JPG")
plot(bonzai, axes = FALSE)

# Coerce to data frame
bonzai_df = as.data.frame(bonzai)

# Round color values to ease up work for decision trees
bonzai_df$value = round(bonzai_df$value, 2)

# Split into colour channels
bonzai_ccs = split(bonzai_df, bonzai_df$cc)

# Fit decision trees on each colour channel DF. Note that I'm deliberating
# truncating the max tree depth to reduce the number of partitions.
bonzai_trees = lapply(
  bonzai_ccs, 
  function(d) rpart(value ~ x + y, data=d, control=list(cp=0.00001, maxdepth=10))
)

# Overwrite value column with predictions vector
bonzai_df$value = do.call("c", lapply(bonzai_trees, predict))

# Convert back into a cimg object, with the predictions providing pixel values
bonzai_pred_img = as.cimg(bonzai_df)

## ----bonzai_mean_cols---------------------------------------------------------
mean_cols = function(dat) {
  mcols = tapply(dat$value, dat$cc, FUN = "mean")
  col1 = rgb(mcols[1], mcols[2], mcols[3])
  hicol = function(x) mean(x) + 0.4*sd(subset(dat, cc==1)$value)
  col2 = rgb(hicol(subset(dat, cc==1)$value), hicol(subset(dat, cc==2)$value), hicol(subset(dat, cc==3)$value))
  locol = function(x) mean(x) - 0.4*sd(subset(dat, cc==1)$value)
  col3 = rgb(locol(subset(dat, cc==1)$value), locol(subset(dat, cc==2)$value), locol(subset(dat, cc==3)$value))
  return(c(col1, col2, col3))
}

bonzai_mean_cols = mean_cols(bonzai_df)

## ----bonza_pts----------------------------------------------------------------
bonzai_pts = lapply(bonzai_trees, parttree)

## ----bonza_abstract-----------------------------------------------------------
plot(bonzai_pred_img, axes = FALSE)
Map(
  function(pt, cols) {
    plot(
      pt, raw = FALSE, add = TRUE, expand = FALSE,
      fill_alpha = 0, lwd = 0.2, border = cols
    )
  },
  pt = bonzai_pts,
  cols = bonzai_mean_cols
)

## -----------------------------------------------------------------------------
# library(ggplot2)
ggplot() +
  annotation_raster(bonzai_pred_img,  ymin=-Inf, ymax=Inf, xmin=-Inf, xmax=Inf) +
  Map(function(d, cols) geom_parttree(data = d, lwd = 0.1, col = cols), d = bonzai_trees, cols = bonzai_mean_cols) +
  scale_x_continuous(limits=c(0, max(bonzai_df$x)), expand=c(0,0)) +
  scale_y_reverse(limits=c(max(bonzai_df$y), 0), expand=c(0,0)) +
  coord_fixed() +
  theme_void()

## ----eval=FALSE, include=FALSE------------------------------------------------
# library(patchwork)
# 
# ## Aside: We're reversing the y-scale since higher values actually correspond
# ## to points lower on the image, visually.
# g =
#   Map(
#     function(tree, pal) {
#       ggplot() +
#         geom_parttree(data = tree, aes(fill=-value), alpha = 0.5, lwd = 0.1) +
#         scale_fill_distiller(palette = pal) +
#         scale_y_reverse() +
#         coord_fixed() +
#         theme_void() +
#         theme(legend.position = 'none') +
#         labs(title = substr(pal, 1, 1))
#     },
#     tree = bonzai_trees,
#     pal = c("Reds", "Greens", "Blues")
#   )
# 
# g[[1]] + g[[2]] + g[[3]]

## ----bonzai_glass-------------------------------------------------------------
lapply(
  seq_along(bonzai_pts),
  function(i) {
    plot(
      bonzai_pts[[i]], raw = FALSE, expand = FALSE,
      axes = FALSE, legend = FALSE,
      main = paste0(c("R", "G", "B")[i]),
      ## Aside: We're reversing the y-scale since higher values actually
      ## correspond to points lower on the image, visually.
      ylim = rev(attr(bonzai_pts[[i]], "parttree")[["yrange"]])
    ) 
  }
)

## ----reset_par----------------------------------------------------------------
# reset the plot margins
par(op)

