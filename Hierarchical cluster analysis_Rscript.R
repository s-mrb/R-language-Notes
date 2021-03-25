#______________________________
# Hierarchical Cluster Analysis
#______________________________

# Hierarchical cluster analysis on a set of dissimilarities and methods for analyzing it.

#----------------------
# Import data file in R
#----------------------

# Import csv (comma (,) separated values file) format

data <- read.csv(file.choose())

# Import csv (semi collon (;) separated values file) format

data <- read.csv2(file.choose())

# Import text (tab-delimited text file) format

data <- read.delim(file.choose())

#-------------
# Observe data
#-------------

# Return the first or last part of an object

head(data)
tail(data)

# View the structure of variables

str(data)

#--------------------
# Formating variables
#--------------------

#------
# Change variable to character type
#------
# Create or test for objects of type "character"
# is.character = return TRUE or FALSE depending on whether its arguments is of character type or not.

data$X <- as.character(x = data$X)
str(data)
head(data)

#------
# Change column variable as rownames for first variable
#------
# rownames = Retrieve or set the row or column names of a matrix-like object.
# c = Combine values into a vector or list

rownames(data) <- c(data$X)
head(data)

#------
# Delete first column
#------
# [ = Extract or replace parts of a data frame.
# -1 = delete first column conatainig rownames

newdata <- data[ ,-1]
head(newdata)

# Assign short name to data set

df <- newdata

#------
# replace any missing values
#------
# na.omit() = returns the object with incomplete cases removed.na.fail returns the object if it does not contain any missing values, and signals an error otherwise.na.pass returns the object unchanged.

df <- na.omit(df)

#------
# scaling data to standardize it
#------
# Scale = scale is generic function whose default method centers and/or scales the columns of a numeric matrix. scale data so that clustering do not depend on arbitrary variable value

df.scaled <- scale(df)
head(df.scaled)

#----------------------------
# Distance matrix computation 
#----------------------------

# dist() = This function computes and returns the distance matrix computed by using the specified distance measure to compute the distances between the rows of a data matrix.
# x = a numeric matrix, data frame or "dist" object.
# method = ("euclidean", "maximum", "manhattan", "canberra", 
#           "binary", "minkowski", "pearson", "spearman" or "kendall" )

require(stats)
res.dist <- dist(x = df.scaled, 
                 method = "euclidean")

# View the euclidean distance measurements

x <- as.matrix(res.dist)[1:6, 1:6]
x

# To round values in distance matrix use as.matrix function. Round first 3 column and 3 rows to three decimal place

round(x, digits = 3)

#------------------------
# Hierarchical clustering
#------------------------

# d = dissimilarity structure produced by dist() function to be used 
# method = agglomeration method to be used 
#          ("ward.D", "ward.D2", "single", "complete", "average"(= UPGMA), 
#          "msquitty"(= WPGMA), "median"(= WPGMC), "centroid"(= UPGMC))

require(stats)
res.hc <- hclust(d = res.dist,
                 method = "complete")
plot(x = res.hc)


#------
# Simple cluster dendrogram (default will result in rectangular type)
#------

# fviz_dend = enhanced visualization of dendrogram
# x = an object of class dendrogram, hclust, agnes, diana, hcut, hkmeans or HCPC (FactoMineR).
# cex = change the label size
# lwd = a numeric value specifying branches and rectangle line width (default is 0.7)

require(factoextra)
fviz_dend(x = res.hc, cex = 0.7, lwd = 0.7)

#------
# Assign colors to clusters and rectangels in dendrogram
#------

# colors() function return the buit-in color names

require(grDevices)

colors()

# Or use palette function to get 8 colors names ("black"   "red"     "green3"  "blue"    "cyan"    "magenta" "yellow"  "gray")
# show_col = A quick way to show colors in a plot
# palette =  set or view the graphics palette

require(scales)
palette()
show_col(palette(rainbow(6)))

# pal_jco = Color palettes inspired by plots in journal of clinical oncology
# palette = There is only one available option "default" (10 color palettes)
# alpha = Transparency level, a real number in (0, 1)
# ggsci = Scientific journal and sci-fi themed color palettes for ggplot2

require("ggsci")
show_col(pal_jco(palette = c("default"))(10))
show_col(pal_jco("default", alpha = 0.6)(10))

#------
# Using colors in dendrogram
#------

# fviz_dend = Use fviz function for enhanced visualization of dendrogram
# x = an object of class dendrogram, hclust, agnes, diana, hcut, hkmeans or HCPC (FactoMineR).
# k = the number of groups for cutting the tree.
# cex = size of labels
# k_colors = a vector containing colors to be used for the groups.It should contains k number of colors. Allowed values include also "grey" for grey color palettes; brewer palettes e.g. "RdBu", "Blues", ...; and scientific journal palettes from ggsci R package, e.g.: "npg", "aaas", "lancet", "jco", "ucscgb", "uchicago", "simpsons" and "rickandmorty"
fviz_dend(x = res.hc, cex = 0.8, lwd = 0.8, k = 4, 
          k_colors = c("red", "green3", "blue","magenta"))

# Or use "jco" colors
# jco = Color palettes inspired by plots in journal of clinical oncology

fviz_dend(x = res.hc, cex = 0.8, lwd = 0.8, k = 4, 
          k_colors = "jco")

#------
# Draw rectangles around the k clusters
#------

# rect = logical value specifying whether to add a rectangle around groups. Used only when k != NULL.
# rect_border = border color and line type for rectangles.
# rect_fill = a logical value. If TRUE, fill the rectangle
# horiz = a logical value. If TRUE, an horizontal dendrogram is drawn.

fviz_dend(res.hc, cex = 0.8, lwd = 0.8, k = 4, 
          rect = TRUE, 
          rect_border = "gray",
          rect_fill = FALSE)

fviz_dend(res.hc, cex = 0.8, lwd = 0.8, k = 4, 
          rect = TRUE, 
          rect_border = "gray",
          rect_fill = TRUE)

#------
# Fill Rectangles with k colors
#------

# provide same list of colors used for clusters in rect_border argument to fill rectangles with same colors

fviz_dend(res.hc, cex = 0.8, lwd = 0.8, k = 4, 
          rect = TRUE, 
          rect_border = "jco", 
          rect_fill = TRUE)

# Using same k cluster and rectangle fill colors
# Here I selected few color codes from "jco" color palettes

fviz_dend(res.hc, cex = 0.8, lwd = 0.8, k = 4, 
          rect = TRUE, 
          k_colors = "jco", 
          rect_border = "jco", 
          rect_fill = TRUE)

# Or you can also use color palettes codes from the list provided by the journal of clinical oncology

fviz_dend(res.hc, cex = 0.8, lwd = 0.8, k = 4, 
          rect = TRUE, 
          k_colors = c("#EFC000FF", "#868686FF", "#CD534CFF", "#003C67FF"), 
          rect_border = c("#EFC000FF", "#868686FF", "#CD534CFF", "#003C67FF"), 
          rect_fill = TRUE)

#------
# Align dendrogram horizontally
#------

fviz_dend(res.hc, cex = 0.8, k=4, 
          rect = TRUE,  
          k_colors = "jco",
          rect_border = "jco", 
          rect_fill = TRUE, 
          horiz = TRUE)

#------
# Using ggtheme of ggplot2
#------
# ggtheme = Default value is theme_classic(). Allowed values include ggplot2 official themes: theme_gray(), theme_bw(), theme_minimal(), theme_classic(), theme_void()
# allowed themes (gray, bw, minimal, void)

fviz_dend(res.hc, cex = 0.8, lwd = 0.8, k = 4, 
          rect = TRUE, 
          k_colors = "jco", 
          rect_border = "jco", 
          rect_fill = TRUE,
          ggtheme = theme_void())

#------
# Change type of dendrogram
#------

# type = c("rectangle", "circular", "phylogenic")

fviz_dend(res.hc, cex = 0.8, lwd = 0.8, k = 4, 
          rect = TRUE, 
          k_colors = "jco", 
          rect_border = "jco", 
          rect_fill = TRUE,
          type = "phylogenic")

# repel = logical value. Use repel = TRUE to avoid label overplotting when type = "phylogenic".

fviz_dend(res.hc, cex = 0.8, lwd = 0.8, k = 4, 
          rect = TRUE, 
          k_colors = "jco", 
          rect_border = "jco", 
          rect_fill = TRUE,
          type = "phylogenic",
          repel = TRUE)

#-------
# Layouts for phylogenic dendrogram
#-------

# You can use phylo layout argument to draw different types of phylogenic trees 
# ("layout.gem", "layout.mds","layout_with_drl","layout_with_lgl", "layout_as_tree")

require(igraph)
fviz_dend(res.hc, cex = 0.8, lwd = 0.8, k = 4, 
          rect = TRUE, 
          k_colors = "jco", 
          rect_border = "jco", 
          rect_fill = TRUE,
          type = "phylogenic",
          repel = TRUE,
          phylo_layout = "layout_as_tree")
