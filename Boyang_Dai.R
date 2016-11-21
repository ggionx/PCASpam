library(kernlab)
data(spam)
data <- spam
#
#
#  # - - - - - - - - - - - #
#  #   Correlation Matrix  #
#  # - - - - - - - - - - - #
#
#
pca <- scale(data[, -58], scale = TRUE, center = TRUE)
cor(pca)
cor_spam <- round(cor(pca), 2)
upper_tri_spam <- get_upper_tri(cor_spam)
melted_spam <- melt(upper_tri_spam, na.rm = TRUE)
ggplot(data = melted_spam, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal() + coord_fixed() + 
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(), 
    axis.text = element_text(size = 10, angle=45, hjust=1))
#
#
# --- Extract the name of the variable with high correlation value --- #
cor_spam <- round(cor(pca), 2)
cor_spam[upper.tri(cor_spam)] <- 0
diag(cor_spam) <- 0
data.new <- pca[, !apply(cor_spam, 2, function(x) {any(abs(x) > 0.70)})]
df <- as.data.frame(data.new)
colnames(df)
'%!in%' <- function(x,y)!('%in%'(x,y))
which(colnames(pca) %!in% colnames(df))
#
#
#
#
#
#  # - - - - - - - - - - - #
#  #       Simple PCA      #
#  # - - - - - - - - - - - #
# 
#
# Next let’s say I know nothing about dimensionality reduction techniques and 
# just naively apply principle components to the data in R:
pc <- princomp(data[, -58])
plot(pc)
#
#This is because the scale of the different variables in the data set is quite variable; 
# we can see this by plotting the variance of the different columns in the data frame 
# (regular scaling on the left, logarithmic on the right):
#
# verify by plotting variance of columns
mar <- par()$mar
par(mar=mar+c(0,5,0,0))
barplot(sapply(data[, -58], var), horiz=T, las=1, cex.names=0.8)
barplot(sapply(data[, -58], var), horiz=T, las=1, cex.names=0.8, log='x')
par(mar=mar)
#
#
# We correct for this by scaling the data using the scale() function. We can then verify 
# that the variances across the different variables are equal so that when we apply principal 
# components one variable does not dominate.
#
#
# Scale
data2 <- data.frame(scale(data[, -58]))
# Verify variance is uniform
plot(sapply(data2, var))
#
#
#
# Now we can apply principal components to the scaled data. Note that this can also be done 
# automatically in call to the prcomp() function by setting the parameter scale=TRUE. 
# Now we see a result which is more along the lines of something we would expect:
#
#
# Proceed with principal components
pc <- princomp(data2)
plot(pc)
plot(pc, type='l')
summary(pc) # 4 components is both 'elbow' and explains >85% variance
#
#
# There are various rules of thumb for selecting the number of principal components to retain 
# in an analysis of this type, two of which I’ve read about are:
#
#    * Pick the number of components which explain 85% or greater of the variation
#    * Use the ‘elbow’ method of the scree plot (on right)
# Here we are fortunate in that these two are the same, so we will retain the first four 
# principal components. We put these into new data frame and plot.
#
#
# Get principal component vectors using prcomp instead of princomp
pc <- prcomp(data2)
#
# First for principal components
comp <- data.frame(pc$x[,1:4])
# Plot
plot(comp, pch=16, col=rgb(0,0,0,0.5))
#
#
So what were are looking at here are twelve 2-D projections of data which are in a 4-D space. 
# You can see there’s a clear outlier in all the dimensions, as well as some bunching together 
# in the different projections.
#
#
library(rgl)
# Multi 3D plot
plot3d(comp$PC1, comp$PC2, comp$PC3)
plot3d(comp$PC1, comp$PC3, comp$PC4)
#
#





    
    
