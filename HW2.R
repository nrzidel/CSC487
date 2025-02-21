#R program to calculate manhattan Distance, a and b are both vectors of the same length

len <- 2
a <- sample(1:100, size=len)
b <- sample(1:100, size=len)

sum(abs(a-b))



#R program to calculate euclidian distance

sqrt(sum(((a-b)^2)))


cor(mtcars$mpg, y = mtcars$wt)

library(ggplot2)

ggplot(data = mtcars, mapping = aes(x = wt, y = mpg)) + geom_point()



#Metabolite practice

#Read file
dat = read.csv("metabolite.csv")

#remove columns missing >75% of row data
dat_wout_missing = dat[, colMeans(is.na(dat)) < .75]

#replace NA values with column median
dat_cleaned <- lapply(dat_wout_missing, function(x) {
  if (is.numeric(x) | is.logical(x)) {
    x[is.na(x)] <- median(x, na.rm = TRUE)
  }
  return(x)
})

#converts the list back to a dataframe
dat_cleaned <- as.data.frame(dat_cleaned)


#Check to ensure no values remain as NA
sum(is.na(dat_cleaned))



#PCA 

pca_results <- prcomp(dat_cleaned[2:188], retx = TRUE, center = TRUE, scale = TRUE)


# Create a new dataframe for plotting
pca_df <- data.frame(
  Class = dat_cleaned[, 1],    # Class labels
  PC1 = pca_results$x[, 1],    # First Principal Component
  PC2 = pca_results$x[, 2]     # Second Principal Component
)

# Scree plot

pca_var <- pca_results$sdev^2
pca_var_per <- round(pca_var/sum(pca_var)*100, 1)
barplot(pca_var_per, main = "Scree plot", xlab = "Principle Component", ylab = "Percent Variation")

# Create the PCA scatter plot
ggplot(pca_df, aes(x = PC1, y = PC2, color = Class)) +
  geom_point(size = 3, alpha = 0.8) + 
  labs(title = "PCA Plot", 
       x = paste("PC1 ", pca_var_per[1], "%"), 
       y = paste("PC2 ", pca_var_per[2], "%"))
