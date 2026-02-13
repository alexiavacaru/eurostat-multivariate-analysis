# Load the required packages for the analysis
install.packages("here")
install.packages("openxlsx")
install.packages("corrplot")
install.packages("ggplot2")
install.packages("factoextra")
install.packages("FactoMineR")
install.packages("psych")
install.packages("nFactors")
install.packages("ca")
install.packages("cluster")

library(here)
library(openxlsx)
library(corrplot)
library(ggplot2)
library(factoextra)
library(FactoMineR)
library(psych)
library(nFactors)
library(ca)
library(cluster)

# 1. Data Ingestion: Importing data from multiple formats (CSV, TXT, XLSX)
# Handling Eurostat-specific null strings like ":" or empty spaces
date1 <- read.csv(here("Tema1_Eurostat.csv"), header=T, sep="'", na.strings=c("NA",":",""))
date2 <- read.table(here("Tema1_Eurostat.txt"), sep="\t", header=T, dec=",", na.strings=c("NA",":",""))
date3 <- read.xlsx(here("Tema1_Eurostat.xlsx"), colNames = TRUE, na.strings = c("NA", ":", ""))

# 2. Data Cleaning: Setting row identifiers and removing non-numeric columns
# We use the 'Cod' column for indexing and omit rows with missing values to ensure quality
row.names(date3) <- date3$Cod
date1 <- date1[, -1] 
date1 <- na.omit(date1)

# 3. Exploratory Data Analysis: Correlation analysis and visualization
# Generating the correlation matrix and plotting a heatmap to find relationships
R <- cor(date1)
round(R, 3)
corrplot(R, method="number", type="upper")

# 4. Standardization: Centering and scaling variables for comparability
# This ensures that variables with different units do not bias the results
Xs <- scale(date1)

# 5. Visualizing distributions and outliers using Boxplots
# Comparing the original data scale with the standardized version
boxplot(Xs, col="lightblue", border="blue", main="Standardized Distributions")
boxplot(date1, col="pink", border="magenta", main="Original Distributions")

# 6. Advanced Visualization: Creating multivariate scatter plots with ggplot2
# Representing four dimensions (x1, x2, x3, x4) in a single chart
ggplot(data=date3, aes(x=x1, y=x2)) +
  geom_point(aes(size=x3, col=x4), alpha=0.5) +
  geom_text(label=row.names(date3), vjust=0, hjust=0, col="blue")

# 7. Matrix Algebra: Calculating the Cross-Product and Covariance matrices
# Validating the mathematical relationship between the product matrix and covariance
PI <- t(Xs) %*% Xs
cov_matrix <- cov(Xs)
n <- dim(date1)[1]
diff_check <- round(PI/(n-1) - cov_matrix, 2)

# 8. Vector Geometry: Calculating standard deviations and vector cosines
# Identifying the variables with maximum variance and measuring their angular alignment
vectorSD <- apply(date1, 2, sd)
coloana1 <- date1[, which.max(vectorSD)]
coloana2 <- date1[, which.min(vectorSD)]
coloana1C <- scale(coloana1, center=T, scale=F)
coloana2C <- scale(coloana2, center=T, scale=F)
cos_alpha <- (t(coloana1C) %*% coloana2C) / (norm(coloana1C, type="2") * norm(coloana2C, type="2"))

# 9. Distance Metrics: Calculating the Euclidean distance between two regions
# Measuring the statistical similarity between region 20 and region 39
dist(rbind(date1[20,], date1[39,]), method="euclidian")

# 10. Eigenvalues & Orthogonality: Spectral decomposition of the correlation matrix
# Verifying that the eigenvector matrix is orthogonal and can reconstruct the data
desc <- eigen(R)
valori <- desc$values
vectori <- desc$vectors
I_check <- round(t(vectori) %*% vectori, 2)
C_reconstructed <- vectori %*% diag(valori) %*% t(vectori)

# 11. Principal Component Analysis (PCA): Dimensionality reduction
# Using the 'princomp' method to synthesize information into fewer components
acp <- princomp(Xs, cor=T, scores=T)
summary(acp)
plot(acp, type="l") # Scree plot

# 12. Manual Component Interpretation: Renaming variables for clarity
# Assigning descriptive names to the data columns for business intelligence
names(date3) <- c("Cod", "Education_Level", "NEET_Youth", "Dropout_Rate", 
                  "Pop_Structure", "Female_Edu", "Agri_Land", "Youth_Emp", 
                  "LT_Unemployment", "Sci_Tech", "Higher_Edu")

# 13. Categorical Transformation: Creating manual bins for analysis
# Using quantiles to classify variables into 'Low', 'Medium', and 'High' categories
pragx2 <- quantile(date1$x2, probs=c(0.33, 0.66, 0.99))
x2c <- rep(0, nrow(date1))
x2c[date1$x2 < pragx2[1]] <- "x2.low"
x2c[date1$x2 >= pragx2[1] & date1$x2 < pragx2[2]] <- "x2.medium"
x2c[date1$x2 >= pragx2[2]] <- "x2.high"

pragx5 <- quantile(date1$x5, probs=c(0.33, 0.66, 0.99))
x5c <- rep(0, nrow(date1))
x5c[date1$x5 < pragx5[1]] <- "x5.low"
x5c[date1$x5 >= pragx5[1] & date1$x5 < pragx5[2]] <- "x5.medium"
x5c[date1$x5 >= pragx5[2]] <- "x5.high"

# Converting to factor type for statistical processing
x2f <- factor(x2c)
x5f <- factor(x5c)

# 14. Correspondence Analysis: Studying the relationship between categorical variables
# Visualizing the association between the bins created in the previous step
tab_conting <- table(x2f, x5f)
ac_model <- ca(tab_conting)
plot(ac_model, main="Correspondence Analysis Map: X2 vs X5")

# 15. Partial Correlation: Isolating influence between variables
# Calculating the correlation between x3 and x5 while controlling for x4 influence
m1 <- lm(x3 ~ x4, data=date1); r1 <- m1$residuals
m2 <- lm(x5 ~ x4, data=date1); r2 <- m2$residuals
partial_cor_manual <- cor(r1, r2)

# Alternative method: Using matrix inversion to find partial correlations
P_inv <- solve(R)
cpX3_X4 <- -1 * P_inv[3,4] / sqrt(P_inv[3,3] * P_inv[4,4])

# 16. Data Suitability Tests: KMO and Bartlett's Test
# Verifying if the dataset is suitable for Factor Analysis
kmo_test <- KMO(R)
bartlett_test <- cortest.bartlett(R, n=40)

# 17. Exploratory Factor Analysis (EFA): Identifying latent factors
# Using Maximum Likelihood and Parallel Analysis to determine factor structure
fa.parallel(scale(date1))
model_fa <- fa(R, nfactors=3, n.obs=40, rotate="none", fm="ml")
fa.diagram(model_fa)

# 18. Cluster Analysis: Grouping regions based on PCA scores
# Implementing Hierarchical Clustering using Single, Centroid, and Ward's methods
scoruri <- acp$scores[, 1:2]
d_matrix <- dist(scoruri)
ierarhieS <- hclust(d_matrix, method="single")
ierarhieC <- hclust(d_matrix, method="centroid")
ierarhieW <- hclust(d_matrix, method="ward.D2")

# 19. Visualizing and validating the Clustering solution
# Cutting the dendrogram into 3 groups and validating with a Silhouette plot
plot(ierarhieW, main="Ward's Hierarchical Clustering")
rect.hclust(ierarhieW, k=3, border=2:4)
solutie3 <- cutree(ierarhieW, k=3)
sil_val <- silhouette(solutie3, d_matrix)
plot(sil_val)

# 20. Final Results: Profiling the clusters and visualizing the map
# Calculating the mean for each cluster to understand the geographic characteristics
fviz_cluster(list(data=scoruri, cluster=solutie3))
aggregate(date1, list(Cluster=solutie3), mean)
