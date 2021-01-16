
nbi <- read.table( file = "clipboard", sep = "\t", header = TRUE)

str(nbi)

Next, we convert the integer and factor to "numeric". PCA and cluster analysis are performed when variables are in numeric.


nbi.numeric = as.data.frame(sapply(nbi,as.numeric))

str(nbi.numeric)

Since we have 52 observations with 24 variables, we proceed to finding the PCA. The PCA show the distribution of the variance across the variables.

nbi.pca<- prcomp((nbi.numeric),scale = TRUE)

str(nbi.pca)

summary(nbi.pca)

Since we have known our PCA, we can proceed to plotting the graph, the variance, cummulative variance and eigen value decrease.

fviz_eig(nbi.pca)

From above, it shows that the PC1 up to PC10 contributed significantly with the following variances; 18.46%,9.4%....... However, the importance of the eigen value decreases as we move along the 10 PCA. 

pcaCharts <- function(x) {
    x.var <- x$sdev ^ 2
    x.pvar <- x.var/sum(x.var)
    print("proportions of variance:")
    print(x.pvar)
    
    par(mfrow=c(2,2))
    plot(x.pvar,xlab="Principal component", ylab="Proportion of variance explained", ylim=c(0,1), type='b')
    plot(cumsum(x.pvar),xlab="Principal component", ylab="Cumulative Proportion of variance explained", ylim=c(0,1), type='b')
    screeplot(x)
    screeplot(x,type="l")
    par(mfrow=c(1,1))
}

pcaCharts(nbi.pca)

pca.out <- nbi.pca
pca.out$rotation <- -pca.out$rotation
pca.out$x <- -pca.out$x
biplot(pca.out,scale=0, cex=.7)

library(ggbiplot)

nbi_biplot <- ggbiplot(pca.out, obs.scale = 1, var.scale = 1, labels=row.names(nbi),
              ellipse = TRUE, 
              circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')

nbi_biplot <- ggbiplot(pca.out, obs.scale = 1, var.scale = 1, labels=row.names(nbi),
              ellipse = TRUE, 
              circle = TRUE)
nbi_biplot <- nbi_biplot + scale_color_discrete(name = '')
nbi_biplot <- nbi_biplot + theme(legend.direction = 'horizontal', 
               legend.position = 'top')

print(nbi_biplot)


