##### Apply Principal Component Analysis on S&P 500 companies #####

##Computing the principal component ##
#First solution

R=cor(stock.close) # the correlation matrix
eigenR=eigen(R) # eigenvalues and eigenvectors
eigenR
factor <- cumsum(eigenR$values)/sum(eigenR$values) # Compute proportion of the total variance explained by the first eigenvalues
factor
eigenProp <- 1 - cumsum(eigenR$values)/sum(eigenR$values)
plot(sort(eigenProp, decreasing=TRUE), type='o')


#Second solution
fit <- princomp(subset1, cor=TRUE)
summary(fit)
loadings(fit)
plot(fit,type="lines")
biplot(fit)


#Third solution
library(psych)
fit <- principal(subset1, nfactors=5, rotate="varimax")
fit # print results
library(nFactors)
ev <- eigen(cor(subset1)) # get eigenvalues
ap <- parallel(subject=nrow(subset1),var=ncol(subset1),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)
##Computing the principal component using eigenvalue decomposition ##
princ.return <- princomp(subset1)
## I am plotting the standard deviation of the PC's divided by standard deviation of PC 1, 
##this can help us decide on a benchmark that we can use to select the relevant components.
barplot(height=princ.return$sdev[1:10]/princ.return$sdev[1])
load <- loadings(princ.return)[,1]
pr.cp <- dataNum %*% load
pr <- as.numeric(pr.cp)
pr
print(load)

