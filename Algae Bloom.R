## Predciting  Algae Blooms 

# Loading the dataset into R

#install.packages("DMwR")

library(DMwR)

# Showing top 5 values of data set
head(algae)
algaeDataset<-algae
#From the below summary we can observe that during the winter and autum season maximum water samples has been taken, also 
#from the chemical parameters summary we undertand that we have some NA's value

summary(algaeDataset)

# Removing NA from the dataset

algaeDataset <- na.omit(algaeDataset)

# Vizualizing algae blooms
# From this histogram we come to know that during winter season maximum PH is been recorded
library(ggplot2)
ggplot(algaeDataset,aes(mxPH,fill = season)) + geom_histogram(colour = "black")

#As we can see from the boxplot, we have a lot of outliers thus concluding that these values
#must have a lot of effect on the orthophoshate chemical count of the river

boxplot(algaeDataset$oPO4, ylab="Orthophosphate Count")

#As we can see from the below graph that maximum times when the size of the river is
#medium we have taken the samples to identify the algae blossoms
ggplot(algaeDataset,aes(x=size)) + geom_bar(colour = "Red")

#Here we are doing a bivariate boxplot which is helping us to understand about algae 1 that in what kind
#of the size of the river does it blossom more, so here we say that 
#when the river size is small algae 1 blossoms a lot and we also see lot of outliers river size is medium 

library(lattice)
bwplot(size~a1,data=algaeDataset,xlab = "Algae A1",ylab = "River size")
bwplot(season~a1,data=algaeDataset,xlab = "Algae A1",ylab = "Season")
bwplot(speed~a1,data=algaeDataset,xlab = "Algae A1",ylab = "River speed")


## Multivariate Normal distribution check:

#Here we have calculated covriance and column Means for the normal distribution check

colnames(algaeDataset)
x <- dist(scale(algaeDataset[, c("mxPH", "mnO2", "Cl","NO3","NH4","oPO4","PO4","Chla")],
                center = FALSE))
as.dist(round(as.matrix(x), 2)[1:12, 1:12])
x <- algaeDataset[, c("mxPH", "mnO2", "Cl","NO3","NH4","oPO4","PO4","Chla")]
cm <- colMeans(x)
S <- cov(x)
d <- apply(x, MARGIN = 1, function(x)t(x - cm) %*% solve(S) %*% (x - cm))
d
S
cm
##from the column means we can say that mxPH,mnO2,NO3 and Chla are related because the column means are close enough

##Now let us draw the normal Q-Q plot for all the chemicals observed in the river

#For PH level we are having a symmetric distribution with flat tails
#MNo2 - negatively skewed
#Cl - positively skewed
#No3 - positively skewed
#NH4 - symmetric with flat tails on the right
#OPo4 - positive skewed
#Po4 - symmetric with flat tails
#CHla - negatively skewed with lot of outliers

{qqnorm(algaeDataset[,"mxPH"], main = "PH level") 
  qqline(algaeDataset[,"mxPH"])}

{qqnorm(algaeDataset[,"mnO2"], main = "MnO2 level") 
  qqline(algaeDataset[,"mnO2"])}

{qqnorm(algaeDataset[,"Cl"], main = "Cl level") 
  qqline(algaeDataset[,"Cl"])}


{qqnorm(algaeDataset[,"NO3"], main = "NO3 level") 
  qqline(algaeDataset[,"NO3"])}

{qqnorm(algaeDataset[,"NH4"], main = "NH4 level") 
  qqline(algaeDataset[,"NH4"])}

{qqnorm(algaeDataset[,"oPO4"], main = "oPO4 level") 
  qqline(algaeDataset[,"oPO4"])}

{qqnorm(algaeDataset[,"PO4"], main = "PO4 level") 
  qqline(algaeDataset[,"PO4"])}

{qqnorm(algaeDataset[,"Chla"], main = "Chla level") 
  qqline(algaeDataset[,"Chla"])}

{plot(qchisq((1:nrow(x) - 1/2) / nrow(x), df = 8), sort(d),
      xlab = expression(paste(chi[3]^2, " Quantile")),
      ylab = "Ordered distances")
  abline(a = 0, b = 1)}  #symmetric distribution with flat tail on the right


#t-test statics are applied on based of the season on 7 different types of algae

#Now we will perform t-test statistics for the season and the frequencies of the algae

with(data=algaeDataset,t.test(a1[season=="winter"],a1[season=="spring"],var.equal=TRUE))
## with this we say that yes there is a lot of algaes 1 blossmings during winter and spring

with(data=algaeDataset,t.test(a1[season=="summer"],a1[season=="autumn"],var.equal=TRUE)) 
## with this we say that yes there is a lot of algaes 1 blossmings during summer and autumn too

with(data=algaeDataset,t.test(a2[season=="winter"],a2[season=="spring"],var.equal=TRUE)) 
##alage 2 also we can see the same amount of blossoms

with(data=algaeDataset,t.test(a3[season=="winter"],a3[season=="spring"],var.equal=TRUE)) 
## alage 3 also we can see the same amount of blossoms

with(data=algaeDataset,t.test(a4[season=="winter"],a4[season=="spring"],var.equal=TRUE)) 
## alage 4 also we can see the same amount of blossoms

with(data=algaeDataset,t.test(a5[season=="winter"],a5[season=="spring"],var.equal=TRUE))
## alage 5 also we can see the same amount of blossoms

with(data=algaeDataset,t.test(a6[season=="winter"],a6[season=="spring"],var.equal=TRUE)) 
## alage 6 also we can see the same amount of blossoms

with(data=algaeDataset,t.test(a7[season=="winter"],a7[season=="spring"],var.equal=TRUE)) 
## alage 7 also we can see the same amount of blossoms



#Hotelling T2 test:

#For all the algaes all the hotelling test were significant except for algae 6 where we are getting the NA value

library(Hotelling)
t2testalgae <- hotelling.test(mxPH + mnO2 + Cl + NO3 +NH4 + oPO4 + PO4 + Chla ~ a1, data=algaeDataset)
cat("T2 statistic =",t2testalgae$stat[[1]],"\n")
print(t2testalgae)

t2testalgae2 <- hotelling.test(mxPH + mnO2 + Cl + NO3 +NH4 + oPO4 + PO4 + Chla ~ a2, data=algaeDataset)
cat("T2 statistic =",t2testalgae2$stat[[1]],"\n")
print(t2testalgae2)

t2testalgae3 <- hotelling.test(mxPH + mnO2 + Cl + NO3 +NH4 + oPO4 + PO4 + Chla ~ a3, data=algaeDataset)
cat("T2 statistic =",t2testalgae3$stat[[1]],"\n")
print(t2testalgae3)

t2testalgae4 <- hotelling.test(mxPH + mnO2 + Cl + NO3 +NH4 + oPO4 + PO4 + Chla ~ a4, data=algaeDataset)
cat("T2 statistic =",t2testalgae4$stat[[1]],"\n")
print(t2testalgae4)

t2testalgae5 <- hotelling.test(mxPH + mnO2 + Cl + NO3 +NH4 + oPO4 + PO4 + Chla ~ a5, data=algaeDataset)
cat("T2 statistic =",t2testalgae5$stat[[1]],"\n")
print(t2testalgae5)

t2testalgae6 <- hotelling.test(mxPH + mnO2 + Cl + NO3 +NH4 + oPO4 + PO4 + Chla ~ a6, data=algaeDataset)
cat("T2 statistic =",t2testalgae6$stat[[1]],"\n")
print(t2testalgae6)

t2testalgae7 <- hotelling.test(mxPH + mnO2 + Cl + NO3 +NH4 + oPO4 + PO4 + Chla ~ a7, data=algaeDataset)
cat("T2 statistic =",t2testalgae7$stat[[1]],"\n")
print(t2testalgae7)

