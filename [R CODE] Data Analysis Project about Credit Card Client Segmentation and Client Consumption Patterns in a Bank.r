##### Data Analysis Project #####
##### Credit Card Client Segmentation and Client Consumption Patterns in a Bank #####

### Libraries and Utilities ###
library(readxl)
library(tidyverse)
library(MASS)
library(car)
library(ggplot2)
library(ggcorrplot)
library(RcmdrMisc)
library(psych)
library(GPArotation)
library(car)
library(ggplot2)
library(Hmisc)
library(corrplot)
library(PerformanceAnalytics)
library(lavaan)
library(haven)
library(dplyr)
library(factoextra)
library(fastDummies)
library(lattice)
library(caret)
library(ROCR)
library(nFactors)
library(sem)
library(tidyverse)
library(cluster)
library(datasets)
library(readr)
library(data.table)
library(stats)
library('datasets')
library('prediction')
library(tidypredict)
library(remotes)

### Import Data ###
library(readxl)
data <- read_excel("[DATA] Data Analysis Project about Credit Card Client Segmentation and Client Consumption Patterns in a Bank.xlsx", 
                   sheet = "cust_segm")
df <- data[,-1] #remove kolom pertama
View(df)
head(df)
tail(df)

### Describe Data ###
dim(df)
names(df)
str(df)

####################### Exploratory Data Analysis ##############################
### Data Wrangling ###
## 1. Handling Missing Value ##
colSums(is.na(df))

# Menghapus Missing Value
df1 <- na.omit(df)
View(df1)
head(df1)
tail(df1)
dim(df1)

summary(df1)
colSums(is.na(df1))

### Visualisasi Data ###
## Variabel Numerik ##
# Plot Korelasi Antar Variabel
df2 <- df1 %>% select_if(is.numeric)
ggcorrplot(cor(df2), hc.order = FALSE,type = "full",lab = TRUE)

# Plot Korelasi 
library(psych)
pairs.panels(df1[1:17], method = "pearson",
             hist.col = "grey", density = TRUE,ellipses = TRUE,lm = FALSE) 

### Scatterplot Matriks Korelasi ###
library(car)
scatterplotMatrix(df1,
                  main="Matriks Korelasi",smooth = FALSE)

plot(df1[,1:17])
plot(df1[,c(3:5,7:9)])
plot(df1[,c(6,10:14)])
plot(df1[,15:17])

### Histogram ###
par(mfrow = c(1,3))
hist(df1$BALANCE, main = "Histogram dari BALANCE", col = 'red')
hist(df1$BALANCE_FREQUENCY, main = "Histogram dari BALANCE_FREQUENCY", col = 'red')
hist(df1$PURCHASES, main = "Histogram dari PURCHASES", col = 'red')
hist(df1$ONEOFF_PURCHASES, main = "Histogram dari ONEOFF_PURCHASES", col = 'red')
hist(df1$INSTALLMENTS_PURCHASES, main = "Histogram dari INSTALLMENTS_PURCHASES", col = 'red')
hist(df1$CASH_ADVANCE, main = "Histogram dari CASH_ADVANCE", col = 'red')
hist(df1$PURCHASES_FREQUENCY, main = "Histogram dari PURCHASES_FREQUENCY", col = 'red')
hist(df1$ONEOFF_PURCHASES_FREQUENCY, main = "Histogram dari ONEOFF_PURCHASES_FREQUENCY", col = 'red')
hist(df1$PURCHASES_INSTALLMENTS_FREQUENCY, main = "Histogram dari PURCHASES_INSTALLMENTS_FREQUENCY", col = 'red')
hist(df1$CASH_ADVANCE_FREQUENCY, main = "Histogram dari CASH_ADVANCE_FREQUENCY", col = 'red')
hist(df1$CASH_ADVANCE_TRX, main = "Histogram dari CASH_ADVANCE_TRX", col = 'red')
hist(df1$PURCHASES_TRX, main = "Histogram dari PURCHASES_TRX", col = 'red')
hist(df1$CREDIT_LIMIT, main = "Histogram dari CREDIT_LIMIT", col = 'red')
hist(df1$PAYMENTS, main = "Histogram dari PAYMENTS", col = 'red')
hist(df1$MINIMUM_PAYMENTS, main = "Histogram dari MINIMUM_PAYMENTS", col = 'red')
hist(df1$PRC_FULL_PAYMENT, main = "Histogram dari PRC_FULL_PAYMENT", col = 'red')
hist(df1$TENURE, main = "Histogram dari TENURE", col = 'red')

####################### Principal Component Analysis (PCA) #####################
### Import Data ###
library(readxl)
data <- read_excel("D:/Document Bram/Michael Bram UI/SEMESTER 6/Analisis Multivariat 2/Tugas/Tugas 3 (Analisis Diskriminan, PCA, Analisis Faktor, Analisis Cluster)/pokemon.xlsx", 
                   sheet = "Data Against")
View(data)

### Standarisasi Data ### 
scaled.data <- scale(df1)
head(scaled.data)

### Vector Mean dan Covariance Matrix ###
#Vector Mean
ybar <- matrix(colMeans(scaled.data), ncol=1)
ybar

#Covariance Matrix
covariance.matrix <- cov(scaled.data)
covariance.matrix

### Eigen Value dan Eigen Vector ###
# dapat dignakan pada covariance matrix
eigen <- eigen(covariance.matrix)
eigen

### PRINCIPAL COMPONENT ANALYSIS (PCA) ###
pca <- prcomp(x = scaled.data, scale. = TRUE, center = TRUE)
summary(pca)

### Total Variance ###
sum(diag(covariance.matrix))
sum(eigen$values)

### Proportion of Variance ###
prop.variance <- eigen$values/sum(eigen$values)
prop.variance

### Cumulative of Proportion ###
cumprop <- cumsum(prop.variance)
cumprop

### Membuat Proportion Plot ### 
prop.data <- data.frame(x=1:17,
                        Percentage=c(prop.variance,
                                     cumprop),
                        Legend=c(rep('Propotion of Variance',17),rep('Cumulative 
Proportion',17)))
ggplot(prop.data, aes(x,Percentage,col=Legend,linetype = Legend)) +
  geom_line(lwd = 1) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 7)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  xlab('PCA') +
  theme_minimal() +
  ggtitle("Proportion Plot")

### Membuat Scree Plot ###
pca.var <- pca$sdev^2
pca.var
pca.var.per <- round(pca.var/sum(pca.var)*100, 1)
plot(pca.var.per, main = "Scree Plot",
     xlab = "Principal Component",
     ylab = "Percent Variaton", type = "b")

### Principal Component Analysis ###
fit <- principal(df1, x$ncomp, rotate="varimax") 
print(fit$loadings, cutoff=.3)

#Plot PCA
factor.plot(fit)
fa.diagram(fit)

############################### Factor Analysis (FA) ###########################
### Standarisasi Data ### 
scaled.data <- scale(df1)
head(scaled.data)

### Uji Kelayakan Data ###
# Mengukur Kecukupan Sampling
#Uji KMO (Kaisar Meyer Olkin)
KMO(scaled.data)

# Mengetahui adanya korelasi pada variabel
#Uji Bartlet
library(REdaS)
library(psych)
bart_spher(scaled.data, use = "everything")

######################## Exploratory Factor Analysis ###########################
### Matriks Kovariansi ###
covariance.matrix <- cov(df1)
round(covariance.matrix,3)

### Matriks Korelasi ###
correlation.matrix <- rcorr(as.matrix(df1))
round(correlation.matrix$r,3)

# Plot Matriks Korelasi
chart.Correlation(correlation.matrix$r)
par(mfrow=c(1,2))

### Scree Plot ###
scree(df1)

### Menentukan Banyaknya Faktor ###
x <- fa.parallel(df1, fm="pa", fa="both", n.iter=1)

### Menentukan Analisis Faktor ###
### Cara 1: Dengan Factor Loadings ###
library(psych)
efa <- fa(df1, nfactors=6, rotate = 'varimax')
sqrt(eigen(covariance.matrix)$values)
print(efa$loadings)

#Plot FA
factor.plot(efa)
fa.diagram(efa)

### Cara 2 : Dengan factanal ###
factanal(scaled.data, factors = 6, rotation ="varimax", scores = c("regression"))

### Cara 3:
fit <- fa(df1, x$nfact, rotate="varimax", fm="pa")
print(fit$loadings, cutoff=.3)

#Plot FA
factor.plot(fit)
fa.diagram(fit)

### Communalities ###
efa$communality

### Specific Variances ###
efa$uniquenesses

### Summary Model ###
efa
summary(efa)

### Diagram Component Analysis ###
fa.diagram(efa)

load <- efa$loadings[,1:2]
plot(load,type="n") # set up plot
text(load,labels=names(df),cex=.7)

######################## Confirmatory Factor Analysis ##########################
### Buat Model Sesuai dengan Hasil EFA ### (lihat nilai terbesarnya)
library(lavaan)
cfa.model <- "PA1=~PURCHASES+ONEOFF_PURCHASES+PAYMENTS+INSTALLMENTS_PURCHASES+PURCHASES_TRX 
PA2=~CASH_ADVANCE_FREQUENCY+CASH_ADVANCE_TRX+CASH_ADVANCE
PA3=~PURCHASES_INSTALLMENTS_FREQUENCY+PURCHASES_FREQUENCY
PA4=~BALANCE+CREDIT_LIMIT+MINIMUM_PAYMENTS
PA5=~ONEOFF_PURCHASES_FREQUENCY
PA6=~PRC_FULL_PAYMENT+BALANCE_FREQUENCY"

### Summary Model ###
cfa.fit <- cfa(cfa.model, data=scaled.data, std.lv = TRUE)
cfa.fit
summary(cfa.fit, fit.measures = TRUE, standardized = TRUE)

var(df$hp)

summary(cfa.fit, standardized = TRUE)

library(semPlot)
semPaths(cfa.fit, "std")

################### Clustering Analysis (Non Hierarkikal) ######################
### Cara 1:
### Mencari Jumlah  Cluster Optimal ###
#Metode 1: Elbow method
fviz_nbclust(scaled.data, kmeans, method = "wss") +
  geom_vline(xintercept = 7, linetype = 5) +
  labs(subtitle = "Elbow method")

#Metode 2: Silhouette method
fviz_nbclust(scaled.data, kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette method")

#Metode 3: Gap statistic
set.seed(123)
fviz_nbclust(scaled.data, kmeans, nstart = 25,  method = "gap_stat", nboot = 50) +
  labs(subtitle = "Gap statistic method")
gap_stat <- clusGap(scaled.data, FUN = kmeans, nstart = 25,
                    K.max = 6, B = 50)
fviz_gap_stat(gap_stat)

### Cara 2:
#Metode 1:  Within Sum Square Method
fviz_nbclust(scaled.data, kmeans, method = c("wss"), k.max = 6)

#Metode 2: Silhouette Square Method
fviz_nbclust(scaled.data, kmeans, method = "silhouette", k.max = 6)

#Metode 3: Gap Stat#
fviz_nbclust(scaled.data, kmeans, method = "gap_stat", k.max = 6)
gap_stat <- clusGap(scaled.data, FUN = kmeans, nstart = 25,
                    K.max = 6, B = 50)
fviz_gap_stat(gap_stat)

### Pembentukkan Klaster ### 
klaster7 <- kmeans(scaled.data, iter.max = 1000, centers = 7, nstart = 25)
klaster9 <- kmeans(scaled.data, iter.max = 1000, centers = 9, nstart = 25)
fviz_cluster(klaster9, df2, stand = F, geom = "point")

klaster10 = kmeans(df2, iter.max = 1000, centers = 1, nstart = 25)
fviz_cluster(klaster10, df2, stand = F, geom = "point")

### Pembentukan Klaster ###
#Kmeans Clustering
klaster7 <- kmeans(scaled.data, centers=7, nstart=25)
klaster9 <- kmeans(scaled.data, centers=9, nstart=25)
klaster10 <- kmeans(scaled.data, centers=10, nstart=25)
data.frame(klaster7$tot.withinss, klaster9$tot.withinss, klaster10$tot.withinss)
data.frame(klaster7$betweenss, klaster9$betweenss, klaster10$betweenss)

### Pembentukan Klaster ###
#Kmeans clustering
klaster7 <- kmeans(scaled.data, centers=7, nstart=25)
klaster7
klaster9 <- kmeans(scaled.data, centers=9, nstart=25)
klaster9
klaster10 <- kmeans(scaled.data, centers=10, nstart=25)
klaster10
data.frame(klaster7$cluster, klaster9$cluster, klaster10$cluster)
klaster7$centers
klaster9$centers
klaster10$centers
data.frame(klaster7$totss, klaster9$totss, klaster10$totss)
klaster7$withinss
klaster9$withinss
klaster10$withinss
data.frame(klaster7$tot.withinss, klaster9$tot.withinss, klaster10$tot.withinss)
data.frame(klaster7$betweenss, klaster9$betweenss, klaster10$betweenss)

### Membandingkan Klaster ###
withinss7 <- sum(klaster7$withinss)
withinss9 <- sum(klaster9$withinss)
withinss10 <- sum(klaster10$withinss)
data.frame(withinss7, withinss9, withinss10)
data.frame(klaster7$betweenss, klaster9$betweenss, klaster10$betweenss)