#install.packages("rgl")
#library(rgl)
library(MASS)
library(fBasics)
library(fRegression)
library(car)


par(mfrow=c(2,2))
hist(GLA$UnderweightYear6,
     main = "Year 6 Underweight",
     xlab = "Underweight",
     col="blue")


hist(GLA$HealthyweightYear6,
     main = "Year 6 Healthy Weight",
     xlab = "Healthy Weight",
     col="grey")

hist(GLA$ObeseYear6,
     main = "Year 6 Obesity",
     xlab = "Obesity Rate (%)",
     col="magenta")

hist(GLA$ChildPoverty,
     main = "Child Poverty",
     xlab = "Poverty Rate (%)",
     col="yellow")


data = as.data.frame(GLA)
names(GLA)
#initial lm
par(mfrow=c(2,2))
cp_lm = lm(GLA$ChildPoverty ~., data = data[4:28]) # remove text variables and ObeseYear 6: cp = child poverty
summary(cp_lm)


plot(cp_lm) # inspection of the regression model; including Cook's Distance


#stepwise regression model for dimensionality reduction
cp_AIC = stepAIC(cp_lm, scale=0, direction=c("both"),
                 steps=1000, k=2)


plot(cp_AIC)

# outlier observations
par(mfrow=c(1,1))
cooks_d = cooks.distance(cp_AIC)
plot(cooks_d, main = "Cooks Distance for Stepwise Regression",
     ylab = "Distance Score",
     xlab = "London Borough",
     type="l",
     col="red")

# outliers noted in boroughs 3, 12, 31, 2



# final model for cp_step obtained

final_cp_mod = lm(GLA$ChildPoverty ~ PopulationDensity + AverageAge + Pop_65andover + 
                    Employmentrate + CrimeRate + QualificationsGCSE + OutofWorkBenefits + 
                    IncomeSupportRate + GrossAnnualPay + LifeExpectMale + TescoTotal, data=data)

summary(final_cp_mod)


# repeat process for childhood obesity in year 6 children: co = childhood obesity
#initial linear model

co_lm = lm(GLA$ObeseYear6 ~., data = data[4:28])
summary(co_lm)
plot(co_lm)


# stepAIC

co_AIC = stepAIC(co_lm, scale=0, direction=c("both"),
                 steps=1000, k=2)


# run co_AIC - find only reduction vars. Choose lowest vif score for correlated variables
final_mod = cbind(data$PopulationDensity, data$CouncilHousing, data$AverageAge)

# Tesco Total has low VIF but doesn't change the Adj R2 so not worth adding
#final_mod = cbind(data$PopulationDensity, data$CouncilHousing, data$AverageAge , data$TescoTotal)
#library(car) # for VIF package
vif(final_co_mod)

vif(final_cp_mod)
round(cor(GLA[5:27]), 3)

plot(co_AIC)
cooks_d = cooks.distance(co_AIC)

plot(cooks_d, main = "Cooks Distance for Stepwise Regression",
     ylab = "Distance Score",
     xlab = "London Borough",
     type="l",
     col="red")


# outliners noted in boroughs 3, 12, 31, 2
# final model for co_step obtained


final_co_mod = lm(GLA$ObeseYear6 ~ Population + PopulationDensity + AverageAge + 
                    Employmentrate + Unemploymentrate + CrimeRate + QualificationsNone + 
                    QualificationsLevel4 + QualificationsGCSE + OutofWorkBenefits + 
                    IncomeSupportRate + CouncilHousing + LifeExpectMale + LifeExpectFemale + 
                    DepreviationIndex + TescoTotal, data=data)

summary(final_co_mod)

#confirm? vif with pca
vif(final_co_mod)
vif(final_cp_mod)

# select mutually significant variables (***)

summary(final_cp_mod)
summary(final_co_mod)

sig_vars = cbind(GLA$Population, GLA$AverageAge, GLA$Pop_65andover, GLA$Employmentrate, GLA$QualificationsGCSE, GLA$OutofWorkBenefits, GLA$IncomeSupportRate,
                 GLA$LifeExpectMale, GLA$LifeExpectFemale, GLA$Unemploymentrate, GLA$CrimeRate, GLA$QualificationsNone, GLA$QualificationsLevel4, GLA$CouncilHousing,
                 GLA$DepreviationIndex, GLA$GrossAnnualPay)


colnames(sig_vars) <- c("Population", "Average Age", "Over 65s", "Employment Rate", "Qualifications GCSE", "Out of Work Benefits", "Income Support Rate",
                        "Life Expectancy Male", "Life Expectancy Female", "Unemployment Rate", "Crime Rate", "No Qualifications", "Qualifications Level 4",
                        "Council Housing", "Deprivation Index", "Gross Annual Pay")
## use model with highest reduction in AIC between highly correlated variables
cors = round(cor(sig_vars), 3)
View(cors)

GGally::ggcorr(sig_vars, method=c("pairwise"))




np=manova(lm(cbind(GLA$ChildPoverty, GLA$ObeseYear6) ~., data=as.data.frame(sig_vars)))
summary(np,test = "Wilks")

sig_pca = prcomp(sig_vars, center=TRUE, scale=TRUE)

summary(sig_pca)
plot(sig_pca, type ='l',
     main = "PCA Screeplot")
round(sig_pca$rotation, 3)

total_variance = sum(sig_pca$sdev^2)
round(sig_pca$sdev^2/sum(sig_pca$sdev^2)*100,2)
summary(sig_pca)
sig_pca_rotation = round(sig_pca$rotation, digits=2)
sig_pca_rotation
dual = cbind(GLA$ChildPoverty, GLA$ObeseYear6)
plot(dual,
     main = "Childhood Poverty and Obesity Prevelance in London",
     xlab="Child Poverty",
     ylab="Obesity Year 6")

cor(sig_pca$x)

head(GLA$ChildPoverty)
head(GLA$ObeseYear6)
cp_pca_table = round(cbind(GLA$ChildPoverty, data.frame(sig_pca$x)))
cp_cor = round(cbind(GLA$ChildPoverty, data.frame(sig_pca$x)))
cor(cp_pca_table)
head(cp_cor)

install.packages("pcr")
pcr_x = pcr(GLA$ChildPoverty ~., data=as.data.frame(sig_vars))
pcr_x$loadings


x = pcr_x$model
round(x, 3)

table_cor = round(full_table, 3)
new_x = round(cor(cbind(GLA[,26:27], data.frame(pca_data$x))), digits=3)

library(plotly)



x = cbind(GLA$CrimeRate, GLA$Unemploymentrate, GLA$GrossAnnualPay)
x = cbind(GLA$CrimeRate, GLA$DepreviationIndex, GLA$ObeseYear6)


library(plotly)
p <- plot_ly(z=~x, type="surface")
p



manv_mod = manova(lm(cbind(GLA$ObeseYear6, GLA$ChildPoverty) ~ data$Population + data$IncomeSupportRate + data$CouncilHousing))
summary(manv_mod)


hotelling_lawley = summary(manv_mod, test="Hotelling-Lawley")
pillai_test = summary(manv_mod, test="Pillai")
wilks_lambda = summary(manv_mod, test="Wilks")
roys_test = summary(manv_mod, test="Roy")




# the model has improved following the AIC 

fin_mod = lm(GLA$ObeseYear6 ~ data$Population + data$IncomeSupportRate + data$CouncilHousing)
fin_mod = lm(GLA$ObeseYear6 ~ data$Population + data$IncomeSupportRate)
summary(fin_mod)



# notice wilks and lambda are inverse 1 - pillai = wilks lambda and Hotelling-Lawley and Roy's root are the exact. 
vif(manv_mod2)
cor(data$CouncilHousing, data$IncomeSupportRate)
cor(GLA$ChildPoverty, GLA$ObeseYear6, method=c("pearson"))



# kmeans clustering

par(mfrow=c(1,1))
dual = cbind(GLA$ObeseYear6, GLA$ChildPoverty)
colnames(dual) = c("Obesity Rate", "Child Poverty")
kmeans_bind = kmeans(dual, centers=5, iter.max=10L)
plot(dual, main="Clustering boroughs with k-means", type="n")
#points(dual, pch=3, col=fin_kmeans$cluster)
text(dual, labels=GLA$Borough, col=kmeans_bind$cluster)
cluster_variability = (kmeans_bind$betweenss/kmeans_bind$totss)
round(kmeans_bind$centers, 2)
