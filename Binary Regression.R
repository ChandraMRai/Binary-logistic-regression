setwd("D:/")
library(readxl)
regression <- read_excel("D:/regression.xlsx")
View(regression)

install.packages("DescTools")
install.packages("manipulate")

library(DescTools)
library(manipulate)

str(regression)

# Binary regression

Binary <- glm(regression$LSTBinary~ regression$RVI+regression$IPVI+
             regression$DVI+regression$NDVI+regression$NDWI+regression$NDMI, 
           data = regression, family = "binomial")
summary(Binary)


# Confidence interval

confint.default(Binary)

# Odd ratio

exp(coef(Binary))

# odd ration and C.I
exp(cbind(OR=coef(Binary), confint.default(Binary)))
round (exp(cbind(OR=coef(Binary), confint.default(Binary))), 3)

#odd ratio
exp(cbind(OR=coef(Binary)))

# evaluating model fit (method 1)

Binarynull <- glm(LSTBinary~1, data = regression, family = "binomial")
summary(Binarynull)

anova(Binarynull, Binary, test="LRT")

# Modelfit using psuedo R (Method 2)

library(DescTools)
PseudoR2(Binary, which=c("McFadden", "McFaddenAdj", "Nagelkerke", "CoxSnell"))

# Modelfit method 3

# Adding predicted logits to the dataframe
regression<- data.frame(regression, PredLogit=predict(Binary))

# add the predicted odds to the dataframe
regression<- data.frame(regression, PredOdds= exp(predict(Binary)))

# add predicted probabilities to the dataframe
regression<- data.frame(regression, PredProb=regression$PredOdds/(1+regression$PredOdds))
str(regression)                        

# Correlation between predicted probabilities and group membership on the DV
r <- cor(regression$PredProb,regression$LSTBinary)

# Squaring correlation
r2 <- r^2
r
r2

# Likelihood test ratio of the predictors

# full model
Binary <- glm(regression$LSTBinary~ regression$RVI+regression$IPVI+
                regression$DVI+regression$NDVI+regression$NDWI+regression$NDMI, 
              data = regression, family = "binomial")
summary(Binary)

# testing RVI: RVI removed from model
Model1 <- glm(regression$LSTBinary~ regression$IPVI+
                regression$DVI+regression$NDVI+regression$NDWI+regression$NDMI, 
              data = regression, family = "binomial")
summary(Model1)
anova(Binary, Model1, test="LRT")

# testing IPVI: IPVI removed from model
Model2 <- glm(regression$LSTBinary~ regression$RVI+
                regression$DVI+regression$NDVI+regression$NDWI+regression$NDMI, 
              data = regression, family = "binomial")
summary(Model2)
anova(Binary, Model2, test="LRT")

# Testing NDVI: NDVI removed from model
Model3 <- glm(regression$LSTBinary~ regression$RVI+regression$IPVI+
                regression$DVI+regression$NDWI+regression$NDMI, 
              data = regression, family = "binomial")
summary(Model3)
anova(Binary, Model3, test="LRT")
# Testing DVI: DVI removed from model
Model4 <- glm(regression$LSTBinary~ regression$RVI+regression$IPVI
                +regression$NDVI+regression$NDWI+regression$NDMI, 
              data = regression, family = "binomial")
summary(Model4)
anova(Binary, Model4, test="LRT")

# Testing NDWI: NDWI removed from model
Model5 <- glm(regression$LSTBinary~ regression$RVI+regression$IPVI+
                regression$DVI+regression$NDVI+regression$NDMI, 
              data = regression, family = "binomial")
summary(Model5)
anova(Binary, Model5, test="LRT")
# Testing NDMI: NDMI removed from model
Model6 <- glm(regression$LSTBinary~ regression$RVI+regression$IPVI+
                regression$DVI+regression$NDVI+regression$NDWI, 
              data = regression, family = "binomial")
summary(Model6)
anova(Binary, Model6, test="LRT")
