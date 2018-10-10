library(glmnet)
library(useful)
library(coefplot)

land_train <- readr::read_csv('data/manhattan_Train.csv')

View(land_train)

valueFormula <- TotalValue ~ FireService + 
    ZoneDist1 + ZoneDist2 + Class + LandUse + 
    OwnerType + LotArea + BldgArea + ComArea + 
    ResArea + OfficeArea + RetailArea + 
    GarageArea + FactryArea + NumBldgs + 
    NumFloors + UnitsRes + UnitsTotal + 
    LotFront + LotDepth + BldgFront + 
    BldgDepth + LotType + Landmark + BuiltFAR +
    Built + HistoricDistrict - 1
valueFormula
class(valueFormula)

value1 <- lm(valueFormula, data=land_train)
summary(value1)
coefplot(value1, sort='magnitude')

landX_train <- build.x(valueFormula, data=land_train, 
                       contrasts=FALSE, sparse=TRUE)
landY_train <- build.y(valueFormula, data=land_train)

denseMat <- build.x(valueFormula, data=land_train, 
                       contrasts=FALSE, sparse=FALSE)
pryr::object_size(landX_train)
pryr::object_size(denseMat)

value2 <- glmnet(x=landX_train, y=landY_train, family='gaussian')
head(coef(value2), n=30)
View(as.matrix(coef(value2)))

plot(value2, xvar='lambda')
plot(value2, xvar='lambda', label=TRUE)

coefpath(value2)

library(animation)
cv.ani(k=10)

value3 <- cv.glmnet(x=landX_train, y=landY_train, family='gaussian', nfolds=5)
plot(value3)
coefpath(value3)

coefplot(value3, sort='magnitude', lambda=exp(11))
coefplot(value3, sort='magnitude', lambda='lambda.min')
coefplot(value3, sort='magnitude', lambda='lambda.1se')

value4 <- cv.glmnet(x=landX_train, y=landY_train,
                    family='gaussian', nfolds=5,
                    alpha=1)

value5 <- cv.glmnet(x=landX_train, y=landY_train,
                    family='gaussian', nfolds=5,
                    alpha=0)
coefpath(value5)

value6 <- cv.glmnet(x=landX_train, y=landY_train,
                    family='gaussian', nfolds=5,
                    alpha=0.6)
coefpath(value6)

coefplot(value6, sort='magnitude', lambda='lambda.1se')
coefplot(value5, sort='magnitude', lambda='lambda.1se')

land_test <- readRDS('data/manhattan_Test.rds')
landX_test <- build.x(valueFormula, data=land_test, 
                      contrasts=FALSE, sparse=TRUE)

valuePredictions6 <- predict(value6, newx=landX_test, s='lambda.1se')
head(valuePredictions6, n=20)
