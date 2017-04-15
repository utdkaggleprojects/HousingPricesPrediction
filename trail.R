library(xlsx)
traindata<-read.csv(file = 'C:/Users/Desktop/R/Housing/train.csv')
testdata<-read.csv(file='C:/Users/Desktop/R/Housing/test.csv')

#removing the sales price
Train_without_price<-traindata[,-which(names(traindata) %in% c("SalePrice"))]
#combining the train and test
combined_data<-rbind(Train_without_price,testdata)


str(reduced_combined)
#changing the data type 
sapply(combined_data,class)
combined_data$Id<-factor(combined_data$Id)
combined_data$MSSubClass<-factor(combined_data$MSSubClass)

combined_data$OverallQual<-ordered(combined_data$OverallQual)
class(combined_data$OverallQual)
combined_data$OverallCond<-ordered(combined_data$OverallCond)




#getting the no of years 
tmp<-as.Date(Sys.Date(),'%m/%d/%Y')

Currentyeaar<-as.numeric(format(tmp,'%Y'))

combined_data$YearBuilt<-Currentyeaar-combined_data$YearBuilt
combined_data$YearRemodAdd<-Currentyeaar-combined_data$YearRemodAdd

combined_data$ExterQual<-factor(combined_data$ExterQual)	
combined_data$ExterCond <-factor(combined_data$ExterCond)
combined_data$BsmtQual<-factor(combined_data$BsmtQual)
combined_data$BsmtCond<-factor(combined_data$BsmtCond)	
combined_data$BsmtExposure<-factor(combined_data$BsmtExposure)
combined_data$BsmtFinType1<-factor(combined_data$BsmtFinType1)
combined_data$BsmtFinType2<-factor(combined_data$BsmtFinType2)
combined_data$HeatingQC<-factor(combined_data$HeatingQC)
combined_data$FireplaceQu<-factor(combined_data$FireplaceQu)
combined_data$GarageQual<-factor(combined_data$GarageQual )	
combined_data$GarageCond<-factor(combined_data$GarageCond)
combined_data$PoolQC<-factor(combined_data$PoolQC)	



percent_of_missing<-sapply(combined_data,function(x) round(sum(is.na(x)/2919),digits = 4))
#removing the variaables whose percent greater than 5%



varibles_in_use<-names(percent_of_missing[((percent_of_missing >= 0 ) &(percent_of_missing <.05 ))])
varibles_in_use






#Visualizing the missing variables in the data set 

aggr_plot <- aggr(reduced_combined, col=c('navyblue','red'),
                  numbers=TRUE, sortVars=TRUE, labels=names(reduced_combined), cex.axis=.9, gap=2, ylab=c("Histogram of missing data","Pattern"))






Varibles_need_imputation<-names(percent_of_missing[((percent_of_missing > 0 ) &(percent_of_missing <.05 ))])
Varibles_need_imputation
 


#After removing the unnecessary variables 
reduced_combined <- combined_data[,varibles_in_use]

library(Rcpp)
library(mice)
library(VIM)#visualize the missing values 
library(ggplot2)


#imputation of the missing values
set.seed(100)

impu<-mice(reduced_combined[,Varibles_need_imputation],method = c('polr','polr','polr','polr','polr',
                                                                  'pmm','polr','polr','polr','polr',
                                                                  'pmm','polr','pmm','pmm','pmm',
                                                                  'polr','pmm','pmm','polr','polr',
                                                                  'pmm','pmm','polr'

))




complete(impu)
impppute<-complete(impu)


#Draw visualization before and after imputation 






# imputation predictors should be 15 to 20 variables 

#checking missing values in original and imputed data 
a<-sum(is.na(reduced_combined$MasVnrArea))
a
b<-sum(is.na(impppute$MasVnrArea))
b
#removing all the plots 
dev.off()

df<-data.frame(reduced_combined$Id,reduced_combined$MasVnrArea,impppute$MasVnrArea)

p1<-ggplot(df,aes(x=df$reduced_combined.MasVnrArea))+geom_histogram(fill='Red')
p2<-ggplot(df,aes(x=df$impppute.MasVnrArea))+geom_histogram(fill='Blue')


df1<-data.frame(reduced_combined$Id,reduced_combined$TotalBsmtSF,impppute$TotalBsmtSF)

p3<-ggplot(df,aes(x=df1$reduced_combined.TotalBsmtSF))+geom_histogram(fill='Red')
p4<-ggplot(df,aes(x=df1$impppute.TotalBsmtSF))+geom_histogram(fill='Blue')

df3<-data.frame(reduced_combined$Id,reduced_combined$MSZoning,impppute$MSZoning)

p5<-ggplot(df,aes(x=df3$reduced_combined.MSZoning))+geom_histogram(fill='Red')
p6<-ggplot(df,aes(x=df3$impppute.MSZoning))+geom_histogram(fill='Blue')


df4<-data.frame(reduced_combined$Id,reduced_combined$BsmtExposure,impppute$BsmtExposure)

p7<-ggplot(df,aes(x=df4$reduced_combined.BsmtExposure))+geom_histogram(fill='Red')
p8<-ggplot(df,aes(x=df4$impppute.BsmtExposure))+geom_histogram(fill='Blue')


nrow(is.na(reduced_combined$TotalBsmtSF))
##########################
#function code to draw multiple plots using ggplot

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


###################################################################################



#plotting multiple graphs in ggplot2
multiplot(p1,p2,p3,p4 ,cols=4)

multiplot(p5,p6,p7,p8 ,cols = 4)


#some times we migh not see the difference between imputed and normal due very less missing values 


reduced_combined_imputed<-reduced_combined

#replacing the imputed values 

reduced_combined_imputed$MSZoning<-impppute$MSZoning
reduced_combined_imputed$Utilities<-impppute$Utilities
reduced_combined_imputed$BsmtCond<-impppute$BsmtCond
reduced_combined_imputed$TotalBsmtSF<-impppute$TotalBsmtSF
reduced_combined_imputed$GarageArea<-impppute$GarageArea
reduced_combined_imputed$Exterior1st<-impppute$Exterior1st
reduced_combined_imputed$BsmtExposure<-impppute$BsmtExposure
reduced_combined_imputed$Electrical<-impppute$Electrical
reduced_combined_imputed$SaleType<-impppute$SaleType
reduced_combined_imputed$Exterior2nd<-impppute$Exterior2nd
reduced_combined_imputed$BsmtFinType1<-impppute$BsmtFinType1
reduced_combined_imputed$BsmtFullBath<-impppute$BsmtFullBath
reduced_combined_imputed$MasVnrType<-impppute$MasVnrType
reduced_combined_imputed$BsmtFinSF1<-impppute$BsmtFinSF1
reduced_combined_imputed$BsmtHalfBath<-impppute$BsmtHalfBath
reduced_combined_imputed$MasVnrArea<-impppute$MasVnrArea
reduced_combined_imputed$BsmtFinType2<-impppute$BsmtFinType2
reduced_combined_imputed$KitchenQual<-impppute$KitchenQual
reduced_combined_imputed$BsmtQual<-impppute$BsmtQual
reduced_combined_imputed$BsmtFinSF2<-impppute$BsmtFinSF2
reduced_combined_imputed$Functional<-impppute$Functional
reduced_combined_imputed$BsmtUnfSF<-impppute$BsmtUnfSF
reduced_combined_imputed$GarageCars<-impppute$GarageCars


#redividing the data sets 
trainfinaldata<-reduced_combined_imputed[1:1460,]
testfinaldata<-reduced_combined_imputed[1461:2919,]

#adding the sales price again to test data set
trainfinaldata["SalePrice"]<-traindata$SalePrice

#Some intutuive descriptive statical graphs 
#Plotting based on zoness b/w area and price
ggplot(trainfinaldata,aes(x=TotalBsmtSF ,y=SalePrice/10000))+  geom_point(shape=1) +geom_smooth(method=lm) +facet_grid(MSZoning ~ .)+
  scale_x_continuous(limits=c(0,3000))

ggplot(trainfinaldata,aes(x=SaleType )) +facet_grid(MSZoning ~ . )+geom_histogram()

ggplot(trainfinaldata,aes(x=MoSold )) +facet_grid(YrSold ~ . )+geom_histogram()+scale_x_continuous(limits=c(1,12),breaks=seq(1,12,1))




#selecting the variables for the regression 
library(leaps)
library(MASS) 

#applying stepwise 

#initial model 
null<-lm(trainfinaldata$SalePrice~1,data=trainfinaldata)
#full model with all the variables 
full<-lm(trainfinaldata$SalePrice~.-Id,data = trainfinaldata)


step(null,scope = list(upper=full),data=trainfinaldata,direction = "both")

#using AIC selected model

linearmodel<-lm(formula = trainfinaldata$SalePrice ~  OverallQual + GrLivArea+Neighborhood + RoofMatl + BsmtFinSF1 +
                  MSSubClass + Condition2 +SaleCondition + OverallCond + BsmtExposure + 
                  YearBuilt + LotArea + KitchenQual + TotalBsmtSF + PoolArea + BsmtQual + 
                  Functional + ScreenPorch + Condition1 + LandSlope + LotConfig + 
                  Fireplaces + MSZoning + Exterior1st + MasVnrArea + YearRemodAdd + 
                  GarageArea + LowQualFinSF + LandContour + BsmtFinSF2 + RoofStyle + 
                  MoSold + Street + KitchenAbvGr + X3SsnPorch + Foundation + 
                  X1stFlrSF + BedroomAbvGr + FullBath + TotRmsAbvGrd, 
   data = trainfinaldata)

statsummary<-summary(linearmodel)
statsummary


#removing the unnecessary variables from the linear model "OverallCond" is related to "OverallQual" 
#LotConfig removed ,LandContour 

#Removal of three variaable reduced .01 adjusted R^2
LinearReducedModel<-lm(SalePrice ~ OverallQual+ GrLivArea + Neighborhood + RoofMatl + BsmtFinSF1 + 
                         MSSubClass + Condition2 + SaleCondition + BsmtExposure + YearBuilt + LotArea +
                         KitchenQual + TotalBsmtSF + GarageArea + PoolArea +
                         BsmtQual + Functional+ScreenPorch+Condition1 + LandSlope + Fireplaces + MSZoning +
                         Exterior1st + MasVnrArea + YearRemodAdd + LowQualFinSF + BsmtFinSF2 + RoofStyle +Street +
                         KitchenAbvGr + X3SsnPorch + Foundation + X1stFlrSF + BedroomAbvGr
                        , data = trainfinaldata)
 
summary(LinearReducedModel)



#####################################
#Diagnostics

library(car)
#Outliers of the model
outlierTest(LinearReducedModel)

#residual for the 
qqPlot(LinearReducedModel, main="QQ Plot")

#seeing the linear dependency for the independent variables keeping the other variables constant which is different than drawing 
#drawing literal values of independent and dependent 
#It helps to see the superficial picture of the relation b/w independent and dependent
leveragePlots(LinearReducedModel)


#http://stats.stackexchange.com/questions/125561/what-does-an-added-variable-plot-partial-regression-plot-explain-in-a-multiple



# residual vs y value 
qplot(fitted(LinearReducedModel),resid(LinearReducedModel))
# hetroskedasticity need to change the model 

#distribution of error 
library(MASS)

sresid<-resid(LinearReducedModel)

hist(sresid,freq = FALSE,main = "Distrubution of Studentized residuals")

#Non-Constant error variance
ncvTest(LinearReducedModel)
spreadLevelPlot(LinearReducedModel)


#getting the outliers of the data set
outliers<-trainfinaldata[c(524,826,899,804,1325,692,1183,1047,689,775),]

#Veerifying the Colinearity between the variables 
vif(LinearReducedModel)


#sqrt(vif(fit)) > 2 we have trouble with the covariance
# After looking at the VIF results only numeric values has VIF measure  greater than 2 ,so get the correlation matrix for the 
#numeric variables 
nums<-sapply(trainfinaldata,is.numeric)
numericpartdata<-trainfinaldata[,nums]

#Pearson :If the relationship is that one variable increases when the other increases, 
#http://support.minitab.com/en-us/minitab-express/1/help-and-how-to/modeling-statistics/regression/supporting-topics/basics/a-comparison-of-the-pearson-and-spearman-correlation-methods/

library(Hmisc)
res2<-rcorr(as.matrix(numericpartdata),type ='pearson' )

#geetting the correlation pair-wise 
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

flattencorrelation<-flattenCorrMatrix(res2$r,res2$P)
flattencorrelation



paircorrelation<-subset(flattencorrelation,((flattencorrelation$cor > .7)&(flattencorrelation$p < .05)),select =c(row,column))
paircorrelation

#comparing VIF and paircorrelatin 
#variables which has value greaterhan 2 
GrLivArea
YearBuilt
TotalBsmtSF
X1stFlrSF

#After comparing the VIF and Correlation coefficient new model "X1stFlrSF" removed

LinearReducedModelversion2<-lm(SalePrice ~ OverallQual+ GrLivArea + Neighborhood + RoofMatl + BsmtFinSF1 + MSSubClass +
                         Condition2 + SaleCondition + BsmtExposure + YearBuilt + LotArea +
                         KitchenQual + TotalBsmtSF + GarageArea + PoolArea +
                         BsmtQual + Functional+ScreenPorch+Condition1 + LandSlope + Fireplaces + MSZoning +
                         Exterior1st + MasVnrArea + YearRemodAdd + LowQualFinSF + BsmtFinSF2 + RoofStyle +Street +
                         KitchenAbvGr + X3SsnPorch + Foundation + BedroomAbvGr
                       , data = trainfinaldata)


vif(LinearReducedModelversion2)
summary(LinearReducedModelversion2)

#VIF has for GrLivArea ,YearBuilt,TotalBsmtSF
#Though we  removed few variables analyzing VIF and Correlation still we have  VIF>2



 

#drawing distribution of all variabes 

str(numericpartdata)
hist.data.frame(numericpartdata[,c(1:10)])

summary(numericpartdata)



#cheking the skewness and tranforming

#values between +2 and -2 are  Acceptable ,check all the variable skewness and follow Power transformation for 
# or log transformation  
skewness(numericpartdata$GrLivArea)
skewvec<-sapply(numericpartdata,skewness)
skewvec

library(e1071)

names(numericpartdata)
hist(numericpartdata$GrLivArea)

dev.off()
par(mfrow=c(3,3))
#Transformaing the variables 
hist(log(numericpartdata$BsmtFinSF1))
hist(log(numericpartdata$LotArea))
hist(log(numericpartdata$GarageArea))
hist(log(numericpartdata$PoolArea))########
hist(log(numericpartdata$ScreenPorch))
hist(log(numericpartdata$MasVnrArea))
hist(log(numericpartdata$LowQualFinSF))
hist(log(numericpartdata$BsmtFinSF2))
hist(log(numericpartdata$X3SsnPorch))



# we need find some optimu value using http://rstudio-pubs-static.s3.amazonaws.com/1563_1ae2544c0e324b9bb7f6e63cf8f9e098.html




LinearReducedModelversion4<-lm(SalePrice ~ OverallQual+GrLivArea + Neighborhood + RoofMatl + log(BsmtFinSF1 +1) +
                                 MSSubClass +Condition2 + SaleCondition + BsmtExposure  + log(LotArea) +
                                 KitchenQual  + log(GarageArea +1 ) + log(PoolArea +1) +BsmtQual + Functional+
                                 log(ScreenPorch)+Condition1 + LandSlope + Fireplaces +MSZoning +
                                 Exterior1st + log(MasVnrArea +1 )+ YearRemodAdd + log(LowQualFinSF + 1) +log(BsmtFinSF2 + 1) + 
                                 RoofStyle +Street + KitchenAbvGr + log(X3SsnPorch +1) + Foundation + BedroomAbvGr, data = trainfinaldata)

#we might want to add +1 for the values which has value 0 to avoid infinity , if your values are going to negitive then 

LinearReducedModelversion4<-lm(SalePrice ~ OverallQual+GrLivArea + Neighborhood + RoofMatl + log(BsmtFinSF1 +1) +
                                 MSSubClass +Condition2 + SaleCondition + BsmtExposure  + log(LotArea) +
                                KitchenQual  + log(GarageArea +1 ) + log(PoolArea +1) +BsmtQual + Functional+
                                 log(ScreenPorch +1) + Condition1 + LandSlope + Fireplaces +MSZoning +
                        Exterior1st + log(MasVnrArea +1 ) + YearRemodAdd + log(LowQualFinSF + 1)  + log(BsmtFinSF2 + 1) + 
                          RoofStyle +Street + KitchenAbvGr + log(X3SsnPorch +1)  + Foundation + BedroomAbvGr, data = trainfinaldata)


summary(LinearReducedModelversion4)

 
qplot(fitted(LinearReducedModelversion4),resid(LinearReducedModelversion4)) 

#Still hetroscedasticity exists 

skewness(resid(LinearReducedModelversion4))
dev.off()
#BoxCox plot for reducing the standard error 
library(MASS)
LinearReducedModelversion5<-boxcox(SalePrice ~ OverallQual+GrLivArea + Neighborhood + RoofMatl + log(BsmtFinSF1 +1) +
                                     MSSubClass +Condition2 + SaleCondition + BsmtExposure  + log(LotArea) +
                                     KitchenQual  + log(GarageArea +1 ) + log(PoolArea +1) +BsmtQual + Functional+
                                     log(ScreenPorch +1) + Condition1 + LandSlope + Fireplaces +MSZoning +
                                     Exterior1st + log(MasVnrArea +1 ) + YearRemodAdd + log(LowQualFinSF + 1)  + log(BsmtFinSF2 + 1) + 
                                     RoofStyle +Street + KitchenAbvGr + log(X3SsnPorch +1)  + Foundation + BedroomAbvGr, data = trainfinaldata)

Lamda<-LinearReducedModelversion5$x
Liklihood<-LinearReducedModelversion5$y

Dataframe<-cbind(Lamda,Liklihood)
Dataframe[order(-Liklihood),]

#Choosing the maximum liklihood lamda value 

LinearReducedModelversion6<-lm(SalePrice^(.22) ~ OverallQual+GrLivArea + Neighborhood + RoofMatl + log(BsmtFinSF1 +1) +
                                     MSSubClass +Condition2 + SaleCondition + BsmtExposure  + log(LotArea) +
                                     KitchenQual  + log(GarageArea +1 ) + log(PoolArea +1) +BsmtQual + Functional+
                                     log(ScreenPorch +1) + Condition1 + LandSlope + Fireplaces +MSZoning +
                                     Exterior1st + log(MasVnrArea +1 ) + YearRemodAdd + log(LowQualFinSF + 1)  + log(BsmtFinSF2 + 1) + 
                                     RoofStyle +Street + KitchenAbvGr + log(X3SsnPorch +1)  + Foundation + BedroomAbvGr, data = trainfinaldata)

summary(LinearReducedModelversion6)



qplot(fitted(LinearReducedModelversion6),resid(LinearReducedModelversion6)) 

hist(resid(LinearReducedModelversion6),freq = FALSE,main = "Distrubution of Studentized residuals")




###############################################################

table(trainfinaldata$MSZoning)









unique(testfinaldata$MSSubClass)
nrow(testfinaldata$MSSubClass=='150')
unique(trainfinaldata$MSSubClass)


#level 150 is not there in the train data so replacing with a meaningfull value 
#150 represents the 1 and 1/2 floor we are replacing with 2 floor which is 160

id<-testfinaldata[testfinaldata$MSSubClass =='150' ,]
id

testfinaldata[testfinaldata$MSSubClass=='150',]$MSSubClass<-'160'


saleprice<-predict(LinearReducedModelversion6,testfinaldata)

predictedvalues<-cbind(as.integer(testfinaldata$Id),saleprice^(9/2))

write.csv(predictedvalues,'C:/Users/Lava kumar Bada/Desktop/R/sample_submission.csv')

crap<-testfinaldata[testfinaldata$Id %in% c('1556',
                                         '1916',
                                         '2121',
                                         '2152',
                                         '2217',
                                         '2251',
                                         '2474',
                                         '2577',
                                         '2905'),]


table(testfinaldata$BsmtExposure)




LinearReducedModelversion7<-lm(SalePrice ~ OverallQual+GrLivArea + Neighborhood + RoofMatl + log(BsmtFinSF1 +1) +
                                     MSSubClass +Condition2 + SaleCondition + BsmtExposure  + log(LotArea) +
                                     KitchenQual  + log(GarageArea +1 ) + log(PoolArea +1) +BsmtQual + Functional+
                                     log(ScreenPorch +1) + Condition1 + LandSlope + Fireplaces +MSZoning +
                                     Exterior1st + log(MasVnrArea +1 ) + YearRemodAdd + log(LowQualFinSF + 1)  + log(BsmtFinSF2 + 1) + 
                                     RoofStyle +Street + KitchenAbvGr + log(X3SsnPorch +1)  + Foundation + BedroomAbvGr, data = trainfinaldata)


saleprice<-predict(LinearReducedModelversion7,testfinaldata)
predictedvalues<-cbind(as.integer(testfinaldata$Id),saleprice)

predictedvalues<-cbind(as.integer(testfinaldata$Id),saleprice^(9/2))


write.csv(predictedvalues,'C:/Users/Lava kumar Bada/Desktop/R/sample_submission.csv')



qplot(fitted(LinearReducedModelversion7),resid(LinearReducedModelversion7)) 
