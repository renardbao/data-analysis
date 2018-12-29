library(knitr)
library(ggplot2)
library(plyr)
library(dplyr)
library(corrplot)
library(caret)
library(gridExtra)
library(scales)
library(Rmisc)
library(ggrepel)
library(randomForest)
library(psych)
library(xgboost)
library(magrittr)
library(data.table)
library(stringr)

options(scipen = 999)
setwd("D:/r_project/data-analysis/house_sale_predict")
train <- read.csv("train.csv", stringsAsFactors = F)
test <- read.csv("test.csv", stringsAsFactors = F)
test_labels <- test$Id
test$SalePrice <- NA
all <- rbind(train, test)
dim(all)
#將是數值的變數名挑出來
num_var <-  sapply(all, is.numeric) %>% which %>% names()
#將是類別的變數名挑出來
char_var <- sapply(all, is.character) %>% which %>% names()
cat("數值變數有",length(num_var),"個",",類別變數有",length(char_var),"個")

#觀察目標變數的分布----
all %>% filter(!(is.na(SalePrice))) %>% ggplot(aes(x = SalePrice)) + 
  geom_histogram(fill="deeppink2", binwidth = 10000) +
  scale_x_continuous(breaks= seq(0, 800000, by=100000),labels = comma)


#相關性----
cor_numvar <- cor(all[, num_var], use="pairwise.complete.obs")
#排序之後挑出去相關性絕對值大於0.4的
cor_sortname <- sort(cor_numvar[,'SalePrice'], decreasing = TRUE) %>% 
  as.matrix() %>% apply(1, function(x) abs(x)>0.4) %>% which() %>% names()
cor_numvar_sort <- cor_numvar[cor_sortname, cor_sortname]
#相關性視覺化
corrplot(cor_numvar_sort, tl.col="black", tl.pos = "lt")
corrplot.mixed(cor_numvar_sort, tl.col="black", tl.pos = "lt")

#Overall Quality----
grid.arrange(all %>% filter(!(is.na(SalePrice))) %>% ggplot(aes(x=factor(OverallQual), y=SalePrice))+
               geom_boxplot(col='dodgerblue2') + labs(x='Overall Quality') +
               scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma),
             all %>% filter(!(is.na(SalePrice))) %>% ggplot(aes(x=OverallQual, y=SalePrice))+
               geom_point(col='dodgerblue2') + geom_smooth(method = "lm", se=FALSE, color="deeppink3") +
               scale_x_continuous(breaks = seq(1,10,1))+
               scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma) 
             )



#Above Grade (Ground) Living Area (square feet)
all %>% filter(!(is.na(SalePrice))) %>% 
  ggplot(aes(x=GrLivArea, y=SalePrice))+
  geom_point(col='dodgerblue2') + 
  geom_smooth(method = "lm", se=FALSE, color="deeppink3") +
  scale_y_continuous(breaks= seq(0, 800000, by=100000), 
                     labels = comma) +
  geom_text_repel(aes(label = ifelse(all$GrLivArea[!is.na(all$SalePrice)]>4500, rownames(all), "")))
#觀察outlier
all[c(524, 1299), c('SalePrice', 'GrLivArea', 'OverallQual')]
#觀察NA值
all[is.na(all) %>% colSums %>% ">"(0) %>% which] %>% sapply(is.na) %>% 
  colSums() %>% sort(decreasing = TRUE) %>% as.data.frame()
###SalePrice有1459個NA值，那是測試資料集
#pool variable----
names(all)[names(all) %>% str_detect("Pool") %>% which]
##將NA轉換為None
all$PoolQC[is.na(all$PoolQC)] <- 'None'
all$PoolQC<-as.integer(plyr::revalue(all$PoolQC,
                                     c('None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3,
                                       'Gd' = 4, 'Ex' = 5)))
table(all$PoolQC)
all[all$PoolQC != 0,c("SalePrice","PoolQC","PoolArea","OverallQual")]

all %>% filter(!(is.na(SalePrice)) & PoolQC != 0) %>% 
  ggplot(aes(x = OverallQual, y = SalePrice ,color = as.factor(PoolQC) ,alpha = PoolArea)) +
  geom_point(size = 5)

#有另一個變數和PoolQC有關,就是PoolArea variable (in square feet),檢查一下發現有三間房子沒有Pool
#但是卻有PoolArea的數據
table(all$PoolArea != 0 )
all[all$PoolArea > 0 & all$PoolQC=="no", c('SalePrice','PoolArea', 'PoolQC', 'OverallQual')]
#觀察PoolAre>0和PoolQC和OverallQual間的關係
all[all$PoolArea > 0 , c('PoolArea', 'PoolQC', 'OverallQual')]
all[all$PoolArea > 0,] %>% ggplot(aes(x = OverallQual,y = PoolArea,color = PoolQC)) + 
  geom_point(size = 5) +
  geom_text_repel(aes(label = ifelse(all[all$PoolArea > 0,"PoolArea"]> 0,
                                     rownames(all[all$PoolArea > 0,]),
                                     "")))
#根據圖形填為Fa
all$PoolQC[2421] <- 2
all$PoolQC[2504] <- 2
all$PoolQC[2600] <- 2

#Miscellaneous Feature----
names(all)[names(all) %>% str_detect("Mis") %>% which]
all$MiscFeature[is.na(all$MiscFeature)] <- 'None'
all$MiscFeature <- as.factor(all$MiscFeature)
all %>% filter(!(is.na(SalePrice))) %>% 
  ggplot(aes(x=MiscFeature, y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "median", fill='blue') +
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..))

#全部的次數
table(all$MiscFeature)
#價值為0的MiscFeature
all %>% filter(!(MiscFeature %in% "None") & MiscVal == 0) %>% 
  select("Id","MiscVal","MiscFeature")
#Alley----
all$Alley[is.na(all$Alley)] <- 'None'
all$Alley %<>% as.factor()

all %>% filter(!(is.na(SalePrice))) %>% 
  ggplot(aes(x=Alley, y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "median", fill='dodgerblue2')+
  scale_y_continuous(breaks= seq(0, 200000, by=50000), labels = comma) + 
  geom_label(stat = "count",aes(label = ..count..,y = ..count..))
table(all$Alley)
#Fence----
all$Fence[is.na(all$Fence)] <- 'None'
table(all$Fence)
all %>% filter(!(is.na(SalePrice))) %>%
  ggplot(aes(x=Fence, y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "median", fill='dodgerblue2')+
  scale_y_continuous(labels = comma) + 
  geom_label(stat = "count",aes(label = ..count..,y = ..count..))

all$Fence <- as.factor(all$Fence)
#Fireplace----
names(all)[names(all) %>% str_detect("Fireplace") %>% which]
all$FireplaceQu[is.na(all$FireplaceQu)] <- 'None'
all$FireplaceQu<-as.integer(plyr::revalue(all$FireplaceQu,
                                          c('None' = 0, 'Po' = 1, 
                                            'Fa' = 2, 'TA' = 3,
                                            'Gd' = 4, 'Ex' = 5)))
table(all$FireplaceQu)
table(all$Fireplaces)

#Lot----
#LotFrontage: Linear feet of street connected to property
#房地產走到街上的直線步數
#486NA,這邊用同地區LotFrontage的中位數來填補這些NA
names(all)[names(all) %>% str_detect("Lot") %>% which]
ggplot(all, aes(x=as.factor(Neighborhood), y=LotFrontage)) +
  geom_bar(stat='summary', fun.y = "median", fill='dodgerblue2') +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major=element_line(colour=NA)) 

#原文利用迴圈進行填補
#for (i in 1:nrow(all)){
#  if(is.na(all$LotFrontage[i])){
#    all$LotFrontage[i] <- as.integer(median(all$LotFrontage[all$Neighborhood==all$Neighborhood[i]], na.rm=TRUE)) 
#  }
#}

#利用dplyr結合ifelse進行向量式填補
all <- all %>% group_by(Neighborhood) %>% 
          mutate(LotFrontage = ifelse(is.na(LotFrontage),
                                      median(LotFrontage,na.rm = T),
                                      LotFrontage)) %>% as.data.frame()
#dplyr 測試時間0.00-0.02
#b <- all
#system.time(
#b <- b %>% group_by(Neighborhood) %>% 
#        mutate(LotFrontage = ifelse(is.na(LotFrontage),
#                                    median(LotFrontage,na.rm = T),
#                                    LotFrontage))
#)
#rm(b)
#迴圈 測試時間0.06-0.08
#b <- all
#system.time(
#for (i in 1:nrow(b)){
#  if(is.na(b$LotFrontage[i])){
#    b$LotFrontage[i] <- as.integer(median(b$LotFrontage[b$Neighborhood==b$Neighborhood[i]], na.rm=TRUE)) 
#  }
#}
#)
#rm(b)

#LotShape: General shape of property
#No NAs. Values seem ordinal (Regular=best)
table(all$LotShape)
#將其轉換為數字

all$LotShape<-as.integer(revalue(all$LotShape, c('IR3'=0, 'IR2'=1, 'IR1'=2, 'Reg'=3)))
table(all$LotShape)

#LotConfig: Lot configuration
#No NAs. The values seemed possibly ordinal to me, but the visualization does not show this. Therefore, I will convert the variable into a factor.

ggplot(all, aes(x=as.factor(LotConfig), y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "median", fill='blue')+
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..))

all$LotConfig <- as.factor(all$LotConfig)
table(all$LotConfig)


#Garage---- 
#GarageYrBlt: Year garage was built 
#將車庫建造年的NA值改成YearBuilt的年份,NA值有可能是因為該房子沒有車庫
#將車庫建造年的NA值改成YearBuilt的年份的主要原因類似於YearRemodAdd
#在說明文件裡面YearRemodAdd: Remodel date (same as construction date if no remodeling or additions)
#如果房子沒有改建過YearRemodAdd則跟YearBuilt一樣
all$GarageYrBlt[is.na(all$GarageYrBlt)] <- all$YearBuilt[is.na(all$GarageYrBlt)]
#找尋2個特別的NA
all[!is.na(all$GarageType) & is.na(all$GarageFinish),
    c('GarageCars', 'GarageArea', 'GarageType', 'GarageCond', 'GarageQual', 'GarageFinish')]
#看起來2127似乎是有車庫,2577沒有,那就把2577改成沒有車庫,2127的GarageCond	GarageQual	GarageFinish
#的三個NA值改成該變數最多的值
all$GarageCond[2127] <- names(sort(-table(all$GarageCond)))[1]
all$GarageQual[2127] <- names(sort(-table(all$GarageQual)))[1]
all$GarageFinish[2127] <- names(sort(-table(all$GarageFinish)))[1]
all[2127,
    c('GarageYrBlt', 'GarageCars', 'GarageArea',
      'GarageType', 'GarageCond', 'GarageQual', 'GarageFinish')]
#修改2577
all$GarageCars[2577] <- 0
all$GarageArea[2577] <- 0
all$GarageType[2577] <- NA
#GarageType: Garage location
all$GarageType[is.na(all$GarageType)] <- 'No Garage'
all$GarageType <- as.factor(all$GarageType)
table(all$GarageType)
#GarageFinish: Interior finish of the garage
all$GarageFinish[is.na(all$GarageFinish)] <- 'None'
Finish <- c('None'=0, 'Unf'=1, 'RFn'=2, 'Fin'=3)
all$GarageFinish<-as.integer(revalue(all$GarageFinish, Finish))
table(all$GarageFinish)
#GarageQual: Garage quality
all$GarageQual[is.na(all$GarageQual)] <- 'None'
all$GarageQual<-as.integer(revalue(all$GarageQual, c('None' = 0, 'Po' = 1, 'Fa' = 2,
                                                     'TA' = 3,'Gd' = 4, 'Ex' = 5)))
table(all$GarageQual)

#GarageCond: Garage condition
all$GarageCond[is.na(all$GarageCond)] <- 'None'
all$GarageCond<-as.integer(revalue(all$GarageCond, c('None' = 0, 'Po' = 1, 'Fa' = 2,
                                                     'TA' = 3,'Gd' = 4, 'Ex' = 5)))
table(all$GarageCond)

#Basement----
#Altogether, there are 11 variables that relate to the Basement of a house
#五個變數有79-82NA,六個有1-2NA
#檢查五個79-82NA變數是否一致
length(which(is.na(all$BsmtQual) & is.na(all$BsmtCond) & 
             is.na(all$BsmtExposure) & is.na(all$BsmtFinType1) & 
             is.na(all$BsmtFinType2)))

#可見會有BsmtFinType1不是NA,而其他四個變數則有NA得情況
#找出額外的NA
all[!is.na(all$BsmtFinType1) & 
    (is.na(all$BsmtCond)|is.na(all$BsmtQual)|is.na(all$BsmtExposure)|is.na(all$BsmtFinType2)),
    c('BsmtQual', 'BsmtCond', 'BsmtExposure', 'BsmtFinType1', 'BsmtFinType2')]
#一樣將缺失值用該變數最多的值來填補
all$BsmtFinType2[333] <- names(sort(-table(all$BsmtFinType2)))[1]
all$BsmtExposure[c(949, 1488, 2349)] <- names(sort(-table(all$BsmtExposure)))[1]
all$BsmtCond[c(2041, 2186, 2525)] <- names(sort(-table(all$BsmtCond)))[1]
all$BsmtQual[c(2218, 2219)] <- names(sort(-table(all$BsmtQual)))[1]

#BsmtQual: Evaluates the height of the basement
all$BsmtQual[is.na(all$BsmtQual)] <- 'None'
all$BsmtQual<-as.integer(revalue(all$BsmtQual, c('None' = 0, 'Po' = 1, 'Fa' = 2,
                                                 'TA' = 3,'Gd' = 4, 'Ex' = 5)))
table(all$BsmtQual)

#BsmtCond: Evaluates the general condition of the basement
all$BsmtCond[is.na(all$BsmtCond)] <- 'None'
all$BsmtCond<-as.integer(revalue(all$BsmtCond, c('None' = 0, 'Po' = 1, 'Fa' = 2,
                                                 'TA' = 3,'Gd' = 4, 'Ex' = 5)))
table(all$BsmtCond)

#BsmtExposure: Refers to walkout or garden level walls
all$BsmtExposure[is.na(all$BsmtExposure)] <- 'None'
all$BsmtExposure<-as.integer(revalue(all$BsmtExposure, c('None'=0, 'No'=1, 'Mn'=2,
                                                         'Av'=3, 'Gd'=4)))
table(all$BsmtExposure)

#BsmtFinType1: Rating of basement finished area
all$BsmtFinType1[is.na(all$BsmtFinType1)] <- 'None'
all$BsmtFinType1<-as.integer(revalue(all$BsmtFinType1, c('None'=0, 'Unf'=1, 'LwQ'=2,
                                                         'Rec'=3, 'BLQ'=4, 'ALQ'=5,
                                                         'GLQ'=6)))
table(all$BsmtFinType1)

#BsmtFinType2: Rating of basement finished area (if multiple types)
all$BsmtFinType2[is.na(all$BsmtFinType2)] <- 'None'
all$BsmtFinType2<-as.integer(revalue(all$BsmtFinType2, c('None'=0, 'Unf'=1, 'LwQ'=2,
                                                         'Rec'=3, 'BLQ'=4, 'ALQ'=5,
                                                         'GLQ'=6)))
table(all$BsmtFinType2)

#剩餘六個1-2NA的變數
all[(is.na(all$BsmtFullBath) | is.na(all$BsmtHalfBath) |
     is.na(all$BsmtFinSF1) | is.na(all$BsmtFinSF2)|
     is.na(all$BsmtUnfSF) | is.na(all$TotalBsmtSF)),
    c('BsmtQual', 'BsmtFullBath', 'BsmtHalfBath', 'BsmtFinSF1',
      'BsmtFinSF2', 'BsmtUnfSF', 'TotalBsmtSF')]
#顯然剩餘含有NA的資料都是不含有地下室

#BsmtFullBath: Basement full bathrooms
all$BsmtFullBath[is.na(all$BsmtFullBath)] <-0
table(all$BsmtFullBath)

#BsmtHalfBath: Basement half bathrooms
all$BsmtHalfBath[is.na(all$BsmtHalfBath)] <-0
table(all$BsmtHalfBath)

#BsmtFinSF1: Type 1 finished square feet
all$BsmtFinSF1[is.na(all$BsmtFinSF1)] <-0

#BsmtFinSF2: Type 2 finished square feet
all$BsmtFinSF2[is.na(all$BsmtFinSF2)] <-0

#BsmtUnfSF: Unfinished square feet of basement area
all$BsmtUnfSF[is.na(all$BsmtUnfSF)] <-0

#TotalBsmtSF: Total square feet of basement area
all$TotalBsmtSF[is.na(all$TotalBsmtSF)] <-0

















#Masonry ----
#Masonry veneer type, and masonry veneer area
#Masonry veneer type 有24 NA. Masonry veneer area 有23 NA
#通常房子有 veneer area,應該也要有 masonry veneer type
length(which(is.na(all$MasVnrType) & is.na(all$MasVnrArea)))
all[is.na(all$MasVnrType) & !is.na(all$MasVnrArea), c('MasVnrType', 'MasVnrArea')]
#選擇用MasVnrType最多的因子來填補,但是2611列AREA有值,最多的因子是none,所以用第二多的來填補
all$MasVnrType[2611] <- names(sort(-table(all$MasVnrType)))[2]

#Masonry veneer type
all$MasVnrType[is.na(all$MasVnrType)] <- 'None'

grid.arrange(
  ggplot(all, aes(x=MasVnrType, y=SalePrice)) +
    geom_bar(stat='summary', fun.y = "median", fill='grey')+
    scale_y_continuous(labels = comma) + 
    geom_label(stat = "count",aes(label = ..count..,y = ..count..)) + 
    theme_bw() + 
    ggtitle("Median"),
  ggplot(all, aes(x=MasVnrType, y=SalePrice)) +
    geom_bar(stat='summary', fun.y = "mean", fill='black')+
    scale_y_continuous(labels = comma) + 
    geom_label(stat = "count",aes(label = ..count..,y = ..count..)) + 
    theme_bw() + 
    ggtitle("Mean"),
  nrow = 2 ,ncol = 1
)
#從平均和中位數來看,Brick Common和None似乎在銷售價格上很接近,我們假設普通石造牆壁和木製一樣便宜
#那這樣我們就把BrkCmn和None合併起來,這樣我們就能把MasVnrType變數弄成次序的

all$MasVnrType<-as.integer(revalue(all$MasVnrType, c('None'=0, 'BrkCmn'=0,
                                                     'BrkFace'=1, 'Stone'=2)))
table(all$MasVnrType)

#MasVnrArea: Masonry veneer area in square feet
all$MasVnrArea[is.na(all$MasVnrArea)] <-0



#MSZonling----
#MSZoning: Identifies the general zoning classification of the sale
all[all$MSZoning %>% is.na(),] %>% View()
#這個變數原文是直接將4NA直接用最多的因子去填補,但是呢我覺得有一點點不妥
#因為這個變數是土地的分區,有商業區農業區工業區等等,我覺得應該跟Neighborhood有關連
#所以這邊我會先依照原文的跑一次,我自己分得再跑一次看看結果差異如何
#原文
all$MSZoning[is.na(all$MSZoning)] <- names(sort(-table(all$MSZoning)))[1]
all$MSZoning <- as.factor(all$MSZoning)
table(all$MSZoning)
#MINE
all.test[all.test$MSZoning %>% is.na(),c('MSZoning',"Neighborhood")]
#4個NA有三個是IDOTRR區一個是Mitchel區
#IDOTRR
filter(all.test,Neighborhood == "IDOTRR")$MSZoning %>% table
#Mitchel
filter(all.test,Neighborhood == "Mitchel")$MSZoning %>% table
#因此這邊1916 2217 2251 將用RM填補,2905將用RL填補
all.test[c(1916,2217,2251),"MSZoning"] <- "RM"
all.test[2905,"MSZoning"] <- "RL"

#Kitchen----
#Kitchen quality 1NA
all$KitchenQual[is.na(all$KitchenQual)] <- 'TA' #replace with most common value
all$KitchenQual<-as.integer(revalue(all$KitchenQual, c('None' = 0, 'Po' = 1, 'Fa' = 2,
                                                       'TA' = 3,'Gd' = 4, 'Ex' = 5)))
table(all$KitchenQual)

#Number of Kitchens above grade
table(all$KitchenAbvGr)

#Utilities----
#Utilities: Type of utilities available 2NA
table(all$Utilities)
#變數因子嚴重不平衡,這變數對預測沒甚麼幫助,所以就剔除掉
all$Utilities <- NULL

#Functional: Home functionality----
#1NA
all$Functional[is.na(all$Functional)] <- names(sort(-table(all$Functional)))[1]
#原文覺得可以轉成次序
all$Functional <- as.integer(revalue(all$Functional, c('Sal'=0, 'Sev'=1,
                                                       'Maj2'=2, 'Maj1'=3,
                                                       'Mod'=4, 'Min2'=5,
                                                       'Min1'=6, 'Typ'=7)))
table(all$Functional)
#Exterior ----
#2 variables have 1 NA, 2 variables have no NAs.
#Exterior1st: Exterior covering on house;1NA. Values are categorical.
all$Exterior1st[is.na(all$Exterior1st)] <- names(sort(-table(all$Exterior1st)))[1]
all$Exterior1st <- as.factor(all$Exterior1st)
table(all$Exterior1st)

#Exterior2nd: Exterior covering on house (if more than one material)
all$Exterior2nd[is.na(all$Exterior2nd)] <- names(sort(-table(all$Exterior2nd)))[1]
all$Exterior2nd <- as.factor(all$Exterior2nd)
table(all$Exterior2nd)

#ExterQual: Evaluates the quality of the material on the exterior
all$ExterQual<-as.integer(revalue(all$ExterQual, c('None' = 0, 'Po' = 1, 'Fa' = 2,
                                                   'TA' = 3,'Gd' = 4, 'Ex' = 5)))
table(all$ExterQual)

#ExterCond: Evaluates the present condition of the material on the exterior
all$ExterCond<-as.integer(revalue(all$ExterCond, c('None' = 0, 'Po' = 1, 'Fa' = 2,
                                                   'TA' = 3,'Gd' = 4, 'Ex' = 5)))
table(all$ExterCond)







#Electrical: Electrical system----
#1 NA. Values are categorical.
all$Electrical[is.na(all$Electrical)] <- names(sort(-table(all$Electrical)))[1]
all$Electrical <- as.factor(all$Electrical)
table(all$Electrical)




#Sale Type and Condition----
#SaleType: Type of sale ;1 NA. Values are categorical.
all$SaleType[is.na(all$SaleType)] <- names(sort(-table(all$SaleType)))[1]
all$SaleType <- as.factor(all$SaleType)
table(all$SaleType)
#SaleCondition: Condition of sale;No NAs. Values are categorical.
all$SaleCondition <- as.factor(all$SaleCondition)
table(all$SaleCondition)
#處理剩餘字串變數----
Charcol <- names(all[,sapply(all, is.character)])
Charcol
#Foundation: Type of foundation----
all$Foundation <- as.factor(all$Foundation)
table(all$Foundation)
#Heating and airco----
#Heating: Type of heating
all$Heating <- as.factor(all$Heating)
table(all$Heating)
#HeatingQC: Heating quality and condition
all$HeatingQC<-as.integer(revalue(all$HeatingQC, c('None' = 0, 'Po' = 1, 'Fa' = 2,
                                                   'TA' = 3,'Gd' = 4, 'Ex' = 5)))
table(all$HeatingQC)

#CentralAir: Central air conditioning
all$CentralAir<-as.integer(revalue(all$CentralAir, c('N'=0, 'Y'=1)))
table(all$CentralAir)





#Roof----
#RoofStyle: Type of roof
all$RoofStyle <- as.factor(all$RoofStyle)
table(all$RoofStyle)
#RoofMatl: Roof material
all$RoofMatl <- as.factor(all$RoofMatl)
table(all$RoofMatl)

#Land----
#LandContour: Flatness of the property
all$LandContour <- as.factor(all$LandContour)
table(all$LandContour)
#LandSlope: Slope of property
#Ordinal, so label encoding
all$LandSlope<-as.integer(revalue(all$LandSlope, c('Sev'=0, 'Mod'=1, 'Gtl'=2)))
table(all$LandSlope)
#Dwelling----
#BldgType: Type of dwelling
ggplot(all, aes(x=as.factor(BldgType), y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "median", fill='blue')+
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..))
#No ordinality, so converting into factors
all$BldgType <- as.factor(all$BldgType)
table(all$BldgType)
#HouseStyle: Style of dwelling
ggplot(all, aes(x=as.factor(HouseStyle), y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "median", fill='blue')+
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..))
#No ordinality, so converting into factors
all$HouseStyle <- as.factor(all$HouseStyle)
table(all$HouseStyle)
#Neighborhood and Conditions----
#3 variables that specify the physical location, and the proximity of ‘conditions’.
#Neighborhood: Physical locations within Ames city limits
all$Neighborhood <- as.factor(all$Neighborhood)
table(all$Neighborhood)
#Condition1: Proximity to various conditions
all$Condition1 <- as.factor(all$Condition1)
table(all$Condition1)
#Condition2: Proximity to various conditions (if more than one is present)
all$Condition2 <- as.factor(all$Condition2)
table(all$Condition2)





#Pavement of Street & Driveway----
#Street: Type of road access to property
all$Street<-as.integer(revalue(all$Street, c('Grvl'=0, 'Pave'=1)))
table(all$Street)
#PavedDrive: Paved driveway
all$PavedDrive<-as.integer(revalue(all$PavedDrive, c('N'=0, 'P'=1, 'Y'=2)))
table(all$PavedDrive)

#將一些數值變數轉成類別變數----
all$MoSold <- as.factor(all$MoSold)
grid.arrange(
  ggplot(all[!is.na(all$SalePrice),], aes(x=as.factor(YrSold), y=SalePrice)) +
    geom_bar(stat='summary', fun.y = "median", fill='blue')+
    scale_y_continuous(breaks= seq(0, 800000, by=25000), labels = comma) +
    geom_label(stat = "count", aes(label = ..count.., y = ..count..)) +
    coord_cartesian(ylim = c(0, 200000)) +
    geom_hline(yintercept=163000, linetype="dashed", color = "red"), #dashed line is median SalePrice
  ggplot(all[!is.na(all$SalePrice),], aes(x=MoSold, y=SalePrice)) +
    geom_bar(stat='summary', fun.y = "median", fill='blue')+
    scale_y_continuous(breaks= seq(0, 800000, by=25000), labels = comma) +
    geom_label(stat = "count", aes(label = ..count.., y = ..count..)) +
    coord_cartesian(ylim = c(0, 200000)) +
    geom_hline(yintercept=163000, linetype="dashed", color = "red"), #dashed line is median SalePrice
  widths=c(1,2)
  )
#MSSubClass
#MSSubClass: Identifies the type of dwelling involved in the sale.
all$MSSubClass <- as.factor(all$MSSubClass)

#revalue for better readability
all$MSSubClass<-revalue(all$MSSubClass, c('20'='1 story 1946+', '30'='1 story 1945-',
                                          '40'='1 story unf attic', '45'='1,5 story unf',
                                          '50'='1,5 story fin', '60'='2 story 1946+',
                                          '70'='2 story 1945-', '75'='2,5 story all ages',
                                          '80'='split/multi level', '85'='split foyer',
                                          '90'='duplex all style/age',
                                          '120'='1 story PUD 1946+',
                                          '150'='1,5 story PUD all',
                                          '160'='2 story PUD 1946+',
                                          '180'='PUD multilevel',
                                          '190'='2 family conversion'))




#視覺化一些變數----
numericVars <- which(sapply(all, is.numeric)) #index vector numeric variables
factorVars <- which(sapply(all, is.factor)) #index vector factor variables
cat('There are', length(numericVars), 'numeric variables, and',
    length(factorVars), 'categoric variables')
#相關性
cor_numVar <- cor(all[, numericVars], use="pairwise.complete.obs") #correlations of all numeric variables
#sort on decreasing correlations with SalePrice
CorHigh <- as.matrix(sort(cor_numVar[,'SalePrice'], decreasing = TRUE)) %>% 
            apply(1, function(x) abs(x)>0.5) %>% which %>% names
#select only high corelations
cor_numVar <- cor_numVar[CorHigh, CorHigh]
corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt", tl.cex = 0.7,cl.cex = .7, number.cex=.7)
#Finding variable importance with a quick Random Forest
set.seed(2018)
#前1460個會test data ,79欄為saleprice
quick_RF <- randomForest(x=all[1:1460,-79], y=all$SalePrice[1:1460], ntree=100,importance=TRUE)
imp_RF <- importance(quick_RF)
imp_DF <- data.frame(Variables = row.names(imp_RF), MSE = imp_RF[,1])
imp_DF <- imp_DF[order(imp_DF$MSE, decreasing = TRUE),]

ggplot(imp_DF[1:20,], aes(x=reorder(Variables, MSE), y=MSE, fill=MSE)) + 
  geom_bar(stat = 'identity') + 
  labs(x = 'Variables', y= '% increase MSE if variable is randomly permuted') + 
  coord_flip() + theme(legend.position="none")
#Above Ground Living Area, and other surface related variables (in square feet)
grid.arrange(ggplot(data= all, aes(x=GrLivArea)) +
               geom_density() + labs(x='Square feet living area'), 
             ggplot(data=all, aes(x=as.factor(TotRmsAbvGrd))) +
               geom_histogram(stat='count') + labs(x='Rooms above Ground'),
             ggplot(data= all, aes(x=X1stFlrSF)) +
               geom_density() + labs(x='Square feet first floor'),
             ggplot(data= all, aes(x=X2ndFlrSF)) +
               geom_density() + labs(x='Square feet second floor'), 
             ggplot(data= all, aes(x=TotalBsmtSF)) +
               geom_density() + labs(x='Square feet basement'),
             ggplot(data= all[all$LotArea<100000,], aes(x=LotArea)) +
               geom_density() + labs(x='Square feet lot'), 
             ggplot(data= all, aes(x=LotFrontage)) +
               geom_density() + labs(x='Linear feet lot frontage'), 
             ggplot(data= all, aes(x=LowQualFinSF)) +
               geom_histogram() + labs(x='Low quality square feet 1st & 2nd'), 
             layout_matrix = matrix(c(1,2,5,3,4,8,6,7),4,2,byrow=TRUE))
#GrLivArea貌似為X1stFlrSF X2ndFlrSF LowQualFinSF加總起來
cor(all$GrLivArea, (all$X1stFlrSF + all$X2ndFlrSF + all$LowQualFinSF))
head(all[all$LowQualFinSF>0, c('GrLivArea', 'X1stFlrSF', 'X2ndFlrSF', 'LowQualFinSF')])

#The most important categorical variable; Neighborhood
grid.arrange(ggplot(all[!is.na(all$SalePrice),], aes(x=Neighborhood, y=SalePrice)) +
               geom_bar(stat='summary', fun.y = "median", fill='blue') +
               theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
               scale_y_continuous(breaks= seq(0, 800000, by=50000), labels = comma) +
               geom_label(stat = "count", aes(label = ..count.., y = ..count..), size=3) +
               geom_hline(yintercept=163000, linetype="dashed", color = "red"), #dashed line is median SalePrice 
             ggplot(data=all, aes(x=Neighborhood)) +
               geom_histogram(stat='count')+
               geom_label(stat = "count", aes(label = ..count.., y = ..count..), size=3)+
               theme(axis.text.x = element_text(angle = 45, hjust = 1)))

#Overall Quality, and other Quality variables

grid.arrange(ggplot(data=all, aes(x=as.factor(OverallQual))) +
               geom_histogram(stat='count'),
             ggplot(data=all, aes(x=as.factor(ExterQual))) +
               geom_histogram(stat='count'),
             ggplot(data=all, aes(x=as.factor(BsmtQual))) +
               geom_histogram(stat='count'),
             ggplot(data=all, aes(x=as.factor(KitchenQual))) +
               geom_histogram(stat='count'),
             ggplot(data=all, aes(x=as.factor(GarageQual))) +
               geom_histogram(stat='count'),
             ggplot(data=all, aes(x=as.factor(FireplaceQu))) +
               geom_histogram(stat='count'),
             ggplot(data=all, aes(x=as.factor(PoolQC))) +
               geom_histogram(stat='count'),
             layout_matrix = matrix(c(1,2,8,3,4,8,5,6,7),3,3,byrow=TRUE)
  
)

#The second most important categorical variable; MSSubClass

grid.arrange(ggplot(all[!is.na(all$SalePrice),], aes(x=MSSubClass, y=SalePrice)) +
               geom_bar(stat='summary', fun.y = "median", fill='blue') +
               theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
               scale_y_continuous(breaks= seq(0, 800000, by=50000), labels = comma) +
               geom_label(stat = "count", aes(label = ..count.., y = ..count..), size=3) +
               geom_hline(yintercept=163000, linetype="dashed", color = "red") ,#dashed line is median SalePrice
             ggplot(data=all, aes(x=MSSubClass)) +
               geom_histogram(stat='count')+
               geom_label(stat = "count", aes(label = ..count.., y = ..count..), size=3) +
               theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
)

#Garage variables
#correct error
all$GarageYrBlt[2593] <- 2007 #this must have been a typo. GarageYrBlt=2207, YearBuilt=2006, YearRemodAdd=2007
grid.arrange(ggplot(data=all[all$GarageCars !=0,], aes(x=GarageYrBlt)) +
               geom_histogram(),
             ggplot(data=all, aes(x=as.factor(GarageCars))) +
               geom_histogram(stat='count'),
             ggplot(data= all, aes(x=GarageArea)) +
               geom_density(),
             ggplot(data=all, aes(x=as.factor(GarageCond))) +
               geom_histogram(stat='count'),
             ggplot(data=all, aes(x=GarageType)) +
               geom_histogram(stat='count'),
             ggplot(data=all, aes(x=as.factor(GarageQual))) +
               geom_histogram(stat='count'),
             ggplot(data=all, aes(x=as.factor(GarageFinish))) +
               geom_histogram(stat='count'),
             layout_matrix =matrix(c(1,5,5,2,3,8,6,4,7),3,3,byrow=TRUE)
  
)

#Basement variables
grid.arrange(ggplot(data=all, aes(x=BsmtFinSF1)) +
               geom_histogram() + labs(x='Type 1 finished square feet'),
             ggplot(data=all, aes(x=BsmtFinSF2)) +
               geom_histogram()+ labs(x='Type 2 finished square feet'),
             ggplot(data=all, aes(x=BsmtUnfSF)) +
               geom_histogram()+ labs(x='Unfinished square feet'),
             ggplot(data=all, aes(x=as.factor(BsmtFinType1))) +
               geom_histogram(stat='count')+ labs(x='Rating of Type 1 finished area'),
             ggplot(data=all, aes(x=as.factor(BsmtFinType2))) +
               geom_histogram(stat='count')+ labs(x='Rating of Type 2 finished area'),
             ggplot(data=all, aes(x=as.factor(BsmtQual))) +
               geom_histogram(stat='count')+ labs(x='Height of the basement'),
             ggplot(data=all, aes(x=as.factor(BsmtCond))) +
               geom_histogram(stat='count')+ labs(x='Rating of general condition'),
             ggplot(data=all, aes(x=as.factor(BsmtExposure))) +
               geom_histogram(stat='count')+ labs(x='Walkout or garden level walls'),
             layout_matrix = matrix(c(1,2,3,4,5,9,6,7,8),3,3,byrow=TRUE)
  
)

#Feature engineering----
#total number of bathrooms

all$TotBathrooms <- all$FullBath + (all$HalfBath*0.5) + all$BsmtFullBath + (all$BsmtHalfBath*0.5)

grid.arrange(ggplot(data=all[!is.na(all$SalePrice),], aes(x=as.factor(TotBathrooms), y=SalePrice))+
               geom_point(col='blue') + geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1)) +
               scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma),
             ggplot(data=all, aes(x=as.factor(TotBathrooms))) +
               geom_histogram(stat='count'))
#Adding ‘House Age’, ‘Remodeled (Yes/No)’, and IsNew variables
all$Remod <- ifelse(all$YearBuilt==all$YearRemodAdd, 0, 1) #0=No Remodeling, 1=Remodeling
all$Age <- as.numeric(all$YrSold)-all$YearRemodAdd
ggplot(data=all[!is.na(all$SalePrice),], aes(x=Age, y=SalePrice))+
  geom_point(col='blue') + geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1)) +
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma)

#a negative correlation with Age (old house are worth less).
cor(all$SalePrice[!is.na(all$SalePrice)], all$Age[!is.na(all$SalePrice)])

ggplot(all[!is.na(all$SalePrice),], aes(x=as.factor(Remod), y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "median", fill='blue') +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..), size=6) +
  scale_y_continuous(breaks= seq(0, 800000, by=50000), labels = comma) +
  theme_grey(base_size = 18) +
  geom_hline(yintercept=163000, linetype="dashed") #dashed line is median SalePrice

all$IsNew <- ifelse(all$YrSold==all$YearBuilt, 1, 0)
table(all$IsNew)

ggplot(all[!is.na(all$SalePrice),], aes(x=as.factor(IsNew), y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "median", fill='blue') +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..), size=6) +
  scale_y_continuous(breaks= seq(0, 800000, by=50000), labels = comma) +
  theme_grey(base_size = 18) +
  geom_hline(yintercept=163000, linetype="dashed") #dashed line is median SalePrice

all$YrSold <- as.factor(all$YrSold) #the numeric version is now not needed anymore

#Binning Neighborhood
grid.arrange(ggplot(all[!is.na(all$SalePrice),], aes(x=reorder(Neighborhood, SalePrice, FUN=median), y=SalePrice)) +
               geom_bar(stat='summary', fun.y = "median", fill='blue') + labs(x='Neighborhood', y='Median SalePrice') +
               theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
               scale_y_continuous(breaks= seq(0, 800000, by=50000), labels = comma) +
               geom_label(stat = "count", aes(label = ..count.., y = ..count..), size=3) +
               geom_hline(yintercept=163000, linetype="dashed", color = "red"), #dashed line is median SalePrice
             ggplot(all[!is.na(all$SalePrice),], aes(x=reorder(Neighborhood, SalePrice, FUN=mean), y=SalePrice)) +
               geom_bar(stat='summary', fun.y = "mean", fill='blue') + labs(x='Neighborhood', y="Mean SalePrice") +
               theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
               scale_y_continuous(breaks= seq(0, 800000, by=50000), labels = comma) +
               geom_label(stat = "count", aes(label = ..count.., y = ..count..), size=3) +
               geom_hline(yintercept=163000, linetype="dashed", color = "red")) #dashed line is median SaleP



all$NeighRich[all$Neighborhood %in% c('StoneBr', 'NridgHt', 'NoRidge')] <- 2
all$NeighRich[!all$Neighborhood %in% c('MeadowV', 'IDOTRR', 'BrDale', 'StoneBr', 'NridgHt', 'NoRidge')] <- 1 #介於貧窮以及富有的街區
all$NeighRich[all$Neighborhood %in% c('MeadowV', 'IDOTRR', 'BrDale')] <- 0
table(all$NeighRich)

#Total Square Feet

all$TotalSqFeet <- all$GrLivArea + all$TotalBsmtSF

ggplot(data=all[!is.na(all$SalePrice),], aes(x=TotalSqFeet, y=SalePrice))+
  geom_point(col='blue') + geom_smooth(method = "lm", se=FALSE, color="black") +
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma) +
  geom_text_repel(aes(label = ifelse(all$GrLivArea[!is.na(all$SalePrice)]>4500, rownames(all), '')))


cor(all$SalePrice, all$TotalSqFeet, use= "pairwise.complete.obs")
# taking out two outliers, the correlation increases by 5%
cor(all$SalePrice[-c(524, 1299)], all$TotalSqFeet[-c(524, 1299)], use= "pairwise.complete.obs")

#Consolidating Porch variables
all$TotalPorchSF <- all$OpenPorchSF + all$EnclosedPorch + all$X3SsnPorch + all$ScreenPorch

cor(all$SalePrice, all$TotalPorchSF, use= "pairwise.complete.obs")
ggplot(data=all[!is.na(all$SalePrice),], aes(x=TotalPorchSF, y=SalePrice))+
  geom_point(col='blue') + geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1)) +
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma)

#preparing data for modeling----
#Dropping highly correlated variables
dropVars <- c('YearRemodAdd', 'GarageYrBlt', 'GarageArea', 'GarageCond', 'TotalBsmtSF', 'TotalRmsAbvGrd', 'BsmtFinSF1')

all <- all[,!(names(all) %in% dropVars)]
corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt", tl.cex = 0.7,cl.cex = .7, number.cex=.7)

#Removing outliers
all <- all[-c(524, 1299),]
#PreProcessing predictor variables
numericVarNames <- numericVarNames[!(numericVarNames %in% c('MSSubClass', 'MoSold', 'YrSold', 'SalePrice', 'OverallQual', 'OverallCond'))] #numericVarNames was created before having done anything
numericVarNames <- c(numericVarNames,'Age', 'TotalPorchSF', 'TotBathrooms', 'TotalSqFeet')

#about c() & append()
#https://stackoverflow.com/questions/16144683/difference-between-c-and-append

DFnumeric <- all[, names(all) %in% numericVarNames]

DFfactors <- all[, !(names(all) %in% numericVarNames)]
DFfactors <- DFfactors[, names(DFfactors) != 'SalePrice'] 

cat('There are', length(DFnumeric), 'numeric variables, and', length(DFfactors), 'factor variables')

#Skewness and normalizing of the numeric predictors
# log+1, to avoid division by zero issues
for(i in 1:ncol(DFnumeric)){
  if (abs(skew(DFnumeric[,i]))>0.8){
    DFnumeric[,i] <- log(DFnumeric[,i] +1)
  }
}

PreNum <- preProcess(DFnumeric, method=c("center", "scale")) #caret
print(PreNum)

DFnorm <- predict(PreNum, DFnumeric)
dim(DFnorm)

#One hot encoding the categorical variables
DFdummies <- as.data.frame(model.matrix(~.-1, DFfactors))
dim(DFdummies)

#Removing levels with few or no observations in train or test
ZerocolTest <- which(DFdummies[all$SalePrice %>% is.na %>% which,] %>% colSums() == 0)
colnames(DFdummies[ZerocolTest])
DFdummies <- DFdummies[,-ZerocolTest] #removing predictors

ZerocolTrain <- which(DFdummies[which(!(all$SalePrice %>% is.na)),] %>% colSums() == 0)
colnames(DFdummies[ZerocolTrain])

DFdummies <- DFdummies[,-ZerocolTrain] #removing predictor
# taking out variables with less than 10 ‘ones’ in the train set.
fewOnes <- which(DFdummies[which(!(all$SalePrice %>% is.na)),] %>% colSums() <10)
colnames(DFdummies[fewOnes])
DFdummies <- DFdummies[,-fewOnes] #removing predictors
dim(DFdummies)
combined <- cbind(DFnorm, DFdummies) #combining all (now numeric) predictors into one dataframe 

# Dealing with skewness of response variable
skew(all$SalePrice)
qqnorm(all$SalePrice)
qqline(all$SalePrice)

all$SalePrice <- log(all$SalePrice) #default is the natural logarithm, "+1" is not necessary as there are no 0's
skew(all$SalePrice)
qqnorm(all$SalePrice)
qqline(all$SalePrice)
#做做看有處理價格跟沒處理價格的準確率
#Composing train and test sets
train1 <- combined[!is.na(all$SalePrice),]
test1 <- combined[is.na(all$SalePrice),]

#Modeling
# Lasso regression model
set.seed(27042018)
my_control <-trainControl(method="cv", number=5)
lassoGrid <- expand.grid(alpha = 1, lambda = seq(0.001,0.1,by = 0.0005))

lasso_mod <- train(x=train1, y=all$SalePrice[!is.na(all$SalePrice)], method='glmnet', trControl= my_control, tuneGrid=lassoGrid) 
lasso_mod$bestTune
min(lasso_mod$results$RMSE)

lassoVarImp <- varImp(lasso_mod,scale=F)
lassoImportance <- lassoVarImp$importance

varsSelected <- length(which(lassoImportance$Overall!=0))
varsNotSelected <- length(which(lassoImportance$Overall==0))

cat('Lasso uses', varsSelected, 'variables in its model, and did not select', varsNotSelected, 'variables.')
LassoPred <- predict(lasso_mod, test1)
predictions_lasso <- exp(LassoPred) #need to reverse the log to the real values
head(predictions_lasso)
#XGBoost model
xgb_grid = expand.grid(
  nrounds = 1000,
  eta = c(0.1, 0.05, 0.01),
  max_depth = c(2, 3, 4, 5, 6),
  gamma = 0,
  colsample_bytree=1,
  min_child_weight=c(1, 2, 3, 4 ,5),
  subsample=1
)
#expand.grid 根據給定的值產生全部的資料組合
# xgb_caret <- train(x=train1,
#                    y=all$SalePrice[!is.na(all$SalePrice)], 
#                    method='xgbTree', trControl= my_control, 
#                    tuneGrid=xgb_grid) 
#xgb_caret$bestTune

label_train <- all$SalePrice[!is.na(all$SalePrice)]

# put our testing & training data into two seperates Dmatrixs objects
dtrain <- xgb.DMatrix(data = as.matrix(train1), label= label_train)
dtest <- xgb.DMatrix(data = as.matrix(test1))

default_param<-list(
  objective = "reg:linear",
  booster = "gbtree",
  eta=0.05, #default = 0.3
  gamma=0,
  max_depth=3, #default=6
  min_child_weight=4, #default=1
  subsample=1,
  colsample_bytree=1
)


set.seed(27042018)
xgbcv <- xgb.cv(params = default_param,
                data = dtrain,
                nrounds = 1000,
                nfold = 10, showsd = T,
                stratified = T,
                print_every_n = 40,
                maximize = F)
xgbcv$evaluation_log$test_rmse_mean %>% which.min()
xgbcv$evaluation_log$test_rmse_mean %>% min();

#train the model using the best iteration found by cross validation
set.seed(27042018)
xgb_mod <- xgb.train(data = dtrain, params=default_param, nrounds = 645)
XGBpred <- predict(xgb_mod, dtest)
predictions_XGB <- exp(XGBpred) #need to reverse the log to the real values
head(predictions_XGB)
xgb.plot.tree(model = xgb_mod)

#view variable importance plot
library(Ckmeans.1d.dp) #required for ggplot clustering
mat <- xgb.importance (feature_names = colnames(train1),model = xgb_mod)
xgb.ggplot.importance(importance_matrix = mat[1:30], rel_to_first = TRUE)



sub_avg <- data.frame(Id = test_labels, 
                      SalePrice = (predictions_XGB+2*predictions_lasso)/3)
sub_lasso <- data.frame(Id = test_labels, 
                      SalePrice = predictions_lasso)
sub_xgb <- data.frame(Id = test_labels, 
                        SalePrice = predictions_XGB)

head(sub_avg)
write.csv(sub_avg, file = 'housesale.csv', row.names = F)
write.csv(sub_lasso, file = 'housesale_lasso.csv', row.names = F)
write.csv(sub_xgb, file = 'housesale_xgb.csv', row.names = F)


a <- read.csv("a.csv",header = T)
