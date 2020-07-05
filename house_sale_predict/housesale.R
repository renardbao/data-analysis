library(knitr)
library(ggplot2)
library(dplyr)
library(reshape2) #melt dcast 寬長資料互轉
library(corrplot)
library(caret)
library(gridExtra)
library(scales)
#library(Rmisc)
library(ggrepel)
library(randomForest)
library(psych)
library(xgboost)
library(magrittr)
library(stringr)
library(Cairo)
library(showtext) #ggplot 中文問題
source('function.R')
showtext_auto()

options(scipen = 999)
setwd("D:/r_project/data-analysis/house_sale_predict")
train <- read.csv("data/train.csv", stringsAsFactors = F)
test <- read.csv("data/test.csv", stringsAsFactors = F)
test_labels <- test$Id
test$SalePrice <- NA
all <- rbind(train, test)
dim(all)
#大致觀察變數
str(all)

#將是數值的變數名挑出來
num_var <-  sapply(all, is.numeric) %>% which %>% names()
#將是類別的變數名挑出來
char_var <- sapply(all, is.character) %>% which %>% names()
cat("數值變數有",length(num_var),"個",",類別變數有",length(char_var),"個")

#觀察目標變數的分布----
all %>% filter(!(is.na(SalePrice))) %>% 
  ggplot(aes(x = SalePrice)) + 
  geom_histogram(fill="deeppink2",
                 binwidth = 10000) +
  scale_x_continuous(breaks= seq(0, 800000, by=100000),
                     labels = comma)+
  theme_bw() + #去掉背景色
  theme(panel.grid=element_blank(),  #去掉網線
        panel.border=element_blank(),#去掉邊線
        axis.line=element_line(size=1,colour="black")) +
  annotation_custom(tableGrob(summary(all$SalePrice) %>%
                                "["(1:3) %>% as.matrix() %>% 
                                t,rows = NULL), 
                    xmin=400000, xmax=700000, 
                    ymin=75, ymax=150)+
  annotation_custom(tableGrob(summary(all$SalePrice) %>% 
                                round() %>%
                                "["(4:6) %>% 
                                as.matrix() %>% 
                                t,rows = NULL), 
                    xmin=400000, xmax=700000, 
                    ymin=75, ymax=100)


#相關性----
cor_numvar <- cor(all[, num_var], 
                  use="pairwise.complete.obs")
#排序之後挑出去相關性絕對值大於0.4的
cor_sortname <- 
  sort(cor_numvar[,'SalePrice'], 
       decreasing = TRUE) %>% 
  as.matrix() %>% 
  apply(1, function(x) abs(x)>0.4) %>% 
  which() %>% 
  names()
cor_numvar_sort <- cor_numvar[cor_sortname, cor_sortname]
corrplot.mixed(cor_numvar_sort, tl.col="black", tl.pos = "lt")

#Overall Quality----
grid.arrange(all %>% filter(!(is.na(SalePrice))) %>% 
               ggplot(aes(x=factor(OverallQual), y=SalePrice))+
               geom_boxplot(col='dodgerblue2') + 
               labs(x='Overall Quality') +
               scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma) +
               GGvisualize_theme(),
             all %>% filter(!(is.na(SalePrice))) %>% 
               ggplot(aes(x=OverallQual, y=SalePrice))+
               geom_point(col='dodgerblue2',position = "jitter") + 
               geom_smooth(method = "lm", se=FALSE, color="deeppink3") +
               scale_x_continuous(breaks = seq(1,10,1))+
               scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma) +
               GGvisualize_theme()
             )



#Above Grade (Ground) Living Area (square feet)----
all %>% 
  filter(!(is.na(SalePrice))) %>% 
  ggplot(aes(x=GrLivArea, y=SalePrice))+
  geom_point(col='dodgerblue2') + 
  geom_smooth(method = "lm", se=FALSE, color="deeppink3") +
  scale_y_continuous(breaks= seq(0, 800000, by=100000), 
                     labels = comma) +
  geom_text_repel(aes(label = ifelse(all$GrLivArea[!is.na(all$SalePrice)]>4500, 
                                     rownames(all), "")))+
  GGvisualize_theme()
#觀察outlier
all[c(524, 1299), c('SalePrice', 'GrLivArea', 'OverallQual')]
#觀察NA值
na_col <- 
  all[is.na(all) %>% colSums %>% ">"(0) %>% which] %>% 
  sapply(is.na) %>% colSums() %>% 
  sort(decreasing = TRUE) 
na_col
###SalePrice有1459個NA值，那是測試資料集
#pool variable----
na_col[names(na_col) %>% str_detect("Pool") %>% which]
##將NA轉換為None
all$PoolQC[is.na(all$PoolQC)] <- 'None'
all$PoolQC<-as.integer(plyr::revalue(all$PoolQC,
                                     c('None' = 0, 'Po' = 1, 
                                       'Fa' = 2, 'TA' = 3,
                                       'Gd' = 4, 'Ex' = 5)))
#觀察有游泳池的房子
table(all$PoolQC)
all[all$PoolQC != 0,c("SalePrice","PoolQC","PoolArea","OverallQual")]

all %>% 
  filter(!(is.na(SalePrice)) & PoolQC != 0) %>% 
  ggplot(aes(x = PoolArea, 
             y = SalePrice ,
             size = PoolQC
             )) +
  geom_point(color = "steelblue") +
  GGvisualize_theme()
#有另一個變數和PoolQC有關,就是PoolArea variable (in square feet),檢查一下發現有三間房子沒有Pool
#但是卻有PoolArea的數據
table(all$PoolArea != 0 )
all[all$PoolArea > 0 & all$PoolQC==0, c('SalePrice','PoolArea', 'PoolQC', 'OverallQual')]
#觀察PoolArea>0和PoolQC和OverallQual間的關係
all[all$PoolArea > 0,] %>% 
  ggplot(aes(x = OverallQual,y = PoolArea)) + 
  geom_point(aes(alpha = PoolQC),
             size = 10,color = 'steelblue') +
  geom_text(aes(label = ifelse(all[all$PoolArea > 0,"PoolArea"]> 0,
                               rownames(all[all$PoolArea > 0,]),
                               "")),nudge_y = 40)  +
  GGvisualize_theme()
#根據圖形填為Fa
all$PoolQC[c(2421,2504,2600)] <- 2


#Miscellaneous Feature----
names(all)[names(all) %>% str_detect("Mis") %>% which]
na_col[names(na_col) %>% str_detect("Mis") %>% which]


all$MiscFeature[is.na(all$MiscFeature)] <- 'None'
all$MiscFeature <- as.factor(all$MiscFeature)

all %>% 
  filter(!(is.na(SalePrice))) %>% 
  ggplot(aes(x=MiscFeature, y=SalePrice)) +
  stat_summary(geom ='bar', fun = "median", fill='dodgerblue2') +
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..))  +
  GGvisualize_theme() +
  annotate('text',x = 2.4,y = 245000,label = '總共')+
  annotation_custom(tableGrob(table(all$MiscFeature) %>% as.data.frame()%>% t ,rows = NULL), 
                    xmin=0.75, xmax=4, 
                    ymin=180000, ymax=250000)

#MiscVal
all %>% 
  filter(!(MiscFeature %in% "None") & MiscVal == 0) %>% 
  select("Id","MiscVal","MiscFeature")
all %>% 
  filter(MiscFeature %in% "None" & MiscVal != 0) %>%
  select("Id","MiscVal","MiscFeature")

all %>% 
  filter(!(is.na(SalePrice)),MiscVal != 0) %>% 
  ggplot(aes(x = SalePrice,y = MiscVal,color = MiscFeature)) +
  geom_point(size = 3) + 
  GGvisualize_theme()
all[2550,'MiscFeature'] <- 'Gar2'


#Alley----
all$Alley[is.na(all$Alley)] <- 'None'
all$Alley %<>% as.factor()

all %>% 
  filter(!(is.na(SalePrice))) %>% 
  ggplot(aes(x=Alley, y=SalePrice)) +
  stat_summary(geom ='bar', fun = "median", fill='dodgerblue2')+
  scale_y_continuous(breaks= seq(0, 200000, by=50000), labels = comma) + 
  geom_label(stat = "count",aes(label = ..count..,y = ..count..)) + 
  GGvisualize_theme() +
  annotate('text',x = 0.9 ,y = 175000  ,label = '總共')+
  annotation_custom(tableGrob(table(all$Alley) %>% as.data.frame() ,
                              rows = NULL,cols = NULL), 
                    xmin=0.25, xmax = 1.5, 
                    ymin=120000, ymax=175000)
#Fence----
all$Fence[is.na(all$Fence)] <- 'None'
all %>% filter(!(is.na(SalePrice))) %>%
  ggplot(aes(x=Fence, y=SalePrice)) +
  stat_summary(geom ='bar', fun = "median", fill='dodgerblue2')+
  scale_y_continuous(labels = comma) + 
  geom_label(stat = "count",aes(label = ..count..,y = ..count..)) + 
  GGvisualize_theme() +
  annotate('text',x = 3 ,y = 60000  ,label = '總共')+
  annotation_custom(tableGrob(table(all$Fence) %>% as.data.frame() %>% t() ,rows = NULL), 
                    xmin=1.5, xmax = 4.5, 
                    ymin=5000, ymax=75000)

all$Fence <- as.factor(all$Fence)
#Fireplace----
names(all)[names(all) %>% str_detect("Fireplace") %>% which]
na_col[names(na_col) %>% str_detect("Fireplace") %>% which]

#FireplaceQu
all$FireplaceQu[is.na(all$FireplaceQu)] <- 'None'
all$FireplaceQu <- as.integer(plyr::revalue(all$FireplaceQu,
                                            c('None' = 0, 'Po' = 1, 
                                              'Fa' = 2, 'TA' = 3,
                                              'Gd' = 4, 'Ex' = 5)))
all %>% filter(!(is.na(SalePrice))) %>%
  ggplot(aes(x = FireplaceQu, y = SalePrice)) +
  stat_summary(geom ='bar', fun = "median", fill='dodgerblue2')+
  scale_y_continuous(labels = comma) + 
  geom_label(stat = "count",aes(label = ..count..,y = ..count..)) + 
  GGvisualize_theme() +
  annotate('text',x = 2 ,y = 290000  ,label = '總共')+
  annotation_custom(tableGrob(table(all$FireplaceQu) %>% as.data.frame() %>% t() ,rows = NULL), 
                    xmin=0, xmax = 4, 
                    ymin=220000, ymax=280000)


#Fireplaces
table(all$Fireplaces)
all %>% filter(!(is.na(SalePrice))) %>%
  ggplot(aes(x = Fireplaces, y = SalePrice)) +
  stat_summary(geom ='bar', fun = "median", fill='dodgerblue2')+
  scale_y_continuous(labels = comma) + 
  geom_label(stat = "count",aes(label = ..count..,y = ..count..)) + 
  GGvisualize_theme()
#Lot----
#LotFrontage: Linear feet of street connected to property
#486NA,這邊用同地區LotFrontage的中位數來填補這些NA

names(all)[names(all) %>% str_detect("Lot") %>% which]
na_col[names(na_col) %>% str_detect("Lot") %>% which]


all %>% 
  ggplot( aes(x=as.factor(Neighborhood), y=LotFrontage)) +
  stat_summary(geom ='bar', fun = "median", fill='dodgerblue2') +
  stat_summary(geom ='bar', fun = "sd", fill='black',width = 0.5) + 
  GGvisualize_theme()
#利用dplyr結合ifelse進行向量式填補
all %<>% group_by(Neighborhood) %>% 
         mutate(LotFrontage = ifelse(is.na(LotFrontage),
                                     median(LotFrontage,na.rm = T),
                                     LotFrontage)) %>% 
         as.data.frame()


#LotShape: General shape of property

all %>% 
  filter(!(is.na(SalePrice))) %>% 
  ggplot( aes(x=LotShape, y=SalePrice)) +
  stat_summary(geom ='bar', fun = "median", fill='dodgerblue2') +
  scale_y_continuous(labels = comma) + 
  geom_label(stat = "count",aes(label = ..count..,y = ..count..)) + 
  GGvisualize_theme()
all$LotShape %<>% as.factor()

table(all$LotShape)

#LotConfig: Lot configuration
#No NAs. 
all %>% filter(!(is.na(SalePrice))) %>% 
  ggplot( aes(x=LotConfig, y=SalePrice)) +
  stat_summary(geom ='bar', fun = "median", fill='dodgerblue2') +
  stat_summary(geom ='bar', fun = "sd", fill='deeppink3',width = 0.5) +
  scale_y_continuous(labels = comma) + 
  geom_label(stat = "count",aes(label = ..count..,y = ..count..)) + 
  GGvisualize_theme()
all$LotConfig %<>% as.factor()

table(all$LotConfig)


#Garage---- 
#GarageYrBlt: Year garage was built 

names(all)[names(all) %>% str_detect("Garage") %>% which]
na_col[names(na_col) %>% str_detect("Garage") %>% which]

#將GarageYrBlt的NA用YearBuilt填補
all$GarageYrBlt[is.na(all$GarageYrBlt)] <- all$YearBuilt[is.na(all$GarageYrBlt)]
#GarageCars & GarageArea
all[!is.na(all$GarageType) & is.na(all$GarageFinish),
    c('GarageCars', 'GarageArea', 'GarageType',
      'GarageCond', 'GarageQual', 'GarageFinish')]

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



all[,str_detect(names(all),"Garage")] %>% 
  sapply(is.na) %>% 
  colSums() %>% 
  "["(.!=0)


#GarageType: Garage location
all$GarageType[is.na(all$GarageType)] <- 'No Garage'
all$GarageType <- as.factor(all$GarageType)
table(all$GarageType)
#GarageFinish: Interior finish of the garage
all %>% 
  filter(!(is.na(SalePrice))) %>% 
  ggplot( aes(x=GarageFinish, y=SalePrice)) +
  stat_summary(geom ='bar', fun = "median", fill='dodgerblue2') +
  stat_summary(geom ='bar', fun = "sd", fill='deeppink3',width = 0.5) +
  scale_y_continuous(labels = comma) + 
  geom_label(stat = "count",aes(label = ..count..,y = ..count..)) + 
  GGvisualize_theme()
all$GarageFinish[is.na(all$GarageFinish)] <- 'None'
all$GarageFinish <-
  as.integer(plyr::revalue(all$GarageFinish, c('None'=0, 'Unf'=1, 
                                               'RFn'=2, 'Fin'=3)))
table(all$GarageFinish)
#GarageQual: Garage quality

all$GarageQual[is.na(all$GarageQual)] <- 'None'
all$GarageQual<-as.integer(plyr::revalue(all$GarageQual, c('None' = 0, 'Po' = 1, 'Fa' = 2,
                                                     'TA' = 3,'Gd' = 4, 'Ex' = 5)))
all %>% 
  filter(!(is.na(SalePrice))) %>% 
  ggplot( aes(x=GarageQual, y=SalePrice)) +
  stat_summary(geom ='bar', fun = "median", fill='dodgerblue2') +
  stat_summary(geom ='bar', fun = "sd", fill='deeppink3',width = 0.5) +
  scale_y_continuous(labels = comma) + 
  geom_label(stat = "count",aes(label = ..count..,y = ..count..)) + 
  GGvisualize_theme()
all$GarageQual <- ifelse(all$GarageQual %in% c(0:1),0,
                         ifelse(all$GarageQual %in% c(4:5),3,all$GarageQual - 1))
all %>% 
  filter(!(is.na(SalePrice))) %>% 
  ggplot( aes(x=GarageQual, y=SalePrice)) +
  geom_bar(stat = 'summary', fun.y = "median", fill = 'dodgerblue2') +
  geom_bar(stat = 'summary', fun.y = "sd", fill = 'deeppink3',width = 0.5) +
  scale_y_continuous(labels = comma) + 
  geom_label(stat = "count",aes(label = ..count..,y = ..count..)) + 
  GGvisualize_theme()

#GarageCond: Garage condition
all$GarageCond[is.na(all$GarageCond)] <- 'None'
all$GarageCond <-
  as.integer(plyr::revalue(all$GarageCond, c('None' = 0, 'Po' = 1, 'Fa' = 2,
                                             'TA' = 3,'Gd' = 4, 'Ex' = 5)))
all %>% 
  filter(!(is.na(SalePrice))) %>% 
  ggplot( aes(x=GarageCond, y=SalePrice)) +
  stat_summary(geom ='bar', fun = "median", fill='dodgerblue2') +
  stat_summary(geom ='bar', fun = "sd", fill='deeppink3',width = 0.5) +
  scale_y_continuous(labels = comma) + 
  geom_label(stat = "count",aes(label = ..count..,y = ..count..)) + 
  GGvisualize_theme()

#Basement----
na_col[names(na_col) %>% str_detect("Bsmt") %>% which]
length(which(is.na(all$BsmtQual) & is.na(all$BsmtCond) & 
             is.na(all$BsmtExposure) & is.na(all$BsmtFinType1) & 
             is.na(all$BsmtFinType2)))

#找出額外的NA
all[!is.na(all$BsmtFinType1) & 
    (is.na(all$BsmtCond)|
       is.na(all$BsmtQual)|
       is.na(all$BsmtExposure)|
       is.na(all$BsmtFinType2)),
    c('BsmtQual', 'BsmtCond', 'BsmtExposure', 'BsmtFinType1', 'BsmtFinType2')]
#一樣將缺失值用該變數最多的值來填補
all$BsmtFinType2[333] <- names(sort(-table(all$BsmtFinType2)))[1]
all$BsmtExposure[c(949, 1488, 2349)] <- names(sort(-table(all$BsmtExposure)))[1]
all$BsmtCond[c(2041, 2186, 2525)] <- names(sort(-table(all$BsmtCond)))[1]
all$BsmtQual[c(2218, 2219)] <- names(sort(-table(all$BsmtQual)))[1]

#BsmtQual: Evaluates the height of the basement
all$BsmtQual[is.na(all$BsmtQual)] <- 'None'
all$BsmtQual <-
  as.integer(plyr::revalue(all$BsmtQual, c('None' = 0, 'Fa' = 1,
                                           'TA' = 2,'Gd' = 3, 'Ex' = 4)))
all %>% 
  filter(!(is.na(SalePrice))) %>% 
  ggplot( aes(x=BsmtQual, y=SalePrice)) +
  stat_summary(geom ='bar', fun = "median", fill='dodgerblue2') +
  stat_summary(geom ='bar', fun = "sd", fill='deeppink3',width = 0.5) +
  scale_y_continuous(labels = comma) + 
  geom_label(stat = "count",aes(label = ..count..,y = ..count..))  + 
  GGvisualize_theme()


#BsmtCond: Evaluates the general condition of the basement
all$BsmtCond[is.na(all$BsmtCond)] <- 'None'
all$BsmtCond <-
  as.integer(plyr::revalue(all$BsmtCond, c('None' = 0, 'Po' = 1, 'Fa' = 2,
                                           'TA' = 3,'Gd' = 4)))
all %>%
  filter(!(is.na(SalePrice))) %>% 
  ggplot( aes(x=BsmtCond, y=SalePrice)) +
  stat_summary(geom ='bar', fun = "median", fill='dodgerblue2') +
  stat_summary(geom ='bar', fun = "sd", fill='deeppink3',width = 0.5) +
  scale_y_continuous(labels = comma) + 
  geom_label(stat = "count",aes(label = ..count..,y = ..count..))  + 
  GGvisualize_theme()
#BsmtExposure: Refers to walkout or garden level walls
all$BsmtExposure[is.na(all$BsmtExposure)] <- 'None'
all$BsmtExposure <-
  as.integer(plyr::revalue(all$BsmtExposure,
                           c('None'=0, 'No'=1, 'Mn'=2,
                             'Av'=3, 'Gd'=4)))
all %>% filter(!(is.na(SalePrice))) %>% 
  ggplot( aes(x=BsmtExposure, y=SalePrice)) +
  stat_summary(geom ='bar', fun = "median", fill='dodgerblue2') +
  stat_summary(geom ='bar', fun = "sd", fill='deeppink3',width = 0.5) +
  scale_y_continuous(labels = comma) + 
  geom_label(stat = "count",aes(label = ..count..,y = ..count..)) + 
  GGvisualize_theme()

#BsmtFinType1: Rating of basement finished area
all$BsmtFinType1[is.na(all$BsmtFinType1)] <- 'None'
all %>% 
  filter(!(is.na(SalePrice))) %>% 
  ggplot( aes(x=factor(BsmtFinType1,levels =c('None', 'Unf', 'LwQ',
                                              'Rec', 'BLQ', 'ALQ','GLQ')),
              y=SalePrice)) +
  stat_summary(geom ='bar', fun = "median", fill='dodgerblue2') +
  stat_summary(geom ='bar', fun = "sd", fill='deeppink3',width = 0.5) +
  scale_y_continuous(labels = comma) + 
  xlab('BsmtFinType1') +
  geom_label(stat = "count",aes(label = ..count..,y = ..count..)) + 
  GGvisualize_theme()

##把'LwQ','Rec', 'BLQ', 'ALQ'合併成一個'ABRL'並將這個變數轉為factor
all$BsmtFinType1 <- ifelse(all$BsmtFinType1 %in% c('LwQ','Rec', 'BLQ', 'ALQ'),
                           'ABRL',all$BsmtFinType1)
all$BsmtFinType1 %<>% factor(levels = c('None','Unf','ABRL','GLQ'))
all %>% 
  filter(!(is.na(SalePrice))) %>% 
  ggplot( aes(x=BsmtFinType1,
              y=SalePrice)) +
  stat_summary(geom ='bar', fun = "median", fill='dodgerblue2') +
  stat_summary(geom ='bar', fun = "sd", fill='deeppink3',width = 0.5) +
  scale_y_continuous(labels = comma) + 
  geom_label(stat = "count",aes(label = ..count..,y = ..count..)) + 
  GGvisualize_theme()


#BsmtFinType2: Rating of basement finished area (if multiple types)
all$BsmtFinType2[is.na(all$BsmtFinType2)] <- 'None'

all %>% 
  filter(!(is.na(SalePrice))) %>% 
  ggplot( aes(x=factor(BsmtFinType2,levels =c('None', 'Unf', 'LwQ',
                                              'Rec', 'BLQ', 'ALQ','GLQ')),
              y=SalePrice)) +
  stat_summary(geom ='bar', fun = "median", fill='dodgerblue2') +
  stat_summary(geom ='bar', fun = "sd", fill='deeppink3',width = 0.5) +
  scale_y_continuous(labels = comma) + 
  xlab('BsmtFinType2') +
  geom_label(stat = "count",aes(label = ..count..,y = ..count..)) + 
  GGvisualize_theme()
all$BsmtFinType2 <- 
  ifelse(all$BsmtFinType2 %in% c('LwQ','Rec', 'BLQ', 'ALQ'),
         'ABRL',all$BsmtFinType2)
all$BsmtFinType2 %<>% factor(levels = c('None','Unf','ABRL','GLQ'))
all %>% 
  filter(!(is.na(SalePrice))) %>% 
  ggplot( aes(x=BsmtFinType2,
              y=SalePrice)) +
  stat_summary(geom ='bar', fun = "median", fill='dodgerblue2') +
  stat_summary(geom ='bar', fun = "sd", fill='deeppink3',width = 0.5) +
  scale_y_continuous(labels = comma) + 
  geom_label(stat = "count",aes(label = ..count..,y = ..count..)) + 
  GGvisualize_theme()
#剩餘六個1-2NA的變數
all[(is.na(all$BsmtFullBath) | is.na(all$BsmtHalfBath) |
     is.na(all$BsmtFinSF1) | is.na(all$BsmtFinSF2)|
     is.na(all$BsmtUnfSF) | is.na(all$TotalBsmtSF)),
    c('BsmtQual', 'BsmtFullBath', 'BsmtHalfBath', 'BsmtFinSF1',
      'BsmtFinSF2', 'BsmtUnfSF', 'TotalBsmtSF')]
#顯然剩餘含有NA的資料都是不含有地下室

#BsmtFullBath: Basement full bathrooms
all$BsmtFullBath[is.na(all$BsmtFullBath)] <-0

#BsmtHalfBath: Basement half bathrooms
all$BsmtHalfBath[is.na(all$BsmtHalfBath)] <-0

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

na_col[names(na_col) %>% str_detect("MasVnr") %>% which]


length(which(is.na(all$MasVnrType) & is.na(all$MasVnrArea)))
all[is.na(all$MasVnrType) & !is.na(all$MasVnrArea), c('MasVnrType', 'MasVnrArea')]
#選擇用MasVnrType最多的因子來填補,但是2611列AREA有值,最多的因子是none,所以用第二多的來填補
all$MasVnrType[2611] <- names(sort(-table(all$MasVnrType)))[2]

#Masonry veneer type
all$MasVnrType[is.na(all$MasVnrType)] <- 'None'

grid.arrange(
  ggplot(all, aes(x=MasVnrType, y=SalePrice)) +
    stat_summary(geom ='bar', fun = "median", fill='grey')+
    scale_y_continuous(labels = comma) + 
    geom_label(stat = "count",aes(label = ..count..,y = ..count..)) + 
    GGvisualize_theme() + 
    ggtitle("Median"),
  ggplot(all, aes(x=MasVnrType, y=SalePrice)) +
    stat_summary(geom ='bar', fun = "mean", fill='black')+
    scale_y_continuous(labels = comma) + 
    geom_label(stat = "count",aes(label = ..count..,y = ..count..)) + 
    GGvisualize_theme() + 
    ggtitle("Mean"),
  nrow = 2 ,ncol = 1
)

all$MasVnrType<-as.integer(plyr::revalue(all$MasVnrType, 
                                         c('None'=0, 'BrkCmn'=0,
                                           'BrkFace'=1, 'Stone'=2)))
all %>% 
  filter(!(is.na(SalePrice))) %>% 
  ggplot( aes(x=MasVnrType, y=SalePrice)) +
  stat_summary(geom ='bar', fun = "median", fill='dodgerblue2') +
  stat_summary(geom ='bar', fun = "sd", fill='deeppink3',width = 0.5) +
  scale_y_continuous(labels = comma) + 
  geom_label(stat = "count",aes(label = ..count..,y = ..count..)) + 
  GGvisualize_theme()

#MasVnrArea: Masonry veneer area in square feet
all$MasVnrArea[is.na(all$MasVnrArea)] <- 0

#observe Neighborhood & MSZoning 
all %>% 
  group_by(MSZoning,Neighborhood) %>% 
  summarise(value = n()) %>% 
  xtabs(data = .,value~.)
  
#MSZoning----
#MSZoning: Identifies the general zoning classification of the sale
all %>% 
  group_by(MSZoning,Neighborhood) %>% 
  summarise(value = n()) %>% 
  xtabs(data = .,value~.)


all[all$MSZoning %>% is.na(),c('MSZoning', 'Neighborhood',"OverallQual")] 
#4個NA有三個是IDOTRR區一個是Mitchel區
#IDOTRR
filter(all,Neighborhood == "IDOTRR")$MSZoning %>% table
#Mitchel
filter(all,Neighborhood == "Mitchel")$MSZoning %>% table
#視覺化IDOTRR的SalePrice、MSZoning和OverallQual
all %>% 
  filter(Neighborhood == 'IDOTRR') %>% 
  ggplot(aes(x = OverallQual,y = SalePrice,color = MSZoning)) +
  geom_point(size = 3)+ 
  GGvisualize_theme()
#因此這邊1916 2217 -> C (all)  2251 -> RM ,2905將用RL填補
all[c(2251),"MSZoning"] <- "RM"
all[c(1916,2217),"MSZoning"] <- "C (all)"
all[2905,"MSZoning"] <- "RL"
all$MSZoning %<>% as.factor()
#Kitchen----
names(all)[names(all) %>% str_detect("Kitchen") %>% which]
na_col[names(na_col) %>% str_detect("Kitchen") %>% which]
#Kitchen quality 1NA
table(all$KitchenQual)
all$KitchenQual[is.na(all$KitchenQual)] <- 'TA' #將最多的填補回去
all$KitchenQual<-as.integer(plyr::revalue(all$KitchenQual, c('None' = 0, 'Po' = 1, 'Fa' = 2,
                                                       'TA' = 3,'Gd' = 4, 'Ex' = 5)))
all %>% 
  filter(!(is.na(SalePrice))) %>% 
  ggplot( aes(x=KitchenQual, y=SalePrice)) +
  stat_summary(geom ='bar', fun = "median", fill='dodgerblue2') +
  stat_summary(geom ='bar', fun = "sd", fill='deeppink3',width = 0.5) +
  scale_y_continuous(labels = comma) + 
  geom_label(stat = "count",aes(label = ..count..,y = ..count..))  + 
  GGvisualize_theme()

#Utilities----
#Utilities: Type of utilities available 2NA
table(all$Utilities)
#變數因子嚴重不平衡,這變數對預測沒甚麼幫助,所以就剔除掉
all$Utilities <- NULL

#Functional: Home functionality----
#1NA
all$Functional[is.na(all$Functional)] <- names(sort(-table(all$Functional)))[1]
table(all$Functional)
all %>% filter(!(is.na(SalePrice))) %>% 
  ggplot( aes(x=factor(Functional,
                       levels =  c('Sal','Sev',
                                   'Maj2','Maj1',
                                   'Mod','Min2',
                                   'Min1','Typ')), 
              y=SalePrice)) +
  stat_summary(geom ='bar', fun = "median", fill='dodgerblue2') +
  stat_summary(geom ='bar', fun = "sd", fill='deeppink3',width = 0.5) +
  scale_y_continuous(labels = comma) + 
  xlab('Functional') + 
  geom_label(stat = "count",aes(label = ..count..,y = ..count..)) + 
  GGvisualize_theme()

all$Functional <- 
  ifelse(all$Functional %in% c('Min2','Min1'),
         'Min',all$Functional)
all$Functional %<>% as.factor()

#Exter ----
na_col[names(na_col) %>% str_detect("Exterior") %>% which]
#Exterior1st: Exterior covering on house;1NA. Values are categorical.
all$Exterior1st[is.na(all$Exterior1st)] <- names(sort(-table(all$Exterior1st)))[1]
all$Exterior1st <- as.factor(all$Exterior1st)
table(all$Exterior1st)

#Exterior2nd: Exterior covering on house (if more than one material)
all$Exterior2nd[is.na(all$Exterior2nd)] <- names(sort(-table(all$Exterior2nd)))[1]
all$Exterior2nd <- as.factor(all$Exterior2nd)
table(all$Exterior2nd)

#ExterQual: Evaluates the quality of the material on the exterior
all$ExterQual <-
  as.integer(plyr::revalue(all$ExterQual, 
                           c('None' = 0, 'Po' = 1, 'Fa' = 2,
                             'TA' = 3,'Gd' = 4, 'Ex' = 5)))
all %>% 
  filter(!(is.na(SalePrice))) %>% 
  ggplot( aes(x=ExterQual, y=SalePrice)) +
  stat_summary(geom ='bar', fun = "median", fill='dodgerblue2') +
  stat_summary(geom ='bar', fun = "sd", fill='deeppink3',width = 0.5) +
  scale_y_continuous(labels = comma) + 
  geom_label(stat = "count",aes(label = ..count..,y = ..count..)) + 
  GGvisualize_theme()

table(all$ExterQual)

#ExterCond: Evaluates the present condition of the material on the exterior
all$ExterCond <- 
  as.integer(plyr::revalue(all$ExterCond,
             c('None' = 0, 'Po' = 1, 'Fa' = 2,
               'TA' = 3,'Gd' = 4, 'Ex' = 5)))

all %>% 
  filter(!(is.na(SalePrice))) %>% 
  ggplot( aes(x=ExterCond, y=SalePrice)) +
  stat_summary(geom ='bar', fun = "median", fill='dodgerblue2') +
  stat_summary(geom ='bar', fun = "sd", fill='deeppink3',width = 0.5) +
  scale_y_continuous(labels = comma) + 
  geom_label(stat = "count",aes(label = ..count..,y = ..count..)) + 
  GGvisualize_theme()



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
#Heating and air condition----
#Heating: Type of heating
all$Heating <- as.factor(all$Heating)
table(all$Heating)
#HeatingQC: Heating quality and condition
all$HeatingQC <- 
  as.integer(plyr::revalue(all$HeatingQC, 
                           c('None' = 0, 'Po' = 1, 'Fa' = 2,
                             'TA' = 3,'Gd' = 4, 'Ex' = 5)))

all %>% 
  filter(!(is.na(SalePrice))) %>% 
  ggplot( aes(x=HeatingQC, y=SalePrice)) +
  stat_summary(geom ='bar', fun = "median", fill='dodgerblue2') +
  stat_summary(geom ='bar', fun = "sd", fill='deeppink3',width = 0.5) +
  scale_y_continuous(labels = comma) + 
  geom_label(stat = "count",aes(label = ..count..,y = ..count..)) + 
  GGvisualize_theme()

table(all$HeatingQC)

#CentralAir: Central air conditioning
all$CentralAir <- 
  as.integer(plyr::revalue(all$CentralAir,
             c('N'=0, 'Y'=1)))
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
table(all$LandSlope)
all$LandSlope <-
  as.integer(plyr::revalue(all$LandSlope, 
             c('Sev' = 0, 'Mod' = 1, 'Gtl' = 2)))

all %>% 
  filter(!(is.na(SalePrice))) %>% 
  ggplot( aes(x=LandSlope, y=SalePrice)) +
  stat_summary(geom ='bar', fun = "median", fill='dodgerblue2') +
  stat_summary(geom ='bar', fun = "sd", fill='deeppink3',width = 0.5) +
  scale_y_continuous(labels = comma) + 
  geom_label(stat = "count",aes(label = ..count..,y = ..count..)) + 
  GGvisualize_theme()

all$LandSlope %<>% as.factor()

#Dwelling----
#BldgType: Type of dwelling

all %>% 
  filter(!(is.na(SalePrice))) %>% 
  ggplot( aes(x=as.factor(BldgType), y=SalePrice)) +
  stat_summary(geom ='bar', fun = "median", fill='dodgerblue2') +
  stat_summary(geom ='bar', fun = "sd", fill='deeppink3',width = 0.5) +
  scale_y_continuous(labels = comma) + 
  geom_label(stat = "count",aes(label = ..count..,y = ..count..)) + 
  GGvisualize_theme()


#沒次序關係轉成factor
all$BldgType <- as.factor(all$BldgType)
#HouseStyle: Style of dwelling
table(all$HouseStyle)

all %>% 
  filter(!(is.na(SalePrice))) %>% 
  ggplot( aes(x=HouseStyle, y=SalePrice)) +
  stat_summary(geom ='bar', fun = "median", fill='dodgerblue2') +
  stat_summary(geom ='bar', fun = "sd", fill='deeppink3',width = 0.5) +
  scale_y_continuous(labels = comma) + 
  geom_label(stat = "count",aes(label = ..count..,y = ..count..)) + 
  GGvisualize_theme()

all$HouseStyle <- as.factor(all$HouseStyle)

#Neighborhood---- 
#Neighborhood: Physical locations within Ames city limits
all$Neighborhood <- as.factor(all$Neighborhood)
table(all$Neighborhood)


#Conditions----
#Condition1: Proximity to various conditions
all$Condition1 <- as.factor(all$Condition1)
table(all$Condition1)
#Condition2: Proximity to various conditions (if more than one is present)
all$Condition2 <- as.factor(all$Condition2)
table(all$Condition2)





#Pavement of Street & Driveway----
#Street: Type of road access to property
all$Street <-as.integer(plyr::revalue(all$Street, c('Grvl'=0, 'Pave'=1)))
table(all$Street)
#PavedDrive: Paved driveway

table(all$PavedDrive)
all$PavedDrive<-as.integer(plyr::revalue(all$PavedDrive,
                                         c('N'=0, 'P'=1, 'Y'=2)))
all %>% 
  filter(!(is.na(SalePrice))) %>% 
  ggplot( aes(x=PavedDrive, y=SalePrice)) +
  stat_summary(geom ='bar', fun = "median", fill='dodgerblue2') +
  stat_summary(geom ='bar', fun = "sd", fill='deeppink3',width = 0.5) +
  scale_y_continuous(labels = comma) + 
  geom_label(stat = "count",aes(label = ..count..,y = ..count..)) + 
  GGvisualize_theme()

#將一些數值變數轉成類別變數----
#MoSold
all$MoSold <- as.factor(all$MoSold)
grid.arrange(
  ggplot(all[!is.na(all$SalePrice),], aes(x=as.factor(YrSold), y=SalePrice)) +
    stat_summary(geom ='bar', fun = "median", fill='dodgerblue2')+
    scale_y_continuous(breaks= seq(0, 800000, by=25000), labels = comma) +
    geom_label(stat = "count", aes(label = ..count.., y = ..count..)) +
    coord_cartesian(ylim = c(0, 200000)) +
    geom_hline(yintercept=163000, linetype="dashed", color = "red") + 
    GGvisualize_theme()
  , #dashed line is median SalePrice
  ggplot(all[!is.na(all$SalePrice),], aes(x=MoSold, y=SalePrice)) +
    stat_summary(geom ='bar', fun = "median", fill='dodgerblue2')+
    scale_y_continuous(breaks= seq(0, 800000, by=25000), labels = comma) +
    geom_label(stat = "count", aes(label = ..count.., y = ..count..)) +
    coord_cartesian(ylim = c(0, 200000)) +
    geom_hline(yintercept=163000, linetype="dashed", color = "red") + 
    GGvisualize_theme() + 
    theme(axis.title.y = element_blank(),
          axis.text.y = element_blank())
  , #dashed line is median SalePrice
  widths=c(2,3)
)
#MSSubClass
#MSSubClass: Identifies the type of dwelling involved in the sale.
table(all$MSSubClass)
all$MSSubClass <- as.factor(all$MSSubClass)



#探索資料分析&特徵組合----
num_var2 <- which(sapply(all, is.numeric)) 
factor_var <- which(sapply(all, is.factor))
cat('總共有', length(num_var2), '連續變數, and',
    length(factor_var), '類別變數')
#相關性
CorNumvar_after <- cor(all[, num_var2], 
                  use="pairwise.complete.obs") 
CorSortName_after <- sort(CorNumvar_after[,'SalePrice'], 
                      decreasing = TRUE) %>% 
                     as.matrix() %>% 
                     apply(1, function(x) abs(x)>0.5) %>% 
                     which() %>% 
                     names()
CorNumvarSort <- CorNumvar_after[CorSortName_after, CorSortName_after]
#比較處理過數值之後的相關性
##這邊沒辦法用grid.arrange,因為corrplot不是grob物件
##https://stackoverflow.com/questions/53734543/converting-corrplot-output-to-grob
##這邊有解法,但是暫時先用r base套件來處理
oldpar <- par()
par(mfrow = c(1,2))
corrplot.mixed(cor_numvar_sort,
               tl.col="black",
               tl.pos = "lt",
               tl.cex = 0.7, #標題大小
               cl.cex = 0.7, #color bar 大小
               number.cex=0.7,
               title = 'BEROFE',
               mar=c(0,0,2,0))

corrplot.mixed(CorNumvarSort, 
               tl.col="black", 
               tl.pos = "lt", 
               tl.cex = 0.7, #標題大小
               cl.cex = 0.7, #color bar 大小
               number.cex=0.7,
               title = 'AFTER',
               mar=c(0,0,2,0))
par(oldpar)


#藉由隨機森林探尋特徵的重要性----
set.seed(2018)
quick_RF <- randomForest(x=all %>% 
                            filter(!(is.na(SalePrice))) %>% 
                            select(-SalePrice,-Id),
                         
                         y=all %>% 
                            filter(!(is.na(SalePrice))) %>% 
                            "$"(SalePrice),
                         ntree=100,importance=TRUE)
imp_RF <- importance(quick_RF)
imp_DF <- data.frame(Variables = row.names(imp_RF),
                     MSE = imp_RF[,1]) %>% 
          arrange(desc(MSE))


important <-  ggplot(imp_DF[1:30,], 
                    aes(x=reorder(Variables, MSE),
                        y=MSE, 
                        fill=MSE)) + 
             geom_bar(stat = 'identity') + 
             scale_y_continuous(labels = function(x)paste0(x,'%'),
                                expand = c(0,0),
                                limits = c(0,20)) +
             labs(x = '變數', 
                  y= '替換掉MSE會提升的%數') + 
             coord_flip() + 
             GGvisualize_theme() +
             theme(legend.position="none",
                   axis.text.x = element_text(size = 25,
                                              face = 'bold'),
                   axis.text.y = element_text(size = 30,
                                              face = 'bold'),
                   axis.title.x = element_text(size = 40,
                                               face = 'bold'),
                   axis.title.y = element_text(size = 40,
                                               face = 'bold')) 
important
##輸出重要性

CairoPNG('output/importance.png',width = 1200, height = 800)
print(important)
dev.off()

#和GrLivArea相關特徵----
grid.arrange(all %>% ggplot() +
               geom_density(aes(x = GrLivArea),
                            fill = 'pink',color = 'deeppink') + 
               GGvisualize_theme(),
             all %>% ggplot() +
               geom_density(aes(x = X1stFlrSF),
                            fill = 'pink',color = 'deeppink') + 
               GGvisualize_theme(),
             all %>% ggplot() +
               geom_density(aes(x = X2ndFlrSF),
                            fill = 'pink',color = 'deeppink') + 
               GGvisualize_theme(),
             all %>% filter() %>% ggplot()+
               geom_density(aes(x = LowQualFinSF),
                            fill = 'pink',color = 'deeppink') + 
               GGvisualize_theme(),
             all %>% ggplot() +
               geom_density(aes(x = TotalBsmtSF),
                            fill = 'pink',color = 'deeppink') + 
               GGvisualize_theme(),
             all %>% ggplot() +
               geom_density(aes(x = TotRmsAbvGrd),
                            fill = 'pink',color = 'deeppink') + 
               scale_x_continuous(breaks = seq(0,max(all$TotRmsAbvGrd))) + 
               GGvisualize_theme(),
             all %>% filter(LotArea< 50000) %>% ggplot()+
               geom_density(aes(x = LotArea) ,
                            fill = 'pink',color = 'deeppink') + 
               GGvisualize_theme(),
             all %>% ggplot()+
               geom_density(aes(x = LotFrontage),
                            fill = 'pink',color = 'deeppink') + 
               GGvisualize_theme(),
             layout_matrix = matrix(1:8,4,2,byrow=TRUE))






#相關性
CorGrivArea <- cor(cbind(all[,c('SalePrice','GrLivArea','X1stFlrSF','X2ndFlrSF',
                                'LowQualFinSF','TotalBsmtSF','TotRmsAbvGrd',
                                'LotArea','LotFrontage')],
                         all$X1stFlrSF+all$X2ndFlrSF), 
                   use="pairwise.complete.obs") 
CorGrivArea_Sort <- sort(CorGrivArea[,'SalePrice'], 
                         decreasing = TRUE) %>%
                      names()
##GrLivArea幾乎和X1stFlrSF、X2ndFlrS相加一樣
CorGrivArea[CorGrivArea_Sort, CorGrivArea_Sort] %>% 
  corrplot.mixed(tl.col="black", tl.pos = "lt")
##相加X1stFlrSF、X2ndFlrS替代GrLivArea
all$GrLivArea <- all$X1stFlrSF + all$X2ndFlrSF
##新增TotalArea = GrLivArea+TotalBsmtSF
all$TotalArea <- all$GrLivArea + all$TotalBsmtSF
all %>% 
  filter(!(is.na(SalePrice))) %>% 
  ggplot(aes(x=TotalArea, y=SalePrice))+
  geom_point(color='blue') + 
  geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1)) +
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma) +
  geom_text_repel(aes(label = ifelse(all$TotalArea[!is.na(all$SalePrice)]>7500,
                                     rownames(all), ''))) + 
  GGvisualize_theme()
#0.782
cor(all$SalePrice, all$TotalArea, use= "pairwise.complete.obs")
#0.832
cor(all$SalePrice[-c(524, 1299)], all$TotalArea[-c(524, 1299)], 
    use= "pairwise.complete.obs")


#Neighborhood-----
##視覺化
grid.arrange(all %>% ggplot(aes(x=Neighborhood, y=SalePrice)) +
               geom_bar(stat='summary',
                        fun.y = "median", 
                        fill='blue') +
               scale_y_continuous(breaks= c(seq(0, 800000, by=50000),
                                            all[!is.na(all$SalePrice),'SalePrice'] %>% 
                                              median()), 
                                  labels = comma) +
               geom_label(stat = "count",
                          aes(label = ..count.., y = ..count..),
                          size=3) +
               geom_hline(yintercept= all[!is.na(all$SalePrice),'SalePrice'] %>% 
                                        median(), 
                          linetype="dashed", 
                          size = 2,
                          color = "red") +#銷售價格中位數
               GGvisualize_theme() +
               theme(axis.text.x = element_text(angle = 45, hjust = 1)),  
             all %>% ggplot(aes(x=Neighborhood)) +
               geom_histogram(stat='count')+
               geom_label(stat = "count", 
                          aes(label = ..count.., y = ..count..), 
                          size=3)+
               GGvisualize_theme() + 
               theme(axis.text.x = element_text(angle = 45, hjust = 1)))

##觀察排序後Neighborhood
grid.arrange(ggplot(all[!is.na(all$SalePrice),], 
                    aes(x=reorder(Neighborhood, SalePrice, FUN=median), y=SalePrice)) +
               stat_summary(geom ='bar', fun = "median", fill='blue') + 
               labs(x='Neighborhood', y='Median SalePrice') +
               scale_y_continuous(breaks= seq(0, 800000, by=50000), labels = comma) +
               geom_label(stat = "count", 
                          aes(label = ..count.., y = ..count..),
                          size=3) +
               geom_hline(yintercept=all[!is.na(all$SalePrice),'SalePrice'] %>% median(),
                          linetype="dashed", color = "red") + #銷售價格中位數
               GGvisualize_theme() + 
               theme(axis.text.x = element_text(angle = 45, hjust = 1)) , 
             ggplot(all[!is.na(all$SalePrice),], 
                    aes(x=reorder(Neighborhood, SalePrice, FUN=mean), y=SalePrice)) +
               stat_summary(geom ='bar', fun = "mean", fill='blue') +
               labs(x='Neighborhood', y="Mean SalePrice") +
               scale_y_continuous(breaks= seq(0, 800000, by=50000), labels = comma) +
               geom_label(stat = "count", 
                          aes(label = ..count.., y = ..count..), size=3) +
               geom_hline(yintercept=163000, 
                          linetype="dashed", color = "red")+ #銷售價格中位數
               GGvisualize_theme() +
               theme(axis.text.x = element_text(angle = 45, hjust = 1))
) 

##重新編碼
all$NeighCluster[all$Neighborhood %in% c('StoneBr', 'NridgHt', 'NoRidge')] <- 2
all$NeighCluster[!all$Neighborhood %in% c('MeadowV', 'IDOTRR', 'BrDale', 'StoneBr', 'NridgHt', 'NoRidge')] <- 1 #介於貧窮以及富有的街區
all$NeighCluster[all$Neighborhood %in% c('MeadowV', 'IDOTRR', 'BrDale')] <- 0
all %>% 
  filter(!(is.na(SalePrice))) %>% 
  ggplot( aes(x=NeighCluster, y=SalePrice)) +
  stat_summary(geom ='bar', fun = "median", fill='dodgerblue2') +
  stat_summary(geom ='bar', fun = "sd", fill='deeppink3',width = 0.5) +
  scale_y_continuous(labels = comma) + 
  geom_label(stat = "count",aes(label = ..count..,y = ..count..)) + 
  GGvisualize_theme()
##暫時踢掉Neighborhood
all$Neighborhood <- NULL


#MSSubClass----

grid.arrange(ggplot(all[!is.na(all$SalePrice),], aes(x=MSSubClass, y=SalePrice)) +
               stat_summary(geom ='bar', fun = "median", fill='blue') +
               theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
               scale_y_continuous(breaks= seq(0, 800000, by=50000), labels = comma) +
               geom_label(stat = "count", aes(label = ..count.., y = ..count..), size=3) +
               geom_hline(yintercept=163000, linetype="dashed", color = "red") +
               GGvisualize_theme() ,#dashed line is median SalePrice
             ggplot(data=all, aes(x=MSSubClass)) +
               geom_histogram(stat='count')+
               geom_label(stat = "count", aes(label = ..count.., y = ..count..), size=3) +
               theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
               GGvisualize_theme() 
  
)

# Garage variables----
names(all)[names(all) %>% str_detect("Garage") %>% which]
all$GarageYrBlt[2593] <- 2007 #keying error GarageYrBlt=2207, YearBuilt=2006, YearRemodAdd=2007
grid.arrange(ggplot(data=all[all$GarageCars !=0,], aes(x=GarageYrBlt)) +
               geom_histogram() +
               GGvisualize_theme() ,
             ggplot(data=all, aes(x=as.factor(GarageCars))) +
               geom_histogram(stat='count') +
               GGvisualize_theme() ,
             ggplot(data= all, aes(x=GarageArea)) +
               geom_histogram() +
               GGvisualize_theme() ,
             ggplot(data=all, aes(x=as.factor(GarageCond))) +
               geom_histogram(stat='count') +
               GGvisualize_theme() ,
             ggplot(data=all, aes(x=GarageType)) +
               geom_histogram(stat='count') +
               GGvisualize_theme() ,
             ggplot(data=all, aes(x=as.factor(GarageQual))) +
               geom_histogram(stat='count') +
               GGvisualize_theme() ,
             ggplot(data=all, aes(x=as.factor(GarageFinish))) +
               geom_histogram(stat='count') +
               GGvisualize_theme() ,
             layout_matrix =matrix(c(1,5,5,2,3,8,6,4,7),3,3,byrow=TRUE)
             
)

#量化廁所數量----

all$TotBathrooms <- all$FullBath + (all$HalfBath*0.5) + all$BsmtFullBath + (all$BsmtHalfBath*0.5)

grid.arrange(
  ggplot(data=all[!is.na(all$SalePrice),], 
         aes(x=as.factor(TotBathrooms), 
             y=SalePrice))+
    geom_point(col='blue') + 
    geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1)) +
    labs(y = 'SalePrice(千元)')+
    scale_y_continuous(breaks= seq(0, 800000, by=100000), 
                       labels = function(d){
                         d/1000
                       }) +
    GGvisualize_theme(),
  ggplot(data=all, aes(x=as.factor(TotBathrooms))) +
    geom_histogram(stat='count',width = 0.5) +
    GGvisualize_theme())

cor(all$SalePrice[!is.na(all$SalePrice)], all$TotBathrooms[!is.na(all$SalePrice)])

#屋齡----
all$Age <- as.numeric(as.character(all$YrSold))-all$YearRemodAdd
all$Age2 <- as.numeric(as.character(all$YrSold))-all$YearBuilt

library(ggpmisc)

grid.arrange(
  all[!is.na(all$SalePrice),] %>%
    ggplot(aes(x=Age, y=SalePrice%>% log))+
    geom_point(col='blue') + 
    geom_smooth(method = "lm", se=T,level = 0.95, color="black") +
    stat_poly_eq(formula =  y~x,
                 aes(label = paste(..rr.label..)), 
                 parse = TRUE)+
    scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma) +
    GGvisualize_theme(),
  all[!is.na(all$SalePrice),] %>%
    ggplot(aes(x=Age2, y=SalePrice %>% log()))+
    geom_point(col='blue') + 
    geom_smooth(method = "lm", se=T, color="black", aes(group=1)) +
    stat_poly_eq(formula =  y~x,
                 aes(label = paste(..rr.label..)), 
                 parse = TRUE)+
    scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma) +
    GGvisualize_theme()
  
)

cor(all$SalePrice[!is.na(all$SalePrice)], all$Age[!is.na(all$SalePrice)])
cor(all$SalePrice[!is.na(all$SalePrice)], all$Age2[!is.na(all$SalePrice)])

all$Age2 <- NULL
all$Age <- as.numeric(as.character(all$YrSold))-all$YearRemodAdd

#是否改建----
all$Remod <- ifelse(all$YearBuilt==all$YearRemodAdd, 0, 1) 

#視覺化
ggplot(all[!is.na(all$SalePrice),], aes(x=as.factor(Remod), y=SalePrice)) +
  stat_summary(geom ='bar', fun = "median", fill='dodgerblue2') +
  stat_summary(geom ='bar', fun = "sd", fill='black',width = 0.5) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..), size=6) +
  scale_y_continuous(breaks= seq(0, 800000, by=50000), labels = comma) +
  theme_grey(base_size = 18) +
  geom_hline(yintercept=163000, linetype="dashed") +
  GGvisualize_theme()
#改建x地區
ggplot(all[!is.na(all$SalePrice),], aes(x=as.factor(Remod), y=SalePrice)) +
  stat_summary(geom ='bar', fun = "median", fill='dodgerblue2') +
  stat_summary(geom ='bar', fun = "sd", fill='black',width = 0.5) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..), size=6) +
  scale_y_continuous(breaks= seq(0, 800000, by=50000), labels = comma) +
  theme_grey(base_size = 18) +
  facet_wrap(NeighCluster~.)+
  geom_hline(yintercept=163000, linetype="dashed") +
  GGvisualize_theme()

#無母數中位數檢定

kruskal.test(data = all[!is.na(all$SalePrice),],SalePrice ~ Remod)


# 是否新屋-----

SalePriceDiff <- data.frame()
for (i in 0:5) {
  all$IsNew <- ifelse(as.numeric(as.character(all$YrSold)) - all$YearBuilt <= i, 1, 0)
  medianDiff <- 
    median(all[all$IsNew == 1 & !is.na(all$SalePrice),'SalePrice']) -
    median(all[all$IsNew == 0 & !is.na(all$SalePrice),'SalePrice'])
  lm_isnew <- lm(log(all$SalePrice)~all$IsNew) %>% summary
  SalePriceDiff <- rbind(SalePriceDiff, 
                         data.frame(
                           year = i,
                           medianDiff = medianDiff,
                           sd_0 =  sd(all[all$IsNew == 0 & !is.na(all$SalePrice),'SalePrice']),
                           sd_1 =  sd(all[all$IsNew == 1 & !is.na(all$SalePrice),'SalePrice']),
                           IsNewNum = table(all$IsNew)[2],
                           IsNewNum_train = table(all[!is.na(all$SalePrice),'IsNew'])[2]
                           )
                         )
}
SalePriceDiff 

#簡單迴歸比較
SalePriceDiff <- data.frame()
for (i in 0:5) {
  all$IsNew <- ifelse(as.numeric(as.character(all$YrSold)) - all$YearBuilt <= i, 1, 0)
  medianDiff <- 
    median(all[all$IsNew == 1 & !is.na(all$SalePrice),'SalePrice']) -
    median(all[all$IsNew == 0 & !is.na(all$SalePrice),'SalePrice'])
  lm_isnew <- lm(log(all$SalePrice)~all$IsNew) %>% summary
  SalePriceDiff <- rbind(SalePriceDiff, 
                         data.frame(
                           year = i,
                           medianDiff = medianDiff,
                           IsNewNum = table(all$IsNew)[2],
                           IsNewNum_train = table(all[!is.na(all$SalePrice),'IsNew'])[2],
                           rmse = lm_isnew$sigma,
                           rSquare = lm_isnew$adj.r.squared
                         )
  )
}
SalePriceDiff 



all$IsNew <- ifelse(as.numeric(as.character(all$YrSold)) - all$YearBuilt <= 3, 1, 0)
table(all$IsNew)
ggplot(all[!is.na(all$SalePrice),], aes(x=as.factor(IsNew), y=SalePrice)) +
  stat_summary(geom ='bar', fun = "median", fill='steelblue') +
  stat_summary(geom ='bar', fun = "sd", fill='black',width = 0.5) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..), size=6) +
  scale_y_continuous(breaks= seq(0, 800000, by=50000), labels = comma) +
  theme_grey(base_size = 18) +
  geom_hline(yintercept=163000, linetype="dashed") +
  GGvisualize_theme()

#最大偏好屋齡----
SalePriceDiff <- data.frame()
for (i in 0:60) {
  all$IsNewMax <- ifelse(as.numeric(as.character(all$YrSold)) - all$YearBuilt <= i, 1, 0)
  medianDiff <- 
    median(all[all$IsNewMax == 1 & !is.na(all$SalePrice),'SalePrice']) -
    median(all[all$IsNewMax == 0 & !is.na(all$SalePrice),'SalePrice'])
  lm_isnew <- lm(log(all$SalePrice)~all$IsNewMax) %>% summary
  SalePriceDiff <- rbind(SalePriceDiff, 
                         data.frame(
                           year = i,
                           medianDiff = medianDiff,
                           IsNewNum = table(all$IsNewMax)[2],
                           IsNewNum_train = table(all[!is.na(all$SalePrice),'IsNewMax'])[2],
                           rmse = lm_isnew$sigma,
                           rSquare = lm_isnew$adj.r.squared
                         )
  )
}
grid.arrange(
  SalePriceDiff %>%  
    ggplot(aes(x = year,y = rSquare,group = 1)) + 
    geom_point() +
    geom_text(aes(label = round(rSquare,3)),
              color = ifelse(max(SalePriceDiff$rSquare) == SalePriceDiff$rSquare,
                             'red','transparent'),
              vjust = 1.5,
              size = 5) + 
    scale_x_continuous(
      breaks = c(seq(0,60,10),
                 SalePriceDiff[which(SalePriceDiff$rSquare == max(SalePriceDiff$rSquare)),
                               'year'])) +
    geom_line() + 
    GGvisualize_theme(),
  SalePriceDiff %>%  
    ggplot(aes(x = year,y = medianDiff)) + 
    geom_bar(stat = 'identity') +
    geom_text(aes(label = medianDiff),
              color = ifelse(max(SalePriceDiff$rSquare) == SalePriceDiff$rSquare,
                             'red','transparent'),
              vjust = -1,
              size = 5) + 
    scale_y_continuous(labels = function(d){
      d/10000
    })+
    scale_x_continuous(
      breaks = c(seq(0,60,10),
                 SalePriceDiff[which(SalePriceDiff$rSquare == max(SalePriceDiff$rSquare)),
                               'year'])) +
    GGvisualize_theme()
  
)



#模型準備----
#踢掉高相關性的變數
dropVars <- c('YearRemodAdd', 'GarageYrBlt', 'GarageArea', 
              'GarageCond', 'TotalBsmtSF', 'TotalRmsAbvGrd', 
              'BsmtFinSF1')
all <- all[,!(names(all) %in% dropVars)]

#數值變數
numericVarNames <- which(sapply(all, is.numeric)) %>% names()
numericVarNames <- numericVarNames[!(numericVarNames %in%
                                       c('SalePrice','Id'))] 

#about c() & append()
#https://stackoverflow.com/questions/16144683/difference-between-c-and-append

DFnumeric <- all[, names(all) %in% numericVarNames]

DFfactors <- all[, !(names(all) %in% numericVarNames)]
DFfactors <- DFfactors[, !names(DFfactors) %in% c('SalePrice','Id')] 

cat('There are', length(DFnumeric), 'numeric variables, and', length(DFfactors), 'factor variables')

#偏態
for(i in 1:ncol(DFnumeric)){
  if (abs(psych::skew(DFnumeric[,i]))>0.5){
    DFnumeric[,i] <- log(DFnumeric[,i]+1)
  }
}

#標準化
PreNum <- preProcess(DFnumeric, method=c("center", "scale")) #caret
print(PreNum)
DFnorm <- predict(PreNum, DFnumeric)
dim(DFnorm)

#One hot encoding
#r 內建
# DFdummies <- as.data.frame(model.matrix(~.-1, DFfactors,
#                                         contrasts.arg = lapply(DFfactors, 
#                                                                contrasts, 
#                                                                contrasts=FALSE)))
# dim(DFdummies)
#caret 版本
PreFac <- dummyVars(" ~ .", data = DFfactors)
DFdummies <- data.frame(predict(PreFac, DFfactors))
dim(DFdummies)

#踢掉在train或test data裡分別小於等於10的類別變數
#test
Tencol_test <- 
  which(DFdummies[all$SalePrice %>% is.na %>% which,] %>% 
          colSums() <= 10)
colnames(DFdummies[Tencol_test])
DFdummies <- DFdummies[,-Tencol_test] 

#train
Zerocol_train <- 
  which(DFdummies[which(!(all$SalePrice %>% is.na)),] %>% 
          colSums() <= 10)
colnames(DFdummies[Zerocol_train])
DFdummies <- DFdummies[,-Zerocol_train]

modelData <- cbind(DFnorm, DFdummies) 
#目標特徵的偏態
psych::skew(all$SalePrice)
qqnorm(all$SalePrice)
qqline(all$SalePrice)

all$SalePrice <- log(all$SalePrice) 
psych::skew(all$SalePrice)
qqnorm(all$SalePrice)
qqline(all$SalePrice)
#將資料分成建模用與預測上傳用
train <- modelData[!is.na(all$SalePrice),]
test <- modelData[is.na(all$SalePrice),]

#建模-----
# Lasso regression model
set.seed(27042018)
my_control <-trainControl(method="cv", number=5)
lassoGrid <- expand.grid(alpha = 1, #1 = lasso,0 = ridge
                         lambda = seq(0.001,0.1,by = 0.0005)
                         #懲罰項 越小
                         )

lasso_mod <- train(x=train, 
                   y=all$SalePrice[!is.na(all$SalePrice)], 
                   method='glmnet', 
                   trControl= my_control, 
                   tuneGrid=lassoGrid) 
lasso_mod$bestTune
min(lasso_mod$results$RMSE)#20:0.1134226 10:0.1135765

lassoVarImp <- varImp(lasso_mod,scale=F)
lassoImportance <- lassoVarImp$importance

LassoPred <- predict(lasso_mod, test)

# 驗證直接用caret tune lasso的模型 predict 與
# tune 出來最佳參數並直接用glmnet建模預測看差別
# 答案: 沒差
# library(glmnet)
# 
# lasso = glmnet(x = as.matrix(train), 
#                y = all$SalePrice[!is.na(all$SalePrice)], 
#                alpha = 1, 
#                family = "gaussian")
# LassoPred2 <- predict(lasso, s = 0.0025,newx = as.matrix(test))

predictions_lasso <- exp(LassoPred) 
head(predictions_lasso)

#XGBoost model
xgb_grid = expand.grid(
  nrounds = 1000,
  eta = c(0.05, 0.01),
  max_depth = c(2, 3, 4, 5, 6),
  gamma = 0,
  colsample_bytree=1,
  min_child_weight=c(1, 2, 3, 4 ,5),
  subsample=1
)


#expand.grid 根據給定的值產生全部的資料組合
xgb_caret <- train(x=train[,varsSelected],
                  y=all$SalePrice[!is.na(all$SalePrice)], 
                  method='xgbTree', trControl= my_control, 
                  tuneGrid=xgb_grid) 
xgb_caret$bestTune
#0428
#nrounds max_depth  eta gamma colsample_bytree min_child_weight subsample
#   1000         3 0.05     0                1                5         1

#20200628 20以內
# nrounds max_depth  eta gamma colsample_bytree min_child_weight  subsample
#     1000        2 0.05     0                1                5          1

#20200628 10以內
# nrounds max_depth  eta gamma colsample_bytree  min_child_weight subsample
#   1000         3  0.05     0                1                2         1
# 
                
#2020701 lasso selected 10以內

# nrounds max_depth  eta gamma colsample_bytree min_child_weight subsample
#   1000         3 0.05     0                1               4         1

   

label_train <- all$SalePrice[!is.na(all$SalePrice)]

# 將資料轉換成Xgb.DMatrix
default_param<-list(
  objective = "reg:linear",
  booster = "gbtree",
  eta=0.05, 
  gamma=0,
  max_depth=3,  #原本2
  min_child_weight=2, 
  subsample=1,
  colsample_bytree=1
)

dtrain <- xgb.DMatrix(data = as.matrix(train), 
                      label= label_train)
dtest <- xgb.DMatrix(data = as.matrix(test))

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

#透過CV找到最好的資料組合訓練模型
set.seed(27042018)
xgb_mod <- xgb.train(data = dtrain, 
                     params=default_param, 
                     nrounds = 567)
XGBpred <- predict(xgb_mod, dtest)
predictions_XGB <- exp(XGBpred) 
head(predictions_XGB)
#view variable importance plot
library(Ckmeans.1d.dp) #required for ggplot clustering
mat <- xgb.importance (feature_names = colnames(train),
                       model = xgb_mod)
xgb.ggplot.importance(importance_matrix = mat[1:30], 
                      rel_to_first = TRUE)
sub_avg <- data.frame(Id = test_labels, 
                      SalePrice = (predictions_XGB+2*predictions_lasso)/3)
sub_lasso <- data.frame(Id = test_labels, 
                      SalePrice = predictions_lasso)
sub_xgb <- data.frame(Id = test_labels, 
                        SalePrice = predictions_XGB)

head(sub_avg)
write.csv(sub_avg, 
          file = 'output/20200628/housesale10.csv',
          row.names = F)
write.csv(sub_lasso, file = 'output/20200628/housesale_lasso.csv', row.names = F)
write.csv(sub_xgb, file = 'output/20200628/housesale_xgb.csv', row.names = F)


