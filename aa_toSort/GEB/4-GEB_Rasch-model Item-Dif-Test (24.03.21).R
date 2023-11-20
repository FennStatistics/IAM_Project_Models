
###############################################
###### reading dataset from an SPSS file ######
###############################################

if (!require("rstudioapi")) install.packages("rstudioapi")
library(rstudioapi)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # sets working directory to path of this script

if (!require("foreign")) install.packages("foreign")
library(foreign)

data <- read.spss("2-Dataset.sav", to.data.frame = T) # replace dataset name if you analyse your own data.

if (!require("varhandle")) install.packages("varhandle")
library(varhandle) 

# If you are using your own dataset please adapt the following command row which picks and unfactors the items according to the structure
# of your own dataset by changing the numbers which represent the columns of your items. --- data[...] ---

items <- unfactor(data[7:56])

# Please check if your dataset is structured correctly by executing the following command row. 
# Polytomous items should have 5 levels and dichotomous items should have 2 levels.
# If items have more levels this could indicate, that missing values are not coded as NA, but instead as -99/9/etc.
# If this is the case, please adapt the following "dichotomize response values"-command rows to recode e.g. -99 to NA.

str(data)


################################################################
###### recode responses in word-form into numeric values #######
################################################################

# Please be aware that this script handles response options in numeric form. Response labels 
# of polytomous items should be numeric values ranging from 1 to 5, or "NA" for missing values. 
# Dichotomous items should be coded 0 for "no" and 1 for "yes".
# To check if your response values are coded as numerics please execute the following command row.

str(items)

# If your dataset contains response values as words (e.g. "very often") instead of using numeric values (e.g. 5) please 
# uncommment and use command rows 47 and 49 according to your language and the structure of your dataset to 
# recode your response values into numeric form.

if (!require("car")) install.packages("car")
library(car)

### recoding of all polytomous items (columns 1 to 32 in demonstration dataset) ###
items[c(1:32)] <- as.data.frame(sapply(items[c(1:32)], recode, "'nie'=1; 'selten'=2; 'manchmal'=3; 'meistens'=4; 'immer'=5; NA=NA"))
### recoding of  alldichotomous items (columns 33 to 50 in demonstration dataset) ###
items[c(33:50)] <- as.data.frame(sapply(items[c(33:50)], recode, "'nein'=0; 'ja'=1; NA=NA"))


##########################################
###### dichotomize response values #######
##########################################

# All reponses are dichotomized before Rasch analysis. 
# In standard polytomous items, the two highest response options (representing the response options "often" and "very often") 
# are coded as 1 representing pro-environmental behavior.
# In inverse polytomous items the two lowest response options (representing the response options "never" and "seldom") are 
# coded as 1 representing pro-environmental behavior.
# Standard dichotomous items are not recoded and remain as 0 for disagreement with the item (representing non-environmental responses) 
# and 1 for agreement with the item (representing pro-environmental reponses).
# Inverse dichotomous items are recoded into 0 for agreement with item (representing non-environmental responses) 
# and 1 for disagreement with the item (representing pro-environmental reponses).
# Inverse items in our demonstration dataset are: GEB03, GEB04, GEB06, GEB07, GEB10, GEB17, GEB18, GEB23, GEB26, GEB27, GEB28, GEB29,
# GEB31, GEB34, GEB35, GEB36, GEB37, GEB38, and GEB40.

# If you are using your own dataset please adapt the command rows which dichotomize the response values according to the structure
# of your own dataset by changing the numbers which represent the columns of your items in command rows 76 and 79. --- items[c(...)] ---

### inverse dichotomous items ###
# items[c(34,35,36,37,38,40)] <- as.data.frame(sapply(items[c(34,35,36,37,38,40)], recode, "0=1; 1=0; NA=NA"))

### inverse polytomous items ###
# items[c(3,4,6,7,10,17,18,23,26,27,28,29,31)] <- as.data.frame(sapply(items[c(3,4,6,7,10,17,18,23,26,27,28,29,31)], recode,  "1=5; 2=4; 3=3; 4=2; 5=1; NA=NA"))

### standard dichotomous items ###
items[c(33:50)] <- as.data.frame(sapply(items[c(33:50)], recode, "0=0; 1=1; NA=NA"))

### standard polytomous items ###
items[c(1:32)] <- as.data.frame(sapply(items[c(1:32)], recode,  "1=0; 2=0; 3=0; 4=1; 5=1; NA=NA"))



#############################################################
###### create subgroups and output dataset for results ######
#############################################################

group_one <- items[data$SO01=="weiblich",]
group_two <- items[data$SO01=="m?nnlich",]   # please check if umlaut is written correctly
all  <- items[data$SO01=="m?nnlich" | data$SO01=="weiblich",] # please also check if umlaut is written correctly in this row

item_results <- data.frame(colnames(items))
names(item_results) <- "item"
item_results$item <- as.character(item_results$item)


###########################################
###### Rasch analysis of full sample ######
###########################################

if (!require("eRm")) install.packages("eRm")
library(eRm)

RaschModel <- RM(all)  # this might take a while 


###########################################
###### execute LR-Test and Wald-Test ######
###########################################

if (!require("plyr")) install.packages("plyr")
library(plyr)

all$sex <- data$SO01[data$SO01=="m?nnlich" | data$SO01=="weiblich"] # please check if umlaut is written correctly
all$sex <- revalue(all$sex, c("m?nnlich"=0, "weiblich"=1)) # please check if umlaut is written correctly
all$sex <- as.numeric(all$sex)

LR <- LRtest(RaschModel, splitcr = all$sex)
Wald <- Waldtest(RaschModel, splitcr = all$sex)


###############################################
###### obtain item parameters of group 1 ######
###############################################

### Create item output dataset ###
item_estimates <- data.frame(colnames(items))
names(item_estimates) <- "item"
item_estimates$item <- as.character(item_estimates$item)

### find items with zero or full score responses ###
items_perfect <- colnames(group_one[colSums(group_one, na.rm=T) == colSums(!is.na(group_one))])
items_zero <- colnames(group_one[colSums(group_one, na.rm=T) == 0])
items_excluded <- c(items_zero, items_perfect)
if(length(items_excluded)==0){items_excluded<-"nothing"} #if there are no items with zero or perfect score, the output list remains complete
item_estimates <- as.data.frame(item_estimates[!item_estimates$item == items_excluded,]) # remove items from output list
names(item_estimates) <- "item"
item_estimates$item <- as.character(item_estimates$item)

### obtain item parameters ###
item_estimates['GEB.diff Group 1'] <- round(LR$betalist$`1`*(-1),3) # betapar is the item easiness; inverting it by *(-1) results in item difficulty.
item_estimates['Std.Error Group 1'] <- round(LR$selist$`1`,3)
item_estimates['Rank Group 1'] <- rank(item_estimates$`GEB.diff Group 1`)

### Obtain item fit values ###
library(eRm)
pp <- person.parameter(LR$fitobj$`1`)
ifit <- itemfit(pp)
item_estimates['infit MS Group 1'] <- round(ifit$i.infitMSQ, 3) # meansquare
item_estimates['outfit MS Group 1'] <- round(ifit$i.outfitMSQ, 3)
item_estimates['infit t Group 1'] <- round(ifit$i.infitZ, 3) # z-standardized
item_estimates['outfit t Group 1'] <- round(ifit$i.outfitZ, 3)

### add results from group to output file ###
item_results <- merge(item_results, item_estimates, by="item", all.x = T)


###############################################
###### obtain item parameters of group 2 ######
###############################################

### Create item output dataset ###
item_estimates <- data.frame(colnames(items))
names(item_estimates) <- "item"
item_estimates$item <- as.character(item_estimates$item)

### find items with zero or full score responses ###
items_perfect <- colnames(group_two[colSums(group_two, na.rm=T) == colSums(!is.na(group_two))])
items_zero <- colnames(group_two[colSums(group_two, na.rm=T) == 0])
items_excluded <- c(items_zero, items_perfect)
if(length(items_excluded)==0){items_excluded<-"nothing"} #if there are no items with zero or perfect score, the output list remains complete
item_estimates <- as.data.frame(item_estimates[!item_estimates$item == items_excluded,]) # remove items from output list
names(item_estimates) <- "item"
item_estimates$item <- as.character(item_estimates$item)

### obtain item parameters ###
item_estimates['GEB.diff Group 2'] <- round(LR$betalist$`2`*(-1),3)
item_estimates['Std.Error Group 2'] <- round(LR$selist$`2`,3)
item_estimates['Rank Group 2'] <- rank(item_estimates$`GEB.diff Group 2`)

### Obtain item fit values ###
pp <- person.parameter(LR$fitobj$`2`)
ifit <- itemfit(pp)
item_estimates['infit MS Group 2'] <- round(ifit$i.infitMSQ, 3) # meansquare
item_estimates['outfit MS Group 2'] <- round(ifit$i.outfitMSQ, 3)
item_estimates['infit t Group 2'] <- round(ifit$i.infitZ, 3) # z-standardized
item_estimates['outfit t Group 2'] <- round(ifit$i.outfitZ, 3)

### add results from group to output file ###
item_results <- merge(item_results, item_estimates, by="item", all.x = T)


########################################################
###### calculate difference of item difficulties #######
########################################################

### calculate difference of subgroup-difficulties ###
item_results <- item_results[,c(1,2,9,4,11,3,10,5,6,7,8,12,13,14,15)]
item_results['difference of difficulty'] <- item_results[2]-item_results[3]

### obtain standardised difference and p-values for items ###
item_estimates <- as.data.frame(Wald$coef.table)
rownames(item_estimates) <- sub('.*\\ ', '', rownames(item_estimates))
item_estimates['item'] <- rownames(item_estimates)
colnames(item_estimates) <- c("standardized difference", "p-value", "item")
item_estimates$`standardized difference` <- round(item_estimates$`standardized difference`,3)
item_estimates$`p-value` <- round(item_estimates$`p-value`,3)
item_results <- merge(item_results, item_estimates, by="item", all.x = T)


######################################################
###### obtain Chi-Squared-Test values for model ######
######################################################

item_adds <- data.frame()
item_adds[1,1] <- "chi-squared-test value for model"
item_adds[1,2] <- round(LR$LR,3)
item_adds[2,1] <- "chi-squared-test df for model"
item_adds[2,2] <- LR$df
item_adds[3,1] <- "chi-squared-test p-value for model"
item_adds[3,2] <- round(LR$pvalue,5)


######################################################
###### obtain item reliabilites for both groups ######
######################################################

item_adds[5,1] <- "item seperation reliability group 1"
item_adds[5,2] <- round( (var(item_results$`GEB.diff Group 1`, na.rm=T) - sum((item_results$`Std.Error Group 1`)^2, na.rm=T) / sum(!is.na(item_results$`Std.Error Group 1`)))/var(item_results$`GEB.diff Group 1`, na.rm=T) ,3)
item_adds[6,1] <- "item seperation reliability group 2"
item_adds[6,2] <- round( (var(item_results$`GEB.diff Group 2`, na.rm=T) - sum((item_results$`Std.Error Group 2`)^2, na.rm=T) / sum(!is.na(item_results$`Std.Error Group 2`)))/var(item_results$`GEB.diff Group 2`, na.rm=T) ,3)


##########################
###### save results ######
##########################

write.csv2(item_results, "itemdif comparison1.csv")
write.csv2(item_adds, "itemdif comparison2.csv")
