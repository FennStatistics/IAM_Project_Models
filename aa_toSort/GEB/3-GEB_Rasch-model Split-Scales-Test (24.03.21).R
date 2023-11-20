
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
# of your own dataset by changing the numbers which represent the columns of your items in command rows 75 and 78. --- items[c(...)] ---

### inverse dichotomous items ###
# items[c(34,35,36,37,38,40)] <- as.data.frame(sapply(items[c(34,35,36,37,38,40)], recode, "0=1; 1=0; NA=NA"))

### inverse polytomous items ###
# items[c(3,4,6,7,10,17,18,23,26,27,28,29,31)] <- as.data.frame(sapply(items[c(3,4,6,7,10,17,18,23,26,27,28,29,31)], recode,  "1=5; 2=4; 3=3; 4=2; 5=1; NA=NA"))

### standard dichotomous items ###
items[c(33:50)] <- as.data.frame(sapply(items[c(33:50)], recode, "0=0; 1=1; NA=NA"))

### standard polytomous items ###
items[c(1:32)] <- as.data.frame(sapply(items[c(1:32)], recode,  "1=0; 2=0; 3=0; 4=1; 5=1; NA=NA"))




#############################################################
###### create subscales and output dataset for results ######
#############################################################

even_indexes<-seq(2,ncol(items),2) 
odd_indexes<-seq(1,ncol(items),2)

scale_even <- items[,even_indexes]
scale_odd <- items[,odd_indexes]

person_results <- data[1]
names(person_results) <- "ID"
person_results$ID <- as.character(person_results$ID)


################################################
###### Rasch analysis scale 1 and scale 2 ######
################################################

if (!require("eRm")) install.packages("eRm")
library(eRm)

RaschModel_even <- RM(scale_even) # this might take a while
pp_even <- person.parameter(RaschModel_even)

RaschModel_odd <- RM(scale_odd) # this might take a while
pp_odd <- person.parameter(RaschModel_odd)


###################################################
###### Obtain person parameters from scale 1 ######
###################################################

### Create person output dataset ###
person_estimates <- data[1]
names(person_estimates) <- "ID"
person_estimates$ID <- as.character(person_estimates$ID)

### find persons with zero or perfect score ###
person_perfect_even <- rownames(scale_even[rowSums(scale_even, na.rm=T) == rowSums(!is.na(scale_even)),])
person_perfect_even <- person_estimates[rownames(person_estimates) %in% person_perfect_even,]
person_zero_even <- rownames(scale_even[rowSums(scale_even, na.rm=T) == 0,])
person_zero_even <- person_estimates[rownames(person_estimates) %in% person_zero_even,]
persons_excluded <- c(person_zero_even, person_perfect_even)
if(length(persons_excluded)==0){persons_excluded<-"noone"} #if there are no persons with zero or perfect score, the output list remains complete

### Obtain person estimates (i.e. logit value representing individual environmental attitude) ###
person_estimates['GEB.est Scale 1'] <- person.parameter(RaschModel_even)$theta.table
person_estimates['GEB.est Scale 1'] <- round(person_estimates['GEB.est Scale 1'], 3)
person_estimates <- as.data.frame(person_estimates[!person_estimates$ID %in% persons_excluded,]) # remove persons from output list
names(person_estimates[1]) <- "ID"
person_estimates$ID <- as.character(person_estimates$ID)

### Obtain person estimates rank ###
person_estimates['Rank Scale 1'] <- rank(person_estimates$`GEB.est Scale 1`)

### person estimate standard error ### 
error <- pp_even$se.theta
error <- unlist(error, use.names=T) # create a list of all Std.errors and their namelabels
names(error) <- sub('.*\\P', '', names(error))
names(error) # only keep number of datarow from orginal data set from the namelabels of Std.errors
error.frame <- data.frame(error)
error.frame['person'] <- as.numeric(names(error)) # create a data frame with Std.errors and extracted datarow numbers
error.frame <- error.frame[order(error.frame$person),] # sort Std.errors by datarow number
person_estimates['Std.Error Scale 1'] <- round(error.frame, 3) # match Std.errors to person_estimates

### Obtain person fit values ###
pfit_even <- personfit(pp_even)
person_estimates['infit MS Scale 1'] <- round(pfit_even$p.infitMSQ, 3)
person_estimates['outfit MS Scale 1'] <- round(pfit_even$p.outfitMSQ, 3)
person_estimates['infit t Scale 1'] <- round(pfit_even$p.infitZ, 3)
person_estimates['outfit t Scale 1'] <- round(pfit_even$p.outfitZ, 3)

### add results from scale to output file ###
person_results <- merge(person_results, person_estimates, by="ID", all.x = T)


###################################################
###### Obtain person parameters from scale 2 ######
###################################################

### Create person output dataset ###
person_estimates <- data[1]
names(person_estimates) <- "ID"
person_estimates$ID <- as.character(person_estimates$ID)

### find persons with zero or perfect score ###
person_perfect_odd <- rownames(scale_odd[rowSums(scale_odd, na.rm=T) == rowSums(!is.na(scale_odd)),])
person_perfect_odd <- person_estimates[rownames(person_estimates) %in% person_perfect_odd,]
person_zero_odd <- rownames(scale_odd[rowSums(scale_odd, na.rm=T) == 0,])
person_zero_odd <- person_estimates[rownames(person_estimates) %in% person_zero_odd,]
persons_excluded <- c(person_zero_odd, person_perfect_odd)
if(length(persons_excluded)==0){persons_excluded<-"noone"} #if there are no persons with zero or perfect score, the output list remains complete

### Obtain person estimates (i.e. logit value representing individual environmental attitude) ###
person_estimates['GEB.est Scale 2'] <- person.parameter(RaschModel_odd)$theta.table
person_estimates['GEB.est Scale 2'] <- round(person_estimates['GEB.est Scale 2'], 3)
person_estimates <- as.data.frame(person_estimates[!person_estimates$ID %in% persons_excluded,]) # remove persons from output list
names(person_estimates[1]) <- "ID"
person_estimates$ID <- as.character(person_estimates$ID)

### Obtain person estimates rank ###
person_estimates['Rank Scale 2'] <- rank(person_estimates$`GEB.est Scale 2`)

### person estimate standard error ### 
error <- pp_odd$se.theta
error <- unlist(error, use.names=T) # create a list of all Std.errors and their namelabels
names(error) <- sub('.*\\P', '', names(error))
names(error) # only keep number of datarow from orginal data set from the namelabels of Std.errors
error.frame <- data.frame(error)
error.frame['person'] <- as.numeric(names(error)) # create a data frame with Std.errors and extracted datarow numbers
error.frame <- error.frame[order(error.frame$person),] # sort Std.errors by datarow number
person_estimates['Std.Error Scale 2'] <- round(error.frame, 3) # match Std.errors to person estimates

### Obtain person fit values ###
pfit_odd <- personfit(pp_odd)
person_estimates['infit MS Scale 2'] <- round(pfit_odd$p.infitMSQ, 3)
person_estimates['outfit MS Scale 2'] <- round(pfit_odd$p.outfitMSQ, 3)
person_estimates['infit t Scale 2'] <- round(pfit_odd$p.infitZ, 3)
person_estimates['outfit t Scale 2'] <- round(pfit_odd$p.outfitZ, 3)

### add results from scale to output file ###
person_results <- merge(person_results, person_estimates, by="ID", all.x = T)


#######################################################
###### calculate difference of person abilities #######
#######################################################

person_results <- person_results[,c(1,2,9,3,10,4,11,5,6,7,8,12,13,14,15)]
person_results['difference of abilities'] <- person_results[2]-person_results[3]


#######################################################
###### correlate person abilities of both scales ######
#######################################################

person_adds <- data.frame()
person_adds[1,1] <- "values of correlation test"
person_adds[3,1] <- "correlation between GEB.est Scale 1 and GEB.est Scale 2"
person_adds[3,2] <- cor.test(person_results$`GEB.est Scale 1`, person_results$`GEB.est Scale 2`, method = "pearson", conf.level = 0.95, na.exclude=T )$estimate
person_adds[4,1] <- "df of pearson correlation test"
person_adds[4,2] <- cor.test(person_results$`GEB.est Scale 1`, person_results$`GEB.est Scale 2`, method = "pearson", conf.level = 0.95, na.exclude=T )$parameter
person_adds[5,1] <- "pearson correlation test statistic"
person_adds[5,2] <- cor.test(person_results$`GEB.est Scale 1`, person_results$`GEB.est Scale 2`, method = "pearson", conf.level = 0.95, na.exclude=T )$statistic
person_adds[6,1] <- "p-value of pearson correlation test"
person_adds[6,2] <- cor.test(person_results$`GEB.est Scale 1`, person_results$`GEB.est Scale 2`, method = "pearson", conf.level = 0.95, na.exclude=T )$p.value

person_adds[3,2] <- round(person_adds[3,2],3)
person_adds[4,2] <- round(person_adds[4,2],1)
person_adds[5,2] <- round(person_adds[5,2],3)
person_adds[6,2] <- round(person_adds[6,2],5)


############################################################
###### Obtain additional person parameters of scale 1 ######
############################################################

person_adds[10,1] <- "Values of Scale 1"
person_adds[12,1] <- "mean of person estimates"
person_adds[12,2] <- round(mean(person_results$`GEB.est Scale 1`, na.rm=T),3)
person_adds[13,1] <- "standard diviation of person estimates"
person_adds[13,2] <- round(sd(person_results$`GEB.est Scale 1`, na.rm=T),3)
person_adds[15,1] <- "mean of infit MS"
person_adds[15,2] <- round(mean(person_results$`infit MS Scale 1`, na.rm=T),3)
person_adds[16,1] <- "standard diviation of infit MS"
person_adds[16,2] <- round(sd(person_results$`infit MS Scale 1`, na.rm=T),3)
person_adds[18,1] <- "mean of infit t"
person_adds[18,2] <- round(mean(person_results$`infit t Scale 1`, na.rm=T),3)
person_adds[19,1] <- "standard diviation of infit t"
person_adds[19,2] <- round(sd(person_results$`infit t Scale 1`, na.rm=T),3)
person_adds[21,1] <- "% Misfit (z-value > 1.96) of subjects"
person_adds[21,2] <- PersonMisfit(pp_even)
person_adds[21,2] <- round(as.numeric(person_adds[21,2]),3)
person_adds[23,1] <- "persons with perfect score (ID)"
person_adds[23,2] <- paste(person_perfect_even, collapse = ' , ')
person_adds[24,1] <- "persons with zero score (ID)"
person_adds[24,2] <- paste(person_zero_even, collapse = ' , ')
person_adds[26,1] <- "person seperation reliability"
person_adds[26,2] <- round(SepRel(person.parameter(RaschModel_even))$sep.rel, 3)


############################################################
###### Obtain additional person parameters of scale 2 ######
############################################################

person_adds[30,1] <- "Values of Scale 2"
person_adds[32,1] <- "mean of person estimates"
person_adds[32,2] <- round(mean(person_results$`GEB.est Scale 2`, na.rm=T),3)
person_adds[33,1] <- "standard diviation of person estimates"
person_adds[33,2] <- round(sd(person_results$`GEB.est Scale 2`, na.rm=T),3)
person_adds[34,1] <- "mean of infit MS"
person_adds[34,2] <- round(mean(person_results$`infit MS Scale 2`, na.rm=T),3)
person_adds[36,1] <- "standard diviation of infit MS"
person_adds[36,2] <- round(sd(person_results$`infit MS Scale 2`, na.rm=T),3)
person_adds[38,1] <- "mean of infit t"
person_adds[38,2] <- round(mean(person_results$`infit t Scale 2`, na.rm=T),3)
person_adds[39,1] <- "standard diviation of infit t"
person_adds[39,2] <- round(sd(person_results$`infit t Scale 2`, na.rm=T),3)
person_adds[41,1] <- "% Misfit (z-value > 1.96) of subjects"
person_adds[41,2] <- PersonMisfit(pp_odd)
person_adds[41,2] <- round(as.numeric(person_adds[41,2]),3)
person_adds[43,1] <- "persons with perfect score (ID)"
person_adds[43,2] <- paste(person_perfect_odd, collapse = ' , ')
person_adds[44,1] <- "persons with zero score (ID)"
person_adds[44,2] <- paste(person_zero_odd, collapse = ' , ')
person_adds[46,1] <- "person seperation reliability"
person_adds[46,2] <- round(SepRel(person.parameter(RaschModel_odd))$sep.rel, 3)


###################################
###### save analysis outputs ######
###################################

write.csv2(person_results, "subscales comparison1.csv")
write.csv2(person_adds, "subscales comparison2.csv")
