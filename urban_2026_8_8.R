# Code for gas_v?.tex titled: "How Consumers Pay for Gasoline"
# This code uses the 2021 to 2025 data

# Packages used
#library(haven)# may be needed to handle haven labelled when data is converted from other software e.g. Stata
library(ggplot2); theme_set(theme_bw())# for graphics
library(scales)# to have % in ggplot axes and tables and for commas , in data frame numbers
#library(nnet)# multinomial regressions
library("xtable") #exporting to LaTeX
library(dplyr)# for sample_n
#library(ineq)# GINI coefficient
#library(Hmisc)# for cutting data into bins

# regression packages
#library(gtools)# for stars.pval function
#library(brglm2)# modifies glm. Useful when glm warns about fitting 0 and 1
#library(logistf)# stronger bias reduction to eliminate the glm warning (when brglm2 cannot eliminate it)
library(mfx)# binomial logit marginal effects
#library(stargazer) # for displaying multinomial coefficients. Does not work with mfx
library(texreg) # for displaying multinomial coefficients. Works with mfx (unlike stargazer). Also displays multiple regression.
#library(huxtable)#displays multiple regressions as table => advantage, since the table can be edited in R => Problem: built-in to_latex output does not run in LaTeX in my experience. 

# machine learning packages
#library(caret)#for confusionMatrix. Note: it flips the orientation so it makes actual (reference) as columns and predicted (data) as rows => unconventional. 
# ML packages
library(rpart)
library(rpart.plot)
library(partykit)# modifies rpart tree plot
library("randomForest")
#library("missForest")#to impute NAs, preparation for random forest (not used because cannot impute based on selected variables (no var selection.)
#library(mice)# for imputations. => cannot impute a large number of NA?
library(missRanger)# for imputations based on "amnt" and "merch only

#2025 data load
setwd("C:/Oz_local_workplace_3_Urban/urban_R")
dir()
# Read transaction dataset
trans_1.df = readRDS("dcpc_2025_tranlevel_public.rds")
dim(trans_1.df)
names(trans_1.df)
(sampled_num_trans = nrow(trans_1.df))

# Read individual dataset
indiv_1.df = readRDS("dcpc_2025_indlevel_public.rds")
dim(indiv_1.df)
names(indiv_1.df)
(sampled_num_resp = nrow(indiv_1.df))

#Refining the indiv dataset
# delete unneeded variables from indiv dataset
i_2.df = subset(indiv_1.df, select = c(id, ind_weight_all, income_hh, hhincome, age, cc_adopt, dc_adopt, cc_rewards, urban_cat, gender, highest_education, race, hispaniclatino))
names(i_2.df)
sum(is.na(i_2.df))
sum(is.na(i_2.df$ind_weight_all))
sum(is.na(i_2.df$id))
sum(is.na(i_2.df$income_hh))#continuous income
sum(is.na(i_2.df$hhincome))#16 income cat
sum(is.na(i_2.df$age))
sum(is.na(i_2.df$cc_adopt))
sum(is.na(i_2.df$dc_adopt))
sum(is.na(i_2.df$cc_rewards))#missing the most
sum(is.na(i_2.df$urban_cat))
sum(is.na(i_2.df$gender))
sum(is.na(i_2.df$highest_education))
sum(is.na(i_2.df$race))
sum(is.na(i_2.df$hispaniclatino))

#
# rename urban categories
i_3.df = i_2.df
table(i_3.df$urban_cat, useNA = "always")
str(i_3.df$urban_cat)
i_3.df$urban_cat = as.factor(i_3.df$urban_cat)
levels(i_3.df$urban_cat)
levels(i_3.df$urban_cat) = c("Rural", "Mixed", "Urban")
str(i_3.df$urban_cat)
table(i_3.df$urban_cat, useNA = "always")
# rename urban_cat to Area
i_3.df = i_3.df %>% rename("Area" = "urban_cat")
#
# condense education categories => new variable = Education
# rename urban categories
table(i_3.df$highest_education, useNA = "always")
str(i_3.df$highest_education)
i_4.df = i_3.df

i_4.df$Education = NA
i_4.df = i_4.df %>%
  mutate(Education = ifelse(highest_education < 11, "High_school_or_less", Education))# including some college
i_4.df = i_4.df %>%
  mutate(Education = ifelse(highest_education == 11 | highest_education == 12, "Associate_degree", Education))
i_4.df = i_4.df %>%
  mutate(Education = ifelse(highest_education == 13, "College_degree", Education))
i_4.df = i_4.df %>%
  mutate(Education = ifelse(highest_education > 13 , "Graduate_degree", Education))
table(i_4.df$Education, useNA = "always")
i_4.df$Education = as.factor(i_4.df$Education)
levels(i_4.df$Education)
i_4.df$Education = factor(i_4.df$Education, levels = c("High_school_or_less", "Associate_degree", "College_degree", "Graduate_degree"))

# rename Gender category
table(i_4.df$gender, useNA = "always")
i_4.df = i_4.df %>% 
  mutate(gender = ifelse(gender==0, "Female", gender))
i_4.df = i_4.df %>% 
  mutate(gender = ifelse(gender==1, "Male", gender))
table(i_4.df$gender, useNA = "always")
i_4.df = i_4.df %>% rename("Gender" = "gender")
i_4.df$Gender = as.factor(i_4.df$Gender)
names(i_4.df)

# rename and condense race categories
table(i_4.df$race, useNA = "always")
i_4.df = i_4.df %>% 
  mutate(race = ifelse(race==1, "White", race))
i_4.df = i_4.df %>% 
  mutate(race = ifelse(race==2, "Black", race))
i_4.df = i_4.df %>% 
  mutate(race = ifelse(race==4, "Asian", race))
i_4.df = i_4.df %>% 
  mutate(race = ifelse(race==3 | race==5 | race==6, "Other/combo", race))
i_4.df = i_4.df %>% rename("Race" = "race")
i_4.df$Race = as.factor(i_4.df$Race)
table(i_4.df$Race, useNA = "always")
levels(i_4.df$Race)
i_4.df$Race = factor(i_4.df$Race, levels = c("White", "Asian", "Black", "Other/combo"))

# rename and condense hispanic/latino categories
table(i_4.df$hispaniclatino, useNA = "always")
i_4.df = i_4.df %>% 
  mutate(hispaniclatino = ifelse(hispaniclatino==1, "Yes", hispaniclatino))
i_4.df = i_4.df %>% 
  mutate(hispaniclatino = ifelse(hispaniclatino==0, "No", hispaniclatino))
i_4.df = i_4.df %>% rename("Hispanic" = "hispaniclatino")
i_4.df$Hispanic = as.factor(i_4.df$Hispanic)

# Renaming all other indiv variables
names(i_4.df)
i_5.df = i_4.df
i_5.df = i_5.df %>% rename("Own_credit" = "cc_adopt", "Credit_reward" = "cc_rewards",  "Own_debit" = "dc_adopt", "HH_income" = "income_hh", "HH_income_cat" = "hhincome", "Age" = "age")
names(i_5.df)
#
table(i_5.df$Own_credit, useNA = "always")
i_5.df = i_5.df %>% 
   mutate(Own_credit = case_when(
     Own_credit == 0 ~ "No",
     Own_credit == 1 ~ "Yes"
   ))
table(i_5.df$Own_credit, useNA = "always")
#
table(i_5.df$Own_debit, useNA = "always")
i_5.df = i_5.df %>% 
  mutate(Own_debit = case_when(
    Own_debit == 0 ~ "No",
    Own_debit == 1 ~ "Yes"
  ))
table(i_5.df$Own_debit, useNA = "always")
#
table(i_5.df$Credit_reward, useNA = "always")
i_5.df = i_5.df %>% 
  mutate(Credit_reward = case_when(
    Credit_reward == 0 ~ "No",
    Credit_reward == 1 ~ "Yes"
  ))
table(i_5.df$Credit_reward, useNA = "always")


# select variables for the trans dataset
t_2.df = subset(trans_1.df, select = c(id, pi, merch, amnt, device, mobile_app, in_person, bill ))
dim(t_2.df)

# merge indiv data into trans data
m1.df = left_join(t_2.df, i_5.df, by = "id")
dim(m1.df)
names(m1.df)

# remove payments with missing obs of: pi, amnt,ind_weight_all. Also, delete transactions with amount = 0 (if any)
sum(is.na(m1.df$pi))
sum(is.na(m1.df$amnt))
nrow(subset(m1.df, amnt <=0))
sum(is.na(m1.df$ind_weight_all))
sum(is.na(m1.df$Area))
sum(is.na(m1.df$in_person))
sum(is.na(m1.df$bill))
dim(m1.df)
m2.df = subset(m1.df, !is.na(m1.df$pi) & !is.na(m1.df$amnt) & m1.df$amnt >0 & !is.na(m1.df$ind_weight_all) & !is.na(m1.df$Area)  & !is.na(m1.df$in_person)  & !is.na(m1.df$bill))
nrow(m2.df)-nrow(m1.df)# num obs removed

# rescale the weights to sum up to num obs: add column w
nrow(m2.df)
sum(m2.df$ind_weight_all)
m2.df$w = nrow(m2.df) * m2.df$ind_weight_all/sum(m2.df$ind_weight_all)
sum(m2.df$w)# verify w sum to num obs

## subsetting w.r.t. urban/mixed/rural
m4.df = m2.df
table(m4.df$Area, useNA = "always")

# rural data frame
rural1.df = subset(m4.df, Area == "Rural")
dim(rural1.df)
sum(rural1.df$w)
# # make rural weights: wr => Rescaling not needed
# rural1.df$wr = nrow(rural1.df)*rural1.df$w / sum(rural1.df$w)
# sum(rural1.df$wr)
# nrow(rural1.df)

# mixed data frame
mixed1.df = subset(m4.df, Area == "Mixed")
dim(mixed1.df)
sum(mixed1.df$w)
# # make mixed weights: wm  => Rescaling not needed
# mixed1.df$wm = nrow(mixed1.df)*mixed1.df$w / sum(mixed1.df$w)
# sum(mixed1.df$wm)
# nrow(mixed1.df)

# urban data frame
urban1.df = subset(m4.df, Area == "Urban")
dim(urban1.df)
sum(urban1.df$w)
# # make urban weights: wm => Rescaling not needed
# urban1.df$wm = nrow(urban1.df)*urban1.df$w / sum(urban1.df$w)
# sum(urban1.df$wm)
# nrow(urban1.df)

#End: Reading, merging, renaming, refining data####

#+++++++++++++++++++++++

#Begin: Table 1: Type of payments####

# extracting info from rural
(rural_num_total = nrow(rural1.df))# num rural payments
length(unique(rural1.df$id))# num resp
# rural in-person
table(rural1.df$in_person)
(rural_num_ip = nrow(subset(rural1.df, in_person==1)))
(rural_num_not_ip = nrow(subset(rural1.df, in_person==0)))
(rural_frac_ip = nrow(subset(rural1.df, in_person==1))/rural_num_total)
(rural_frac_not_ip = nrow(subset(rural1.df, in_person==0))/rural_num_total)
#rural bill
table(rural1.df$bill)
(rural_num_bill = nrow(subset(rural1.df, bill==1)))
(rural_num_not_bill = nrow(subset(rural1.df, bill==0)))
(rural_frac_bill = nrow(subset(rural1.df, bill==1))/rural_num_total)
(rural_frac_not_bill = nrow(subset(rural1.df, bill==0))/rural_num_total)
#rural in-person & bill
(rural_num_ip_bill = nrow(subset(rural1.df, in_person==1 & bill==1)))
(rural_num_ip_not_bill = nrow(subset(rural1.df, in_person==1 & bill==0)))
(rural_frac_ip_bill = nrow(subset(rural1.df, in_person==1 & bill==1))/rural_num_ip)
(rural_frac_ip_not_bill = nrow(subset(rural1.df, in_person==1 & bill==0))/rural_num_ip)
#rural not in-person & bill
(rural_num_not_ip_bill = nrow(subset(rural1.df, in_person==0 & bill==1)))
(rural_num_not_ip_not_bill = nrow(subset(rural1.df, in_person==0 & bill==0)))
(rural_frac_not_ip_bill = nrow(subset(rural1.df, in_person==0 & bill==1))/rural_num_not_ip)
(rural_frac_not_ip_not_bill = nrow(subset(rural1.df, in_person==0 & bill==0))/rural_num_not_ip)

# extracting info from mixed
(mixed_num_total = nrow(mixed1.df))# num mixed payments
length(unique(mixed1.df$id))# num resp
# mixed in-person
table(mixed1.df$in_person)
(mixed_num_ip = nrow(subset(mixed1.df, in_person==1)))
(mixed_num_not_ip = nrow(subset(mixed1.df, in_person==0)))
(mixed_frac_ip = nrow(subset(mixed1.df, in_person==1))/mixed_num_total)
(mixed_frac_not_ip = nrow(subset(mixed1.df, in_person==0))/mixed_num_total)
#mixed bill
table(mixed1.df$bill)
(mixed_num_bill = nrow(subset(mixed1.df, bill==1)))
(mixed_num_not_bill = nrow(subset(mixed1.df, bill==0)))
(mixed_frac_bill = nrow(subset(mixed1.df, bill==1))/mixed_num_total)
(mixed_frac_not_bill = nrow(subset(mixed1.df, bill==0))/mixed_num_total)
#mixed in-person & bill
(mixed_num_ip_bill = nrow(subset(mixed1.df, in_person==1 & bill==1)))
(mixed_num_ip_not_bill = nrow(subset(mixed1.df, in_person==1 & bill==0)))
(mixed_frac_ip_bill = nrow(subset(mixed1.df, in_person==1 & bill==1))/mixed_num_ip)
(mixed_frac_ip_not_bill = nrow(subset(mixed1.df, in_person==1 & bill==0))/mixed_num_ip)
#mixed not in-person & bill
(mixed_num_not_ip_bill = nrow(subset(mixed1.df, in_person==0 & bill==1)))
(mixed_num_not_ip_not_bill = nrow(subset(mixed1.df, in_person==0 & bill==0)))
(mixed_frac_not_ip_bill = nrow(subset(mixed1.df, in_person==0 & bill==1))/mixed_num_not_ip)
(mixed_frac_not_ip_not_bill = nrow(subset(mixed1.df, in_person==0 & bill==0))/mixed_num_not_ip)

# extracting info from urban
(urban_num_total = nrow(urban1.df))# num urban payments
length(unique(urban1.df$id))# num resp
# urban in-person
table(urban1.df$in_person)
(urban_num_ip = nrow(subset(urban1.df, in_person==1)))
(urban_num_not_ip = nrow(subset(urban1.df, in_person==0)))
(urban_frac_ip = nrow(subset(urban1.df, in_person==1))/urban_num_total)
(urban_frac_not_ip = nrow(subset(urban1.df, in_person==0))/urban_num_total)
#urban bill
table(urban1.df$bill)
(urban_num_bill = nrow(subset(urban1.df, bill==1)))
(urban_num_not_bill = nrow(subset(urban1.df, bill==0)))
(urban_frac_bill = nrow(subset(urban1.df, bill==1))/urban_num_total)
(urban_frac_not_bill = nrow(subset(urban1.df, bill==0))/urban_num_total)
#urban in-person & bill
(urban_num_ip_bill = nrow(subset(urban1.df, in_person==1 & bill==1)))
(urban_num_ip_not_bill = nrow(subset(urban1.df, in_person==1 & bill==0)))
(urban_frac_ip_bill = nrow(subset(urban1.df, in_person==1 & bill==1))/urban_num_ip)
(urban_frac_ip_not_bill = nrow(subset(urban1.df, in_person==1 & bill==0))/urban_num_ip)
#urban not in-person & bill
(urban_num_not_ip_bill = nrow(subset(urban1.df, in_person==0 & bill==1)))
(urban_num_not_ip_not_bill = nrow(subset(urban1.df, in_person==0 & bill==0)))
(urban_frac_not_ip_bill = nrow(subset(urban1.df, in_person==0 & bill==1))/urban_num_not_ip)
(urban_frac_not_ip_not_bill = nrow(subset(urban1.df, in_person==0 & bill==0))/urban_num_not_ip)

#preparing columns for urban.df
#in person yes and no (two columns)
(in_person.vec = c(rural_num_ip, mixed_num_ip, urban_num_ip, rural_frac_ip, mixed_frac_ip, urban_frac_ip))
(not_in_person.vec = c(rural_num_not_ip, mixed_num_not_ip, urban_num_not_ip, rural_frac_not_ip, mixed_frac_not_ip, urban_frac_not_ip))
#bill yes and no (2 columnes)
(bill.vec = c(rural_num_bill, mixed_num_bill, urban_num_bill, rural_frac_bill, mixed_frac_bill, urban_frac_bill))
(not_bill.vec = c(rural_num_not_bill, mixed_num_not_bill, urban_num_not_bill, rural_frac_not_bill, mixed_frac_not_bill, urban_frac_not_bill))
# in person & bill versus nonbill (2 columns)
(inperson_bill.vec = c(rural_num_ip_bill, mixed_num_ip_bill, urban_num_ip_bill, rural_frac_ip_bill, mixed_frac_ip_bill, urban_frac_ip_bill))
(inperson_not_bill.vec = c(rural_num_ip_not_bill, mixed_num_ip_not_bill, urban_num_ip_not_bill, rural_frac_ip_not_bill, mixed_frac_ip_not_bill, urban_frac_ip_not_bill))
# not in person & bill versus nonbill (2 columns)
(not_inperson_bill.vec = c(rural_num_not_ip_bill, mixed_num_not_ip_bill, urban_num_not_ip_bill, rural_frac_not_ip_bill, mixed_frac_not_ip_bill, urban_frac_not_ip_bill))
(not_inperson_not_bill.vec = c(rural_num_not_ip_not_bill, mixed_num_not_ip_not_bill, urban_num_not_ip_not_bill, rural_frac_not_ip_not_bill, mixed_frac_not_ip_not_bill, urban_frac_not_ip_not_bill))

# Combine the above into a data frame
(pay_type.df = data.frame(ip_yes = in_person.vec, ip_no = not_in_person.vec, bill_yes = bill.vec, bill_no = not_bill.vec, ip_yes_bill_yes = inperson_bill.vec, ip_yes_bill_no = inperson_not_bill.vec, ip_no_bill_yes = not_inperson_bill.vec, ip_no_bill_no = not_inperson_not_bill.vec))

# verifying the above table (sum to total?)
rural_num_total
mixed_num_total
urban_num_total
pay_type.df$ip_yes + pay_type.df$ip_no
pay_type.df$bill_yes + pay_type.df$bill_no
#
pay_type.df$ip_yes
pay_type.df$ip_yes_bill_yes + pay_type.df$ip_yes_bill_no
#
pay_type.df$ip_no
pay_type.df$ip_no_bill_yes + pay_type.df$ip_no_bill_no
#

## Construct a version with weights (to be appended below the above)
# rural (weighted)
(rural_num_total = nrow(rural1.df))# num rural payments
length(unique(rural1.df$id))# num resp
sum(rural1.df$w)
# rural in-person
table(rural1.df$in_person)
(rural_num_ip_w = sum(subset(rural1.df, in_person==1)$w))
(rural_num_not_ip_w = sum(subset(rural1.df, in_person==0)$w))
(rural_frac_ip_w = rural_num_ip_w/sum(rural1.df$w))
(rural_frac_not_ip_w = rural_num_not_ip_w/sum(rural1.df$w))
rural_frac_ip_w + rural_frac_not_ip_w
#rural bill
table(rural1.df$bill)
(rural_num_bill_w = sum(subset(rural1.df, bill==1)$w))
(rural_num_not_bill_w = sum(subset(rural1.df, bill==0)$w))
(rural_frac_bill_w = rural_num_bill_w/sum(rural1.df$w))
(rural_frac_not_bill_w = rural_num_not_bill_w/sum(rural1.df$w))
rural_frac_bill_w + rural_frac_not_bill_w
#rural in-person & bill
(rural_num_ip_bill_w = sum(subset(rural1.df, in_person==1 & bill==1)$w))
(rural_num_ip_not_bill_w = sum(subset(rural1.df, in_person==1 & bill==0)$w))
(rural_frac_ip_bill_w = rural_num_ip_bill_w/rural_num_ip_w )
(rural_frac_ip_not_bill_w = rural_num_ip_not_bill_w/rural_num_ip_w )
rural_frac_ip_bill_w + rural_frac_ip_not_bill_w
#rural not in-person & bill
(rural_num_not_ip_bill_w = sum(subset(rural1.df, in_person==0 & bill==1)$w))
(rural_num_not_ip_not_bill_w = sum(subset(rural1.df, in_person==0 & bill==0)$w))
(rural_frac_not_ip_bill_w = rural_num_not_ip_bill_w/rural_num_not_ip_w)
(rural_frac_not_ip_not_bill_w = rural_num_not_ip_not_bill_w/rural_num_not_ip_w)
rural_frac_not_ip_bill_w + rural_frac_not_ip_not_bill

# mixed (weighted)
(mixed_num_total = nrow(mixed1.df))# num mixed payments
length(unique(mixed1.df$id))# num resp
sum(mixed1.df$w)
# mixed in-person
table(mixed1.df$in_person)
(mixed_num_ip_w = sum(subset(mixed1.df, in_person==1)$w))
(mixed_num_not_ip_w = sum(subset(mixed1.df, in_person==0)$w))
(mixed_frac_ip_w = mixed_num_ip_w/sum(mixed1.df$w))
(mixed_frac_not_ip_w = mixed_num_not_ip_w/sum(mixed1.df$w))
mixed_frac_ip_w + mixed_frac_not_ip_w
#mixed bill
table(mixed1.df$bill)
(mixed_num_bill_w = sum(subset(mixed1.df, bill==1)$w))
(mixed_num_not_bill_w = sum(subset(mixed1.df, bill==0)$w))
(mixed_frac_bill_w = mixed_num_bill_w/sum(mixed1.df$w))
(mixed_frac_not_bill_w = mixed_num_not_bill_w/sum(mixed1.df$w))
mixed_frac_bill_w + mixed_frac_not_bill_w
#mixed in-person & bill
(mixed_num_ip_bill_w = sum(subset(mixed1.df, in_person==1 & bill==1)$w))
(mixed_num_ip_not_bill_w = sum(subset(mixed1.df, in_person==1 & bill==0)$w))
(mixed_frac_ip_bill_w = mixed_num_ip_bill_w/mixed_num_ip_w )
(mixed_frac_ip_not_bill_w = mixed_num_ip_not_bill_w/mixed_num_ip_w )
mixed_frac_ip_bill_w + mixed_frac_ip_not_bill_w
#mixed not in-person & bill
(mixed_num_not_ip_bill_w = sum(subset(mixed1.df, in_person==0 & bill==1)$w))
(mixed_num_not_ip_not_bill_w = sum(subset(mixed1.df, in_person==0 & bill==0)$w))
(mixed_frac_not_ip_bill_w = mixed_num_not_ip_bill_w/mixed_num_not_ip_w)
(mixed_frac_not_ip_not_bill_w = mixed_num_not_ip_not_bill_w/mixed_num_not_ip_w)
mixed_frac_not_ip_bill_w + mixed_frac_not_ip_not_bill

# urban (weighted)
(urban_num_total = nrow(urban1.df))# num urban payments
length(unique(urban1.df$id))# num resp
sum(urban1.df$w)
# urban in-person
table(urban1.df$in_person)
(urban_num_ip_w = sum(subset(urban1.df, in_person==1)$w))
(urban_num_not_ip_w = sum(subset(urban1.df, in_person==0)$w))
(urban_frac_ip_w = urban_num_ip_w/sum(urban1.df$w))
(urban_frac_not_ip_w = urban_num_not_ip_w/sum(urban1.df$w))
urban_frac_ip_w + urban_frac_not_ip_w
#urban bill
table(urban1.df$bill)
(urban_num_bill_w = sum(subset(urban1.df, bill==1)$w))
(urban_num_not_bill_w = sum(subset(urban1.df, bill==0)$w))
(urban_frac_bill_w = urban_num_bill_w/sum(urban1.df$w))
(urban_frac_not_bill_w = urban_num_not_bill_w/sum(urban1.df$w))
urban_frac_bill_w + urban_frac_not_bill_w
#urban in-person & bill
(urban_num_ip_bill_w = sum(subset(urban1.df, in_person==1 & bill==1)$w))
(urban_num_ip_not_bill_w = sum(subset(urban1.df, in_person==1 & bill==0)$w))
(urban_frac_ip_bill_w = urban_num_ip_bill_w/urban_num_ip_w )
(urban_frac_ip_not_bill_w = urban_num_ip_not_bill_w/urban_num_ip_w )
urban_frac_ip_bill_w + urban_frac_ip_not_bill_w
#urban not in-person & bill
(urban_num_not_ip_bill_w = sum(subset(urban1.df, in_person==0 & bill==1)$w))
(urban_num_not_ip_not_bill_w = sum(subset(urban1.df, in_person==0 & bill==0)$w))
(urban_frac_not_ip_bill_w = urban_num_not_ip_bill_w/urban_num_not_ip_w)
(urban_frac_not_ip_not_bill_w = urban_num_not_ip_not_bill_w/urban_num_not_ip_w)
urban_frac_not_ip_bill_w + urban_frac_not_ip_not_bill

# Extending the columns adding 3 rows with weights to the vectors
#
#extending columns for urban.df
#in person yes and no (2 columns)
(in_person2.vec = c(rural_num_ip, mixed_num_ip, urban_num_ip, rural_frac_ip, mixed_frac_ip, urban_frac_ip, rural_frac_ip_w, mixed_frac_ip_w, urban_frac_ip_w))
(not_in_person2.vec = c(rural_num_not_ip, mixed_num_not_ip, urban_num_not_ip, rural_frac_not_ip, mixed_frac_not_ip, urban_frac_not_ip, rural_frac_not_ip_w, mixed_frac_not_ip_w, urban_frac_not_ip_w))
#bill yes and no (2 columnes)
(bill2.vec = c(rural_num_bill, mixed_num_bill, urban_num_bill, rural_frac_bill, mixed_frac_bill, urban_frac_bill, rural_frac_bill_w, mixed_frac_bill_w, urban_frac_bill_w))
(not_bill2.vec = c(rural_num_not_bill, mixed_num_not_bill, urban_num_not_bill, rural_frac_not_bill, mixed_frac_not_bill, urban_frac_not_bill, rural_frac_not_bill_w, mixed_frac_not_bill_w, urban_frac_not_bill_w))
# in person & bill versus nonbill (2 columns)
(inperson_bill2.vec = c(rural_num_ip_bill, mixed_num_ip_bill, urban_num_ip_bill, rural_frac_ip_bill, mixed_frac_ip_bill, urban_frac_ip_bill, rural_frac_ip_bill_w, mixed_frac_ip_bill_w, urban_frac_ip_bill_w))
(inperson_not_bill2.vec = c(rural_num_ip_not_bill, mixed_num_ip_not_bill, urban_num_ip_not_bill, rural_frac_ip_not_bill, mixed_frac_ip_not_bill, urban_frac_ip_not_bill, rural_frac_ip_not_bill_w, mixed_frac_ip_not_bill_w, urban_frac_ip_not_bill_w))
# not in person & bill versus nonbill (2 columns)
(not_inperson_bill2.vec = c(rural_num_not_ip_bill, mixed_num_not_ip_bill, urban_num_not_ip_bill, rural_frac_not_ip_bill, mixed_frac_not_ip_bill, urban_frac_not_ip_bill, rural_frac_not_ip_bill_w, mixed_frac_not_ip_bill_w, urban_frac_not_ip_bill_w))
(not_inperson_not_bill2.vec = c(rural_num_not_ip_not_bill, mixed_num_not_ip_not_bill, urban_num_not_ip_not_bill, rural_frac_not_ip_not_bill, mixed_frac_not_ip_not_bill, urban_frac_not_ip_not_bill, rural_frac_not_ip_not_bill_w, mixed_frac_not_ip_not_bill_w, urban_frac_not_ip_not_bill_w))

# Combine w-inclusive vectors the above into a data frame
(pay_type2.df = data.frame(ip_yes = in_person2.vec, ip_no = not_in_person2.vec, bill_yes = bill2.vec, bill_no = not_bill2.vec, ip_yes_bill_yes = inperson_bill2.vec, ip_yes_bill_no = inperson_not_bill2.vec, ip_no_bill_yes = not_inperson_bill2.vec, ip_no_bill_no = not_inperson_not_bill2.vec))

# verifying the above table (sum to total?)
rural_num_total
mixed_num_total
urban_num_total
pay_type2.df$ip_yes + pay_type2.df$ip_no
pay_type2.df$bill_yes + pay_type2.df$bill_no
#
pay_type2.df$ip_yes
pay_type2.df$ip_yes_bill_yes + pay_type2.df$ip_yes_bill_no
#
pay_type2.df$ip_no
pay_type2.df$ip_no_bill_yes + pay_type2.df$ip_no_bill_no
#

# make is a LaTeX table
dim(pay_type2.df)
# 1st column with area
(area.vec = c("Rural", "Mixed", "Urban","Rural", "Mixed", "Urban","Rural", "Mixed", "Urban"))
(pay_type3.df = cbind(Area = area.vec, pay_type2.df))

#p-values for z-test percentages comparing rural with urban only
#comparing FRAC IN-PERSON rural vs. urban => sig
# two-proportion z-test (pooled)
rural_num_ip# rural in person
urban_num_ip# urban in person
rural_num_total# total rural
urban_num_total# total urban
prop.test(x = c(rural_num_ip, urban_num_ip),
          n = c(rural_num_total, urban_num_total),
          correct = FALSE)   # disable Yates correction to get pure z-test
#
#comparing FRAC BILLS rural vs. urban => not sig
# two-proportion z-test (pooled)
rural_num_bill# rural bills
urban_num_bill# urban bills
rural_num_total# total rural
urban_num_total# total urban
prop.test(x = c(rural_num_bill, urban_num_bill),
          n = c(rural_num_total, urban_num_total),
          correct = FALSE)   # disable Yates correction to get pure z-test
#
#comparing inperson: FRAC BILLs vs. NONBILLs rural vs. urban => not sig
# two-proportion z-test (pooled)
rural_num_ip_bill
urban_num_ip_bill# 
rural_num_ip# total rural in person
urban_num_ip# total urban in person
prop.test(x = c(rural_num_ip_bill, urban_num_ip_bill),
          n = c(rural_num_ip, urban_num_ip),
          correct = FALSE)   # disable Yates correction to get pure z-test
#comparing remote: FRAC BILLs vs. NONBILLs rural vs. urban => sig
# two-proportion z-test (pooled)
rural_num_not_ip_bill
urban_num_not_ip_bill# 
rural_num_not_ip# total rural remote
urban_num_not_ip# total urban remote
prop.test(x = c(rural_num_not_ip_bill, urban_num_not_ip_bill),
          n = c(rural_num_not_ip, urban_num_not_ip),
          correct = FALSE)   # disable Yates correction to get pure z-test

pay_type3.df[4:9, ] = lapply(pay_type3.df[4:9, ], function(x) {
  if (is.numeric(x)) sprintf("%.0f%%", x * 100) else x
})
#Explanation
#%.0f means format as a number with 0 decimal places.
#x * 100 converts the fraction to a percentage.
#%% prints the literal percent sign.

# Create a digits matrix: (rows + 0) × (columns + 1)
# First row of digits is for rownames
#(digits_matrix <- matrix(0, nrow = nrow(pay_type2.df)+ 0, ncol = ncol(pay_type2.df) + 1))
# Rows 4–9 get 2 digits (shifted by +1 because of the rownames)
#digits_matrix[4:9, 2:9] = 0

# Check: digits_matrix aligns as [rownames + rows] × [rownames + columns]
print(xtable(pay_type3.df, digits=0), include.rownames = F, hline.after = c(0,3,6))

#End: Table 1: Type of payments####

#++++++++++++++++++++++++++++++

#Begin: Table 2: Adoption of credit and debit cards####
# It is sufficient to use the indiv dataset only => more respondents, although more missing data.
names(i_5.df)
nrow(i_5.df)# num resp
length(unique(i_5.df$id))# num resp

# divide indiv resp by area
table(i_5.df$Area, useNA = "always")
i_6.df = subset(i_5.df, !is.na(Area))# remove 2 area NAs
table(i_6.df$Area, useNA = "always")
#
# rescale weights to equal num resp
nrow(i_6.df)
sum(i_6.df$ind_weight_all)
# new weights, call it wi before splitting into area
i_6.df$wi = nrow(i_6.df)*i_6.df$ind_weight_all/sum(i_6.df$ind_weight_all)
sum(i_6.df$wi)

# impute NAs for Own_credit, Own_debit, and Credit_reward (better to impute to preserve consistent sample size) => Don't impute, because it assigns all NAs to YES. Instead, display NAs
names(i_6.df)
table(i_6.df$Own_credit, useNA = "always")
table(i_6.df$Own_debit, useNA = "always")
table(i_6.df$Credit_reward, useNA = "always")
i_7.df = i_6.df
# i_7.df = missRanger(data = i_7.df, formula = Own_credit ~ HH_income_cat + Age + Area + Gender + highest_education)
# i_7.df = missRanger(data = i_7.df, formula = Own_debit ~ HH_income_cat + Age + Area + Gender + highest_education)
# i_7.df = missRanger(data = i_7.df, formula = Credit_reward ~ HH_income_cat + Age + Area + Gender + highest_education)
# table(i_7.df$Own_credit, useNA = "always")
# table(i_7.df$Own_debit, useNA = "always")
# table(i_7.df$Credit_reward, useNA = "always")

#
#splitting into areas
irural1.df = subset(i_7.df, Area == "Rural")
dim(irural1.df)
imixed1.df = subset(i_7.df, Area == "Mixed")
dim(imixed1.df)
iurban1.df = subset(i_7.df, Area == "Urban")
dim(iurban1.df)

# rural Own debit
table(irural1.df$Own_debit, useNA = "always")#Own debit
(irural_num_debit_yes = nrow(subset(irural1.df, Own_debit == "Yes")))
(irural_num_debit_no = nrow(subset(irural1.df, Own_debit == "No")))
(irural_num_debit_na = nrow(subset(irural1.df, is.na(Own_debit))))
(irural_frac_debit_yes = irural_num_debit_yes/nrow(irural1.df))
(irural_frac_debit_no = irural_num_debit_no/nrow(irural1.df))
(irural_frac_debit_na = irural_num_debit_na/nrow(irural1.df))
(irural_num_debit_yes_wi = round(sum(subset(irural1.df, Own_debit == "Yes")$wi)))#weighted
(irural_num_debit_no_wi = sum(subset(irural1.df, Own_debit == "No")$wi))
(irural_num_debit_na_wi = nrow(subset(irural1.df, is.na(Own_debit))))
(irural_frac_debit_yes_wi = irural_num_debit_yes_wi/sum(irural1.df$wi))
(irural_frac_debit_no_wi = irural_num_debit_no_wi/sum(irural1.df$wi))
(irural_frac_debit_na_wi = irural_num_debit_na_wi/sum(irural1.df$wi))

# rural Own credit
table(irural1.df$Own_credit, useNA = "always")#Own credit
(irural_num_credit_yes = nrow(subset(irural1.df, Own_credit == "Yes")))
(irural_num_credit_no = nrow(subset(irural1.df, Own_credit == "No")))
(irural_num_credit_na = nrow(subset(irural1.df, is.na(Own_credit))))
(irural_frac_credit_yes = irural_num_credit_yes/nrow(irural1.df))
(irural_frac_credit_no = irural_num_credit_no/nrow(irural1.df))
(irural_frac_credit_na = irural_num_credit_na/nrow(irural1.df))
(irural_num_credit_yes_wi = round(sum(subset(irural1.df, Own_credit == "Yes")$wi)))#weighted
(irural_num_credit_no_wi = sum(subset(irural1.df, Own_credit == "No")$wi))
(irural_num_credit_na_wi = nrow(subset(irural1.df, is.na(Own_credit))))
(irural_frac_credit_yes_wi = irural_num_credit_yes_wi/sum(irural1.df$wi))
(irural_frac_credit_no_wi = irural_num_credit_no_wi/sum(irural1.df$wi))
(irural_frac_credit_na_wi = irural_num_credit_na_wi/sum(irural1.df$wi))

# rural reward
table(irural1.df$Credit_reward, useNA = "always")#credit reward
(irural_num_reward_yes = nrow(subset(irural1.df, Credit_reward == "Yes")))
(irural_num_reward_no = nrow(subset(irural1.df, Credit_reward == "No")))
(irural_num_reward_na = nrow(subset(irural1.df, is.na(Credit_reward))))
(irural_frac_reward_yes = irural_num_reward_yes/nrow(irural1.df))
(irural_frac_reward_no = irural_num_reward_no/nrow(irural1.df))
(irural_frac_reward_na = irural_num_reward_na/nrow(irural1.df))
(irural_num_reward_yes_wi = round(sum(subset(irural1.df, Credit_reward == "Yes")$wi)))#weighted
(irural_num_reward_no_wi = sum(subset(irural1.df, Credit_reward == "No")$wi))
(irural_num_reward_na_wi = nrow(subset(irural1.df, is.na(Credit_reward))))
(irural_frac_reward_yes_wi = irural_num_reward_yes_wi/sum(irural1.df$wi))
(irural_frac_reward_no_wi = irural_num_reward_no_wi/sum(irural1.df$wi))
(irural_frac_reward_na_wi = irural_num_reward_na_wi/sum(irural1.df$wi))

# mixed Own debit
table(imixed1.df$Own_debit, useNA = "always")#Own debit
(imixed_num_debit_yes = nrow(subset(imixed1.df, Own_debit == "Yes")))
(imixed_num_debit_no = nrow(subset(imixed1.df, Own_debit == "No")))
(imixed_num_debit_na = nrow(subset(imixed1.df, is.na(Own_debit))))
(imixed_frac_debit_yes = imixed_num_debit_yes/nrow(imixed1.df))
(imixed_frac_debit_no = imixed_num_debit_no/nrow(imixed1.df))
(imixed_frac_debit_na = imixed_num_debit_na/nrow(imixed1.df))
(imixed_num_debit_yes_wi = round(sum(subset(imixed1.df, Own_debit == "Yes")$wi)))#weighted
(imixed_num_debit_no_wi = sum(subset(imixed1.df, Own_debit == "No")$wi))
(imixed_num_debit_na_wi = nrow(subset(imixed1.df, is.na(Own_debit))))
(imixed_frac_debit_yes_wi = imixed_num_debit_yes_wi/sum(imixed1.df$wi))
(imixed_frac_debit_no_wi = imixed_num_debit_no_wi/sum(imixed1.df$wi))
(imixed_frac_debit_na_wi = imixed_num_debit_na_wi/sum(imixed1.df$wi))

# mixed Own credit
table(imixed1.df$Own_credit, useNA = "always")#Own credit
(imixed_num_credit_yes = nrow(subset(imixed1.df, Own_credit == "Yes")))
(imixed_num_credit_no = nrow(subset(imixed1.df, Own_credit == "No")))
(imixed_num_credit_na = nrow(subset(imixed1.df, is.na(Own_credit))))
(imixed_frac_credit_yes = imixed_num_credit_yes/nrow(imixed1.df))
(imixed_frac_credit_no = imixed_num_credit_no/nrow(imixed1.df))
(imixed_frac_credit_na = imixed_num_credit_na/nrow(imixed1.df))
(imixed_num_credit_yes_wi = round(sum(subset(imixed1.df, Own_credit == "Yes")$wi)))#weighted
(imixed_num_credit_no_wi = sum(subset(imixed1.df, Own_credit == "No")$wi))
(imixed_num_credit_na_wi = nrow(subset(imixed1.df, is.na(Own_credit))))
(imixed_frac_credit_yes_wi = imixed_num_credit_yes_wi/sum(imixed1.df$wi))
(imixed_frac_credit_no_wi = imixed_num_credit_no_wi/sum(imixed1.df$wi))
(imixed_frac_credit_na_wi = imixed_num_credit_na_wi/sum(imixed1.df$wi))

# mixed reward
table(imixed1.df$Credit_reward, useNA = "always")#credit reward
(imixed_num_reward_yes = nrow(subset(imixed1.df, Credit_reward == "Yes")))
(imixed_num_reward_no = nrow(subset(imixed1.df, Credit_reward == "No")))
(imixed_num_reward_na = nrow(subset(imixed1.df, is.na(Credit_reward))))
(imixed_frac_reward_yes = imixed_num_reward_yes/nrow(imixed1.df))
(imixed_frac_reward_no = imixed_num_reward_no/nrow(imixed1.df))
(imixed_frac_reward_na = imixed_num_reward_na/nrow(imixed1.df))
(imixed_num_reward_yes_wi = round(sum(subset(imixed1.df, Credit_reward == "Yes")$wi)))#weighted
(imixed_num_reward_no_wi = sum(subset(imixed1.df, Credit_reward == "No")$wi))
(imixed_num_reward_na_wi = nrow(subset(imixed1.df, is.na(Credit_reward))))
(imixed_frac_reward_yes_wi = imixed_num_reward_yes_wi/sum(imixed1.df$wi))
(imixed_frac_reward_no_wi = imixed_num_reward_no_wi/sum(imixed1.df$wi))
(imixed_frac_reward_na_wi = imixed_num_reward_na_wi/sum(imixed1.df$wi))

# urban Own debit
table(iurban1.df$Own_debit, useNA = "always")#Own debit
(iurban_num_debit_yes = nrow(subset(iurban1.df, Own_debit == "Yes")))
(iurban_num_debit_no = nrow(subset(iurban1.df, Own_debit == "No")))
(iurban_num_debit_na = nrow(subset(iurban1.df, is.na(Own_debit))))
(iurban_frac_debit_yes = iurban_num_debit_yes/nrow(iurban1.df))
(iurban_frac_debit_no = iurban_num_debit_no/nrow(iurban1.df))
(iurban_frac_debit_na = iurban_num_debit_na/nrow(iurban1.df))
(iurban_num_debit_yes_wi = round(sum(subset(iurban1.df, Own_debit == "Yes")$wi)))#weighted
(iurban_num_debit_no_wi = sum(subset(iurban1.df, Own_debit == "No")$wi))
(iurban_num_debit_na_wi = nrow(subset(iurban1.df, is.na(Own_debit))))
(iurban_frac_debit_yes_wi = iurban_num_debit_yes_wi/sum(iurban1.df$wi))
(iurban_frac_debit_no_wi = iurban_num_debit_no_wi/sum(iurban1.df$wi))
(iurban_frac_debit_na_wi = iurban_num_debit_na_wi/sum(iurban1.df$wi))

# urban Own credit
table(iurban1.df$Own_credit, useNA = "always")#Own credit
(iurban_num_credit_yes = nrow(subset(iurban1.df, Own_credit == "Yes")))
(iurban_num_credit_no = nrow(subset(iurban1.df, Own_credit == "No")))
(iurban_num_credit_na = nrow(subset(iurban1.df, is.na(Own_credit))))
(iurban_frac_credit_yes = iurban_num_credit_yes/nrow(iurban1.df))
(iurban_frac_credit_no = iurban_num_credit_no/nrow(iurban1.df))
(iurban_frac_credit_na = iurban_num_credit_na/nrow(iurban1.df))
(iurban_num_credit_yes_wi = round(sum(subset(iurban1.df, Own_credit == "Yes")$wi)))#weighted
(iurban_num_credit_no_wi = sum(subset(iurban1.df, Own_credit == "No")$wi))
(iurban_num_credit_na_wi = nrow(subset(iurban1.df, is.na(Own_credit))))
(iurban_frac_credit_yes_wi = iurban_num_credit_yes_wi/sum(iurban1.df$wi))
(iurban_frac_credit_no_wi = iurban_num_credit_no_wi/sum(iurban1.df$wi))
(iurban_frac_credit_na_wi = iurban_num_credit_na_wi/sum(iurban1.df$wi))

# urban reward
table(iurban1.df$Credit_reward, useNA = "always")#credit reward
(iurban_num_reward_yes = nrow(subset(iurban1.df, Credit_reward == "Yes")))
(iurban_num_reward_no = nrow(subset(iurban1.df, Credit_reward == "No")))
(iurban_num_reward_na = nrow(subset(iurban1.df, is.na(Credit_reward))))
(iurban_frac_reward_yes = iurban_num_reward_yes/nrow(iurban1.df))
(iurban_frac_reward_no = iurban_num_reward_no/nrow(iurban1.df))
(iurban_frac_reward_na = iurban_num_reward_na/nrow(iurban1.df))
(iurban_num_reward_yes_wi = round(sum(subset(iurban1.df, Credit_reward == "Yes")$wi)))#weighted
(iurban_num_reward_no_wi = sum(subset(iurban1.df, Credit_reward == "No")$wi))
(iurban_num_reward_na_wi = nrow(subset(iurban1.df, is.na(Credit_reward))))
(iurban_frac_reward_yes_wi = iurban_num_reward_yes_wi/sum(iurban1.df$wi))
(iurban_frac_reward_no_wi = iurban_num_reward_no_wi/sum(iurban1.df$wi))
(iurban_frac_reward_na_wi = iurban_num_reward_na_wi/sum(iurban1.df$wi))

# prepare vector columns for data frame
(debit_yes.vec = c(irural_num_debit_yes, imixed_num_debit_yes, iurban_num_debit_yes, irural_frac_debit_yes, imixed_frac_debit_yes, iurban_frac_debit_yes, irural_frac_debit_yes_wi, imixed_frac_debit_yes_wi, iurban_frac_debit_yes_wi))
#
(debit_no.vec = c(irural_num_debit_no, imixed_num_debit_no, iurban_num_debit_no, irural_frac_debit_no, imixed_frac_debit_no, iurban_frac_debit_no, irural_frac_debit_no_wi, imixed_frac_debit_no_wi, iurban_frac_debit_no_wi))
#
(debit_na.vec = c(irural_num_debit_na, imixed_num_debit_na, iurban_num_debit_na, irural_frac_debit_na, imixed_frac_debit_na, iurban_frac_debit_na, irural_frac_debit_na_wi, imixed_frac_debit_na_wi, iurban_frac_debit_na_wi))
#verify sum of debit columns
debit_yes.vec + debit_no.vec + debit_na.vec
c(nrow(irural1.df), nrow(imixed1.df), nrow(iurban1.df))
#
(credit_yes.vec = c(irural_num_credit_yes, imixed_num_credit_yes, iurban_num_credit_yes, irural_frac_credit_yes, imixed_frac_credit_yes, iurban_frac_credit_yes, irural_frac_credit_yes_wi, imixed_frac_credit_yes_wi, iurban_frac_credit_yes_wi))
#
(credit_no.vec = c(irural_num_credit_no, imixed_num_credit_no, iurban_num_credit_no, irural_frac_credit_no, imixed_frac_credit_no, iurban_frac_credit_no, irural_frac_credit_no_wi, imixed_frac_credit_no_wi, iurban_frac_credit_no_wi))
#
(credit_na.vec = c(irural_num_credit_na, imixed_num_credit_na, iurban_num_credit_na, irural_frac_credit_na, imixed_frac_credit_na, iurban_frac_credit_na, irural_frac_credit_na_wi, imixed_frac_credit_na_wi, iurban_frac_credit_na_wi))
#verify sum of credit columns
credit_yes.vec + credit_no.vec + credit_na.vec
c(nrow(irural1.df), nrow(imixed1.df), nrow(iurban1.df))
#
(reward_yes.vec = c(irural_num_reward_yes, imixed_num_reward_yes, iurban_num_reward_yes, irural_frac_reward_yes, imixed_frac_reward_yes, iurban_frac_reward_yes, irural_frac_reward_yes_wi, imixed_frac_reward_yes_wi, iurban_frac_reward_yes_wi))
#
(reward_no.vec = c(irural_num_reward_no, imixed_num_reward_no, iurban_num_reward_no, irural_frac_reward_no, imixed_frac_reward_no, iurban_frac_reward_no, irural_frac_reward_no_wi, imixed_frac_reward_no_wi, iurban_frac_reward_no_wi))
#
(reward_na.vec = c(irural_num_reward_na, imixed_num_reward_na, iurban_num_reward_na, irural_frac_reward_na, imixed_frac_reward_na, iurban_frac_reward_na, irural_frac_reward_na_wi, imixed_frac_reward_na_wi, iurban_frac_reward_na_wi))
#verify sum of reward columns
reward_yes.vec + reward_no.vec + reward_na.vec
c(nrow(irural1.df), nrow(imixed1.df), nrow(iurban1.df))

# make it data frame
(adopt1.df = data.frame(debit_yes.vec, debit_no.vec, debit_na.vec, credit_yes.vec, credit_no.vec, credit_na.vec, reward_yes.vec, reward_no.vec, reward_na.vec))

#set digits
adopt2.df = adopt1.df
adopt2.df[4:9, ] = lapply(adopt2.df[4:9, ], function(x) {
  if (is.numeric(x)) sprintf("%.0f%%", x * 100) else x
})
adopt2.df

(area.vec = c("Rural", "Mixed", "Urban","Rural", "Mixed", "Urban","Rural", "Mixed", "Urban"))
(adopt3.df = cbind(Area = area.vec, adopt2.df))

print(xtable(adopt3.df, digits=0), include.rownames = F, hline.after = c(0,3,6))

# info for Table 2 (notes)
nrow(i_6.df)# num resp
nrow(irural1.df)# num rural resp
nrow(imixed1.df)# num mixed resp
nrow(iurban1.df)# num urban resp
nrow(irural1.df) + nrow(imixed1.df) + nrow(iurban1.df)# verify sum

#p-values for z-test percentages comparing rural with urban only
#comparing FRAC IN-PERSON rural vs. urban => sig
# two-proportion z-test (pooled)
irural_num_debit_yes# rural debit yes
iurban_num_debit_yes# rural debit yes
nrow(irural1.df)# total rural
nrow(iurban1.df)# total urban
prop.test(x = c(irural_num_debit_yes, iurban_num_debit_yes),
          n = c(nrow(irural1.df), nrow(iurban1.df)),
          correct = FALSE)   # disable Yates correction to get pure z-test
#
irural_num_debit_no# rural debit no
iurban_num_debit_no# rural debit no
nrow(irural1.df)# total rural
nrow(iurban1.df)# total urban
prop.test(x = c(irural_num_debit_no, iurban_num_debit_no),
          n = c(nrow(irural1.df), nrow(iurban1.df)),
          correct = FALSE)   # disable Yates correction to get pure z-test
#
irural_num_credit_yes# rural credit yes
iurban_num_credit_yes# rural credit yes
nrow(irural1.df)# total rural
nrow(iurban1.df)# total urban
prop.test(x = c(irural_num_credit_yes, iurban_num_credit_yes),
          n = c(nrow(irural1.df), nrow(iurban1.df)),
          correct = FALSE)   # disable Yates correction to get pure z-test
#
irural_num_credit_no# rural credit no
iurban_num_credit_no# rural credit no
nrow(irural1.df)# total rural
nrow(iurban1.df)# total urban
prop.test(x = c(irural_num_credit_no, iurban_num_credit_no),
          n = c(nrow(irural1.df), nrow(iurban1.df)),
          correct = FALSE)   # disable Yates correction to get pure z-test

#
irural_num_reward_yes# rural reward yes
iurban_num_reward_yes# rural reward yes
nrow(irural1.df)# total rural
nrow(iurban1.df)# total urban
prop.test(x = c(irural_num_reward_yes, iurban_num_reward_yes),
          n = c(nrow(irural1.df), nrow(iurban1.df)),
          correct = FALSE)   # disable Yates correction to get pure z-test
#
irural_num_reward_no# rural reward no
iurban_num_reward_no# rural reward no
nrow(irural1.df)# total rural
nrow(iurban1.df)# total urban
prop.test(x = c(irural_num_reward_no, iurban_num_reward_no),
          n = c(nrow(irural1.df), nrow(iurban1.df)),
          correct = FALSE)   # disable Yates correction to get pure z-test


#End: Table 2: Adoption of credit and debit cards####

#++++++++++++++++++++++++++++++

#Begin: Table 3: Adoption regressions####

names(i_6.df)# count missing obs
sum(is.na(i_6.df$Area))
sum(is.na(i_6.df$HH_income))
with(i_6.df, table(is.na(HH_income), Area))# missing HH_income by Area => more missing in Urban, but numbers are small
sum(is.na(i_6.df$Age))
sum(is.na(i_6.df$Gender))
sum(is.na(i_6.df$Race))
sum(is.na(i_6.df$Hispanic))
sum(is.na(i_6.df$Education))
sum(is.na(i_6.df$Own_debit))#depending var
with(i_6.df, table(is.na(Own_debit), Area))# evenly across areas (small)
sum(is.na(i_6.df$Own_credit))#depending var
with(i_6.df, table(is.na(Own_credit), Area))# evenly across areas (small)
sum(is.na(i_6.df$Credit_reward))#depending var (large number)
with(i_6.df, table(is.na(Credit_reward), Area))# relatively more missing @ rural (large)
table(i_6.df$Race, useNA = "always")
table(i_6.df$Hispanic, useNA = "always")

#need to rename dependent var to 0-1
i_6_reg.df = i_6.df
table(i_6_reg.df$Own_debit, useNA = "always")
i_6_reg.df = i_6_reg.df %>% mutate(Own_debit = case_when(
  Own_debit ==  "No" ~ 0,
  Own_debit ==  "Yes" ~ 1
  ))
table(i_6_reg.df$Own_debit, useNA = "always")
#
table(i_6_reg.df$Own_credit, useNA = "always")
i_6_reg.df = i_6_reg.df %>% mutate(Own_credit = case_when(
  Own_credit ==  "No" ~ 0,
  Own_credit ==  "Yes" ~ 1
))
table(i_6_reg.df$Own_credit, useNA = "always")
#
table(i_6_reg.df$Credit_reward, useNA = "always")
i_6_reg.df = i_6_reg.df %>% mutate(Credit_reward = case_when(
  Credit_reward ==  "No" ~ 0,
  Credit_reward ==  "Yes" ~ 1
))
table(i_6_reg.df$Credit_reward, useNA = "always")
#
# Construct a new variable HH_income_10k measurign incomes in $10k units
i_6_reg.df$HH_income_10k = as.numeric(i_6_reg.df$HH_income/10000)
summary(i_6_reg.df$HH_income_10k)

# verify factor variables
str(i_6_reg.df)
i_6_reg.df$Age = as.integer(i_6_reg.df$Age)
i_6_reg.df$Own_credit = as.factor(i_6_reg.df$Own_credit)
i_6_reg.df$Own_debit = as.factor(i_6_reg.df$Own_debit)
i_6_reg.df$Credit_reward = as.factor(i_6_reg.df$Credit_reward)
i_6_reg.df$Gender = as.factor(i_6_reg.df$Gender)
i_6_reg.df$Race = as.factor(i_6_reg.df$Race)
i_6_reg.df$Hispanic = as.factor(i_6_reg.df$Hispanic)


# 3 logit regression models: Own debit, own credit, own reward
debit.model = Own_debit ~ Area + HH_income_10k + Age +Gender + Education + Race + Hispanic
#
credit.model = Own_credit ~ Area + HH_income_10k + Age +Gender + Education + Race + Hispanic
#
reward.model = Credit_reward ~ Area + HH_income_10k + Age +Gender + Education + Race + Hispanic

#Own debit regression R mfx yielding 0,1 warning
(debit.reg = logitmfx(debit.model, data = i_6_reg.df, atmean = F))
#Own credit regression R mfx yielding 0,1 warning
(credit.reg = logitmfx(credit.model, data = i_6_reg.df, atmean = F))
#Own credit regression R mfx yielding 0,1 warning
(reward.reg = logitmfx(reward.model, data = i_6_reg.df, atmean = F))

#construct a LaTeX table
screenreg(list(debit.reg, credit.reg, reward.reg), digits = 3,
          stars  = c(0.001, 0.01, 0.05, 0.10),  # add 10% cutoff
          symbol = ".",
          custom.model.names = c("Own debit", "Own credit", "Credit reward"))#texreg package => screen
#
texreg(list(debit.reg, credit.reg, reward.reg), digits = 3,
       stars  = c(0.001, 0.01, 0.05, 0.10),  # add 10% cutoff
       symbol = ".",
       custom.model.names = c("Own debit", "Own credit", "Credit reward"))#texreg package => LaTeX

# Reference variables (factor variables)
levels(i_6_reg.df$Area)
levels(i_6_reg.df$Gender)
levels(i_6_reg.df$Education)

#End: Table 3: Adoption regressions####

#++++++++++++++++++++++++++++++

#Begin: Table 4: In-person payments####

table(rural1.df$in_person)
table(mixed1.df$in_person)
table(urban1.df$in_person)

#subset to in person (ip) only
ip_rural1.df = subset(rural1.df, in_person==1)
dim(ip_rural1.df)
ip_mixed1.df = subset(mixed1.df, in_person==1)
dim(ip_mixed1.df)
ip_urban1.df = subset(urban1.df, in_person==1)
dim(ip_urban1.df)

table(ip_rural1.df$pi, useNA = "always")
table(ip_mixed1.df$pi, useNA = "always")
table(ip_urban1.df$pi, useNA = "always")
round(prop.table(table(ip_rural1.df$pi, useNA = "always")),2)
round(prop.table(table(ip_mixed1.df$pi, useNA = "always")),2)
round(prop.table(table(ip_urban1.df$pi, useNA = "always")),2)

# PI renaming rural
ip_rural1.df$pi = as.numeric(ip_rural1.df$pi)
ip_rural1.df = ip_rural1.df %>% mutate(pi = ifelse(pi > 10, "Other", pi))# must come first b/c logical will not work after some are replaced by text
ip_rural1.df = ip_rural1.df %>% mutate(pi = ifelse(pi==0, "Other", pi))
ip_rural1.df = ip_rural1.df %>% mutate(pi = ifelse(pi==6, "Other", pi))
ip_rural1.df = ip_rural1.df %>% mutate(pi = ifelse(pi==7, "Other", pi))
ip_rural1.df = ip_rural1.df %>% mutate(pi = ifelse(pi==8, "Other", pi))
ip_rural1.df = ip_rural1.df %>% mutate(pi = ifelse(pi==9, "Other", pi))
ip_rural1.df = ip_rural1.df %>% mutate(pi = ifelse(pi==1, "Cash", pi))
ip_rural1.df = ip_rural1.df %>% mutate(pi = ifelse(pi==2, "Check", pi))
ip_rural1.df = ip_rural1.df %>% mutate(pi = ifelse(pi==3, "Credit card", pi))
ip_rural1.df = ip_rural1.df %>% mutate(pi = ifelse(pi==4, "Debit card", pi))
ip_rural1.df = ip_rural1.df %>% mutate(pi = ifelse(pi==5, "Prepaid card", pi))
ip_rural1.df = ip_rural1.df %>% mutate(pi = ifelse(pi==10, "Mobile app", pi))
round(prop.table(table(ip_rural1.df$pi, useNA = "always")),2)
ip_rural1.df$pi = as.factor(ip_rural1.df$pi)
levels(ip_rural1.df$pi)
ip_rural1.df$pi = factor(ip_rural1.df$pi, levels = c("Cash", "Credit card", "Debit card", "Prepaid card", "Check", "Mobile app", "Other" ))
levels(ip_rural1.df$pi)
table(ip_rural1.df$pi)

# PI renaming mixed
ip_mixed1.df$pi = as.numeric(ip_mixed1.df$pi)
ip_mixed1.df = ip_mixed1.df %>% mutate(pi = ifelse(pi > 10, "Other", pi))# must come first b/c logical will not work after some are replaced by text
ip_mixed1.df = ip_mixed1.df %>% mutate(pi = ifelse(pi==0, "Other", pi))
ip_mixed1.df = ip_mixed1.df %>% mutate(pi = ifelse(pi==6, "Other", pi))
ip_mixed1.df = ip_mixed1.df %>% mutate(pi = ifelse(pi==7, "Other", pi))
ip_mixed1.df = ip_mixed1.df %>% mutate(pi = ifelse(pi==8, "Other", pi))
ip_mixed1.df = ip_mixed1.df %>% mutate(pi = ifelse(pi==9, "Other", pi))
ip_mixed1.df = ip_mixed1.df %>% mutate(pi = ifelse(pi==1, "Cash", pi))
ip_mixed1.df = ip_mixed1.df %>% mutate(pi = ifelse(pi==2, "Check", pi))
ip_mixed1.df = ip_mixed1.df %>% mutate(pi = ifelse(pi==3, "Credit card", pi))
ip_mixed1.df = ip_mixed1.df %>% mutate(pi = ifelse(pi==4, "Debit card", pi))
ip_mixed1.df = ip_mixed1.df %>% mutate(pi = ifelse(pi==5, "Prepaid card", pi))
ip_mixed1.df = ip_mixed1.df %>% mutate(pi = ifelse(pi==10, "Mobile app", pi))
round(prop.table(table(ip_mixed1.df$pi, useNA = "always")),2)
ip_mixed1.df$pi = as.factor(ip_mixed1.df$pi)
levels(ip_mixed1.df$pi)
ip_mixed1.df$pi = factor(ip_mixed1.df$pi, levels = c("Cash", "Credit card", "Debit card", "Prepaid card", "Check", "Mobile app", "Other" ))
levels(ip_mixed1.df$pi)
table(ip_mixed1.df$pi)

# PI renaming urban
ip_urban1.df$pi = as.numeric(ip_urban1.df$pi)
ip_urban1.df = ip_urban1.df %>% mutate(pi = ifelse(pi > 10, "Other", pi))# must come first b/c logical will not work after some are replaced by text
ip_urban1.df = ip_urban1.df %>% mutate(pi = ifelse(pi==0, "Other", pi))
ip_urban1.df = ip_urban1.df %>% mutate(pi = ifelse(pi==6, "Other", pi))
ip_urban1.df = ip_urban1.df %>% mutate(pi = ifelse(pi==7, "Other", pi))
ip_urban1.df = ip_urban1.df %>% mutate(pi = ifelse(pi==8, "Other", pi))
ip_urban1.df = ip_urban1.df %>% mutate(pi = ifelse(pi==9, "Other", pi))
ip_urban1.df = ip_urban1.df %>% mutate(pi = ifelse(pi==1, "Cash", pi))
ip_urban1.df = ip_urban1.df %>% mutate(pi = ifelse(pi==2, "Check", pi))
ip_urban1.df = ip_urban1.df %>% mutate(pi = ifelse(pi==3, "Credit card", pi))
ip_urban1.df = ip_urban1.df %>% mutate(pi = ifelse(pi==4, "Debit card", pi))
ip_urban1.df = ip_urban1.df %>% mutate(pi = ifelse(pi==5, "Prepaid card", pi))
ip_urban1.df = ip_urban1.df %>% mutate(pi = ifelse(pi==10, "Mobile app", pi))
round(prop.table(table(ip_urban1.df$pi, useNA = "always")),2)
ip_urban1.df$pi = as.factor(ip_urban1.df$pi)
levels(ip_urban1.df$pi)
ip_urban1.df$pi = factor(ip_urban1.df$pi, levels = c("Cash", "Credit card", "Debit card", "Prepaid card", "Check", "Mobile app", "Other" ))
levels(ip_urban1.df$pi)
table(ip_urban1.df$pi)

# num payments by PI by area
(ip_rural_num.vec = unname(table(ip_rural1.df$pi)))#unname the table
(ip_mixed_num.vec = unname(table(ip_mixed1.df$pi)))#unname the table
(ip_urban_num.vec = unname(table(ip_urban1.df$pi)))#unname the table

# frac payments by PI by area
(ip_rural_frac.vec = ip_rural_num.vec/nrow(ip_rural1.df) )
(ip_mixed_frac.vec = ip_mixed_num.vec/nrow(ip_mixed1.df) )
(ip_urban_frac.vec = ip_urban_num.vec/nrow(ip_urban1.df) )
# verify sum to 1
sum(ip_rural_frac.vec)
sum(ip_mixed_frac.vec)
sum(ip_urban_frac.vec)

# weighted num payments rural
table(ip_rural1.df$pi)
(ip_rural_num_cash_w = sum(subset(ip_rural1.df, pi=="Cash")$w))
(ip_rural_num_credit_w = sum(subset(ip_rural1.df, pi=="Credit card")$w))
(ip_rural_num_debit_w = sum(subset(ip_rural1.df, pi=="Debit card")$w))
(ip_rural_num_prepaid_w = sum(subset(ip_rural1.df, pi=="Prepaid card")$w))
(ip_rural_num_check_w = sum(subset(ip_rural1.df, pi=="Check")$w))
(ip_rural_num_mobile_w = sum(subset(ip_rural1.df, pi=="Mobile app")$w))
(ip_rural_num_other_w = sum(subset(ip_rural1.df, pi!="Cash" & pi!="Credit card" & pi!="Debit card" & pi!="Prepaid card" & pi!="Check" & pi!="Mobile app")$w))

# weighted fraction payments rural
table(ip_rural1.df$pi)
(ip_rural_frac_cash_w = sum(subset(ip_rural1.df, pi=="Cash")$w)/sum(ip_rural1.df$w))
(ip_rural_frac_credit_w = sum(subset(ip_rural1.df, pi=="Credit card")$w)/sum(ip_rural1.df$w))
(ip_rural_frac_debit_w = sum(subset(ip_rural1.df, pi=="Debit card")$w)/sum(ip_rural1.df$w))
(ip_rural_frac_prepaid_w = sum(subset(ip_rural1.df, pi=="Prepaid card")$w)/sum(ip_rural1.df$w))
(ip_rural_frac_check_w = sum(subset(ip_rural1.df, pi=="Check")$w)/sum(ip_rural1.df$w))
(ip_rural_frac_mobile_w = sum(subset(ip_rural1.df, pi=="Mobile app")$w)/sum(ip_rural1.df$w))
(ip_rural_frac_other_w = sum(subset(ip_rural1.df, pi!="Cash" & pi!="Credit card" & pi!="Debit card" & pi!="Prepaid card" & pi!="Check" & pi!="Mobile app")$w)/sum(ip_rural1.df$w))
# verify sum up to 1
ip_rural_frac_cash_w+ip_rural_frac_credit_w+ip_rural_frac_debit_w+ip_rural_frac_prepaid_w+ip_rural_frac_check_w+ip_rural_frac_mobile_w+ip_rural_frac_other_w

# weighted num payments mixed
table(ip_mixed1.df$pi)
(ip_mixed_num_cash_w = sum(subset(ip_mixed1.df, pi=="Cash")$w))
(ip_mixed_num_credit_w = sum(subset(ip_mixed1.df, pi=="Credit card")$w))
(ip_mixed_num_debit_w = sum(subset(ip_mixed1.df, pi=="Debit card")$w))
(ip_mixed_num_prepaid_w = sum(subset(ip_mixed1.df, pi=="Prepaid card")$w))
(ip_mixed_num_check_w = sum(subset(ip_mixed1.df, pi=="Check")$w))
(ip_mixed_num_mobile_w = sum(subset(ip_mixed1.df, pi=="Mobile app")$w))
(ip_mixed_num_other_w = sum(subset(ip_mixed1.df, pi!="Cash" & pi!="Credit card" & pi!="Debit card" & pi!="Prepaid card" & pi!="Check" & pi!="Mobile app")$w))

# weighted fraction payments mixed
table(ip_mixed1.df$pi)
(ip_mixed_frac_cash_w = sum(subset(ip_mixed1.df, pi=="Cash")$w)/sum(ip_mixed1.df$w))
(ip_mixed_frac_credit_w = sum(subset(ip_mixed1.df, pi=="Credit card")$w)/sum(ip_mixed1.df$w))
(ip_mixed_frac_debit_w = sum(subset(ip_mixed1.df, pi=="Debit card")$w)/sum(ip_mixed1.df$w))
(ip_mixed_frac_prepaid_w = sum(subset(ip_mixed1.df, pi=="Prepaid card")$w)/sum(ip_mixed1.df$w))
(ip_mixed_frac_check_w = sum(subset(ip_mixed1.df, pi=="Check")$w)/sum(ip_mixed1.df$w))
(ip_mixed_frac_mobile_w = sum(subset(ip_mixed1.df, pi=="Mobile app")$w)/sum(ip_mixed1.df$w))
(ip_mixed_frac_other_w = sum(subset(ip_mixed1.df, pi!="Cash" & pi!="Credit card" & pi!="Debit card" & pi!="Prepaid card" & pi!="Check" & pi!="Mobile app")$w)/sum(ip_mixed1.df$w))
# verify sum up to 1
ip_mixed_frac_cash_w+ip_mixed_frac_credit_w+ip_mixed_frac_debit_w+ip_mixed_frac_prepaid_w  +ip_mixed_frac_check_w+ip_mixed_frac_mobile_w+ip_mixed_frac_other_w

# weighted num payments urban
table(ip_urban1.df$pi)
(ip_urban_num_cash_w = sum(subset(ip_urban1.df, pi=="Cash")$w))
(ip_urban_num_credit_w = sum(subset(ip_urban1.df, pi=="Credit card")$w))
(ip_urban_num_debit_w = sum(subset(ip_urban1.df, pi=="Debit card")$w))
(ip_urban_num_prepaid_w = sum(subset(ip_urban1.df, pi=="Prepaid card")$w))
(ip_urban_num_check_w = sum(subset(ip_urban1.df, pi=="Check")$w))
(ip_urban_num_mobile_w = sum(subset(ip_urban1.df, pi=="Mobile app")$w))
(ip_urban_num_other_w = sum(subset(ip_urban1.df, pi!="Cash" & pi!="Credit card" & pi!="Debit card" & pi!="Prepaid card" & pi!="Check" & pi!="Mobile app")$w))

# weighted fraction payments urban
table(ip_urban1.df$pi)
(ip_urban_frac_cash_w = sum(subset(ip_urban1.df, pi=="Cash")$w)/sum(ip_urban1.df$w))
(ip_urban_frac_credit_w = sum(subset(ip_urban1.df, pi=="Credit card")$w)/sum(ip_urban1.df$w))
(ip_urban_frac_debit_w = sum(subset(ip_urban1.df, pi=="Debit card")$w)/sum(ip_urban1.df$w))
(ip_urban_frac_prepaid_w = sum(subset(ip_urban1.df, pi=="Prepaid card")$w)/sum(ip_urban1.df$w))
(ip_urban_frac_check_w = sum(subset(ip_urban1.df, pi=="Check")$w)/sum(ip_urban1.df$w))
(ip_urban_frac_mobile_w = sum(subset(ip_urban1.df, pi=="Mobile app")$w)/sum(ip_urban1.df$w))
(ip_urban_frac_other_w = sum(subset(ip_urban1.df, pi!="Cash" & pi!="Credit card" & pi!="Debit card" & pi!="Prepaid card" & pi!="Check" & pi!="Mobile app")$w)/sum(ip_urban1.df$w))
# verify sum up to 1
ip_urban_frac_cash_w+ip_urban_frac_credit_w+ip_urban_frac_debit_w+ip_urban_frac_prepaid_w  +ip_urban_frac_check_w+ip_urban_frac_mobile_w+ip_urban_frac_other_w

# construct column vectors for data frame
(ip_cash.vec = c(ip_rural_num.vec[1], ip_mixed_num.vec[1], ip_urban_num.vec[1], ip_rural_frac.vec[1] , ip_mixed_frac.vec[1], ip_urban_frac.vec[1], ip_rural_frac_cash_w, ip_mixed_frac_cash_w, ip_urban_frac_cash_w))
#
(ip_credit.vec = c(ip_rural_num.vec[2], ip_mixed_num.vec[2], ip_urban_num.vec[2], ip_rural_frac.vec[2] , ip_mixed_frac.vec[2], ip_urban_frac.vec[2], ip_rural_frac_credit_w, ip_mixed_frac_credit_w, ip_urban_frac_credit_w))
#
(ip_debit.vec = c(ip_rural_num.vec[3], ip_mixed_num.vec[3], ip_urban_num.vec[3], ip_rural_frac.vec[3] , ip_mixed_frac.vec[3], ip_urban_frac.vec[3], ip_rural_frac_debit_w, ip_mixed_frac_debit_w, ip_urban_frac_debit_w))
#
(ip_prepaid.vec = c(ip_rural_num.vec[4], ip_mixed_num.vec[4], ip_urban_num.vec[4], ip_rural_frac.vec[4] , ip_mixed_frac.vec[4], ip_urban_frac.vec[4], ip_rural_frac_prepaid_w, ip_mixed_frac_prepaid_w, ip_urban_frac_prepaid_w))
#
(ip_check.vec = c(ip_rural_num.vec[5], ip_mixed_num.vec[5], ip_urban_num.vec[5], ip_rural_frac.vec[5] , ip_mixed_frac.vec[5], ip_urban_frac.vec[5], ip_rural_frac_check_w, ip_mixed_frac_check_w, ip_urban_frac_check_w))
#
(ip_mobile.vec = c(ip_rural_num.vec[6], ip_mixed_num.vec[6], ip_urban_num.vec[6], ip_rural_frac.vec[6] , ip_mixed_frac.vec[6], ip_urban_frac.vec[6], ip_rural_frac_mobile_w, ip_mixed_frac_mobile_w, ip_urban_frac_mobile_w))
#
(ip_other.vec = c(ip_rural_num.vec[7], ip_mixed_num.vec[7], ip_urban_num.vec[7], ip_rural_frac.vec[7] , ip_mixed_frac.vec[7], ip_urban_frac.vec[7], ip_rural_frac_other_w, ip_mixed_frac_other_w, ip_urban_frac_other_w))
# verify sum
ip_cash.vec + ip_credit.vec + ip_debit.vec + ip_prepaid.vec + ip_check.vec + ip_mobile.vec + ip_other.vec
nrow(ip_rural1.df)
nrow(ip_mixed1.df)
nrow(ip_urban1.df)

# Make it a data frame

#(ip_pi_list.vec = c("Cash", "Credit card", "Debit card", "Prepaid card", "Check",  "Mobile app", "Other"))
(ip_area.vec = c("Rural", "Mixed", "Urban","Rural", "Mixed", "Urban","Rural", "Mixed", "Urban"))
#
(inperson1.df = data.frame(Method = ip_area.vec, Cash = ip_cash.vec, Credit = ip_credit.vec, Debit = ip_debit.vec, Prepaid = ip_prepaid.vec, Check = ip_check.vec, Mobile = ip_mobile.vec, Other = ip_other.vec))

inperson1.df[4:9, ] = lapply(inperson1.df[4:9, ], function(x) {
  if (is.numeric(x)) sprintf("%.0f%%", x * 100) else x
})

print(xtable(inperson1.df, digits=0), include.rownames = F, hline.after = c(0,3,6))

# info for Table 4
rural_num_ip# num payments
mixed_num_ip# num payments
urban_num_ip# num payments
#
length(unique(ip_rural1.df$id))# num respondents
length(unique(ip_mixed1.df$id))# num respondents
length(unique(ip_urban1.df$id))# num respondents

#p-values for z-test percentages comparing rural with urban only
#comparing FRAC IN-PERSON rural vs. urban => sig
# two-proportion z-test (pooled)
# cash difference
ip_rural_num.vec[1]# rural cash in person payments
ip_urban_num.vec[1]# urban cash in person payments
nrow(ip_rural1.df)# total rural in person payments
nrow(ip_urban1.df)# total urban in person payments
prop.test(x = c(ip_rural_num.vec[1], ip_urban_num.vec[1]),
          n = c(nrow(ip_rural1.df), nrow(ip_urban1.df)),
          correct = FALSE)   # disable Yates correction to get pure z-test
# credit difference
ip_rural_num.vec[2]# rural credit in person payments
ip_urban_num.vec[2]# urban credit in person payments
nrow(ip_rural1.df)# total rural in person payments
nrow(ip_urban1.df)# total urban in person payments
prop.test(x = c(ip_rural_num.vec[2], ip_urban_num.vec[2]),
          n = c(nrow(ip_rural1.df), nrow(ip_urban1.df)),
          correct = FALSE)   # disable Yates correction to get pure z-test
# debit difference
ip_rural_num.vec[3]# rural debit in person payments
ip_urban_num.vec[3]# urban debit in person payments
nrow(ip_rural1.df)# total rural in person payments
nrow(ip_urban1.df)# total urban in person payments
prop.test(x = c(ip_rural_num.vec[3], ip_urban_num.vec[3]),
          n = c(nrow(ip_rural1.df), nrow(ip_urban1.df)),
          correct = FALSE)   # disable Yates correction to get pure z-test
# Prepaid card difference
ip_rural_num.vec[4]# rural prepaid in person payments
ip_urban_num.vec[4]# urban prepaid in person payments
nrow(ip_rural1.df)# total rural in person payments
nrow(ip_urban1.df)# total urban in person payments
prop.test(x = c(ip_rural_num.vec[4], ip_urban_num.vec[4]),
          n = c(nrow(ip_rural1.df), nrow(ip_urban1.df)),
          correct = FALSE)   # disable Yates correction to get pure z-test
# check difference
ip_rural_num.vec[5]# rural check in person payments
ip_urban_num.vec[5]# urban check in person payments
nrow(ip_rural1.df)# total rural in person payments
nrow(ip_urban1.df)# total urban in person payments
prop.test(x = c(ip_rural_num.vec[5], ip_urban_num.vec[5]),
          n = c(nrow(ip_rural1.df), nrow(ip_urban1.df)),
          correct = FALSE)   # disable Yates correction to get pure z-test
# mobile difference
ip_rural_num.vec[6]# rural mobile in person payments
ip_urban_num.vec[6]# urban mobile in person payments
nrow(ip_rural1.df)# total rural in person payments
nrow(ip_urban1.df)# total urban in person payments
prop.test(x = c(ip_rural_num.vec[6], ip_urban_num.vec[6]),
          n = c(nrow(ip_rural1.df), nrow(ip_urban1.df)),
          correct = FALSE)   # disable Yates correction to get pure z-test
# other difference
ip_rural_num.vec[7]# rural other in person payments
ip_urban_num.vec[7]# urban other in person payments
nrow(ip_rural1.df)# total rural in person payments
nrow(ip_urban1.df)# total urban in person payments
prop.test(x = c(ip_rural_num.vec[7], ip_urban_num.vec[7]),
          n = c(nrow(ip_rural1.df), nrow(ip_urban1.df)),
          correct = FALSE)   # disable Yates correction to get pure z-test



#End: Table 4: In-person payments####

#++++++++++++++++++++++++++++++

#Begin: Table 5: In-person random forest####

names(m4.df)# entire indiv and trans data combined
ip_m4.df = subset(m4.df, in_person == 1)# in person payments

# redefine pi
ip_m4.df = ip_m4.df %>% mutate(pi = ifelse(pi > 10, "Other", pi))# must come first b/c logical will not work after some are replaced by text
ip_m4.df = ip_m4.df %>% mutate(pi = ifelse(pi==0, "Other", pi))
ip_m4.df = ip_m4.df %>% mutate(pi = ifelse(pi==6, "Other", pi))
ip_m4.df = ip_m4.df %>% mutate(pi = ifelse(pi==7, "Other", pi))
ip_m4.df = ip_m4.df %>% mutate(pi = ifelse(pi==8, "Other", pi))
ip_m4.df = ip_m4.df %>% mutate(pi = ifelse(pi==9, "Other", pi))
ip_m4.df = ip_m4.df %>% mutate(pi = ifelse(pi==1, "Cash", pi))
ip_m4.df = ip_m4.df %>% mutate(pi = ifelse(pi==2, "Check", pi))
ip_m4.df = ip_m4.df %>% mutate(pi = ifelse(pi==3, "Credit card", pi))
ip_m4.df = ip_m4.df %>% mutate(pi = ifelse(pi==4, "Debit card", pi))
ip_m4.df = ip_m4.df %>% mutate(pi = ifelse(pi==5, "Prepaid card", pi))
ip_m4.df = ip_m4.df %>% mutate(pi = ifelse(pi==10, "Mobile app", pi))
round(prop.table(table(ip_m4.df$pi, useNA = "always")),2)
ip_m4.df$pi = as.factor(ip_m4.df$pi)
levels(ip_m4.df$pi)
ip_m4.df$pi = factor(ip_m4.df$pi, levels = c("Cash", "Credit card", "Debit card", "Prepaid card", "Check", "Mobile app", "Other" ))
levels(ip_m4.df$pi)
table(ip_m4.df$pi)

str(ip_m4.df$amnt)
ip_m4.df=ip_m4.df %>% rename(Amount = amnt)
str(ip_m4.df$HH_income)
str(ip_m4.df$Age)
str(ip_m4.df$Gender)
str(ip_m4.df$Area)
str(ip_m4.df$Race)
str(ip_m4.df$Hispanic)
str(ip_m4.df$Education)
str(ip_m4.df$Own_credit)
ip_m4.df$Own_credit = as.factor(ip_m4.df$Own_credit)
str(ip_m4.df$Own_debit)
ip_m4.df$Own_debit = as.factor(ip_m4.df$Own_debit)
str(ip_m4.df$Credit_reward)
ip_m4.df$Credit_reward = as.factor(ip_m4.df$Credit_reward)
str(ip_m4.df$pi)


# random forest model
names(ip_m4.df)
rf_ip_pi.model = pi ~ Amount + HH_income + Age + Gender + Area +Education +Race +Hispanic + Own_credit + Own_debit  + Credit_reward

set.seed(1955)

(forest_output =randomForest(rf_ip_pi.model, data = ip_m4.df, importance=T, na.action=na.roughfix))
# defaults: mtry = rounded downwards sqrt(#predictors), nodesize=1, na.action =na.roughtfix => imputations. 
forest_output$type# verify classification tree (not regression tree)
#forest_output$confusion based on the OOB sample (Instead, I use train-test subsamples for the confusion matrix)

# Table of variable importance for the entire sample
forest_importance.df =importance(forest_output) 
str(forest_importance.df)
(forest_importance2.df = as.data.frame(forest_importance.df))
# Below, Plot of variable importance (not in paper)
#varImpPlot(random_forest_output, type = 1, main ='', bg = "blue", cex=2)#default type 1&2, 

# Rename rows
(forest_importance3.df = round(forest_importance2.df, digits = 2))
row.names(forest_importance3.df)
#
# Delete the GINI MDA from the importance table
names(forest_importance3.df)
dim(forest_importance3.df)
(forest_importance4.df = forest_importance3.df[,1:8])
#
print(xtable(forest_importance4.df))
# info about the above table notes
nrow(ip_m4.df)# num of payments
length(unique(ip_m4.df$id))# num respondents

#End: Table 5: In-person random forest####

#++++++++++++++++++++++++++++++

#Begin: Table 7: Remote payments####

#subset to remote (rem) only
rem_rural1.df = subset(rural1.df, in_person==0)
dim(rem_rural1.df)
rem_mixed1.df = subset(mixed1.df, in_person==0)
dim(rem_mixed1.df)
rem_urban1.df = subset(urban1.df, in_person==0)
dim(rem_urban1.df)

table(rem_rural1.df$pi, useNA = "always")
table(rem_mixed1.df$pi, useNA = "always")
table(rem_urban1.df$pi, useNA = "always")
round(prop.table(table(rem_rural1.df$pi, useNA = "always")),2)
round(prop.table(table(rem_mixed1.df$pi, useNA = "always")),2)
round(prop.table(table(rem_urban1.df$pi, useNA = "always")),2)

# PI renaming rural
rem_rural1.df$pi = as.numeric(rem_rural1.df$pi)
rem_rural1.df = rem_rural1.df %>% mutate(pi = ifelse(pi==0, "Other", pi))#
rem_rural1.df = rem_rural1.df %>% mutate(pi = ifelse(pi==1, "Other", pi))
rem_rural1.df = rem_rural1.df %>% mutate(pi = ifelse(pi==8, "Other", pi))
rem_rural1.df = rem_rural1.df %>% mutate(pi = ifelse(pi==13, "Other", pi))
rem_rural1.df = rem_rural1.df %>% mutate(pi = ifelse(pi==14, "Income deduction", pi))
rem_rural1.df = rem_rural1.df %>% mutate(pi = ifelse(pi==2, "Check", pi))
rem_rural1.df = rem_rural1.df %>% mutate(pi = ifelse(pi==3, "Credit card", pi))
rem_rural1.df = rem_rural1.df %>% mutate(pi = ifelse(pi==4, "Debit card", pi))
rem_rural1.df = rem_rural1.df %>% mutate(pi = ifelse(pi==5, "Prepaid card", pi))
rem_rural1.df = rem_rural1.df %>% mutate(pi = ifelse(pi==6, "Bank account number", pi))
rem_rural1.df = rem_rural1.df %>% mutate(pi = ifelse(pi==7, "Online banking bill", pi))
rem_rural1.df = rem_rural1.df %>% mutate(pi = ifelse(pi==10, "Mobile app", pi))
rem_rural1.df = rem_rural1.df %>% mutate(pi = ifelse(pi==11, "Account to account", pi))

round(prop.table(table(rem_rural1.df$pi, useNA = "always")),2)
rem_rural1.df$pi = as.factor(rem_rural1.df$pi)
levels(rem_rural1.df$pi)
rem_rural1.df$pi = factor(rem_rural1.df$pi, levels = c("Credit card", "Debit card", "Prepaid card", "Check", "Mobile app", "Bank account number", "Online banking bill", "Account to account", "Income deduction", "Other" ))
levels(rem_rural1.df$pi)
table(rem_rural1.df$pi, useNA = "always")

# PI renaming mixed
rem_mixed1.df$pi = as.numeric(rem_mixed1.df$pi)
rem_mixed1.df = rem_mixed1.df %>% mutate(pi = ifelse(pi==0, "Other", pi))#
rem_mixed1.df = rem_mixed1.df %>% mutate(pi = ifelse(pi==1, "Other", pi))
rem_mixed1.df = rem_mixed1.df %>% mutate(pi = ifelse(pi==8, "Other", pi))
rem_mixed1.df = rem_mixed1.df %>% mutate(pi = ifelse(pi==13, "Other", pi))
rem_mixed1.df = rem_mixed1.df %>% mutate(pi = ifelse(pi==14, "Income deduction", pi))
rem_mixed1.df = rem_mixed1.df %>% mutate(pi = ifelse(pi==2, "Check", pi))
rem_mixed1.df = rem_mixed1.df %>% mutate(pi = ifelse(pi==3, "Credit card", pi))
rem_mixed1.df = rem_mixed1.df %>% mutate(pi = ifelse(pi==4, "Debit card", pi))
rem_mixed1.df = rem_mixed1.df %>% mutate(pi = ifelse(pi==5, "Prepaid card", pi))
rem_mixed1.df = rem_mixed1.df %>% mutate(pi = ifelse(pi==6, "Bank account number", pi))
rem_mixed1.df = rem_mixed1.df %>% mutate(pi = ifelse(pi==7, "Online banking bill", pi))
rem_mixed1.df = rem_mixed1.df %>% mutate(pi = ifelse(pi==10, "Mobile app", pi))
rem_mixed1.df = rem_mixed1.df %>% mutate(pi = ifelse(pi==11, "Account to account", pi))

round(prop.table(table(rem_mixed1.df$pi, useNA = "always")),2)
rem_mixed1.df$pi = as.factor(rem_mixed1.df$pi)
levels(rem_mixed1.df$pi)
rem_mixed1.df$pi = factor(rem_mixed1.df$pi, levels = c("Credit card", "Debit card", "Prepaid card", "Check", "Mobile app", "Bank account number", "Online banking bill", "Account to account", "Income deduction", "Other" ))
levels(rem_mixed1.df$pi)
table(rem_mixed1.df$pi, useNA = "always")

# PI renaming urban
rem_urban1.df$pi = as.numeric(rem_urban1.df$pi)
rem_urban1.df = rem_urban1.df %>% mutate(pi = ifelse(pi==0, "Other", pi))#
rem_urban1.df = rem_urban1.df %>% mutate(pi = ifelse(pi==1, "Other", pi))
rem_urban1.df = rem_urban1.df %>% mutate(pi = ifelse(pi==8, "Other", pi))
rem_urban1.df = rem_urban1.df %>% mutate(pi = ifelse(pi==13, "Other", pi))
rem_urban1.df = rem_urban1.df %>% mutate(pi = ifelse(pi==14, "Income deduction", pi))
rem_urban1.df = rem_urban1.df %>% mutate(pi = ifelse(pi==2, "Check", pi))
rem_urban1.df = rem_urban1.df %>% mutate(pi = ifelse(pi==3, "Credit card", pi))
rem_urban1.df = rem_urban1.df %>% mutate(pi = ifelse(pi==4, "Debit card", pi))
rem_urban1.df = rem_urban1.df %>% mutate(pi = ifelse(pi==5, "Prepaid card", pi))
rem_urban1.df = rem_urban1.df %>% mutate(pi = ifelse(pi==6, "Bank account number", pi))
rem_urban1.df = rem_urban1.df %>% mutate(pi = ifelse(pi==7, "Online banking bill", pi))
rem_urban1.df = rem_urban1.df %>% mutate(pi = ifelse(pi==10, "Mobile app", pi))
rem_urban1.df = rem_urban1.df %>% mutate(pi = ifelse(pi==11, "Account to account", pi))

round(prop.table(table(rem_urban1.df$pi, useNA = "always")),2)
rem_urban1.df$pi = as.factor(rem_urban1.df$pi)
levels(rem_urban1.df$pi)
rem_urban1.df$pi = factor(rem_urban1.df$pi, levels = c("Credit card", "Debit card", "Prepaid card", "Check", "Mobile app", "Bank account number", "Online banking bill", "Account to account", "Income deduction", "Other" ))
levels(rem_urban1.df$pi)
table(rem_urban1.df$pi, useNA = "always")


# num payments by PI by area (get 2nd row only)
(rem_rural_num.vec = unname(table(rem_rural1.df$pi)))#unname the table
(rem_mixed_num.vec = unname(table(rem_mixed1.df$pi)))#unname the table
(rem_urban_num.vec = unname(table(rem_urban1.df$pi)))#unname the table

# frac payments by PI by area
(rem_rural_frac.vec = rem_rural_num.vec/nrow(rem_rural1.df) )
(rem_mixed_frac.vec = rem_mixed_num.vec/nrow(rem_mixed1.df) )
(rem_urban_frac.vec = rem_urban_num.vec/nrow(rem_urban1.df) )
# verify sum to 1
sum(rem_rural_frac.vec)
sum(rem_mixed_frac.vec)
sum(rem_urban_frac.vec)

# weighted num payments rural
table(rem_rural1.df$pi)
(rem_rural_num_credit_w = sum(subset(rem_rural1.df, pi=="Credit card")$w))
(rem_rural_num_debit_w = sum(subset(rem_rural1.df, pi=="Debit card")$w))
(rem_rural_num_prepaid_w = sum(subset(rem_rural1.df, pi=="Prepaid card")$w))
(rem_rural_num_check_w = sum(subset(rem_rural1.df, pi=="Check")$w))
(rem_rural_num_mobile_w = sum(subset(rem_rural1.df, pi=="Mobile app")$w))
(rem_rural_num_number_w = sum(subset(rem_rural1.df, pi=="Bank account number")$w))
(rem_rural_num_bill_w = sum(subset(rem_rural1.df, pi=="Online banking bill")$w))
(rem_rural_num_a2a_w = sum(subset(rem_rural1.df, pi=="Account to account")$w))
(rem_rural_num_deducation_w = sum(subset(rem_rural1.df, pi=="Income deduction")$w))
(rem_rural_num_other_w = sum(subset(rem_rural1.df, pi=="Other")$w))

# weighted fraction payments rural
table(rem_rural1.df$pi)
(rem_rural_frac_credit_w = sum(subset(rem_rural1.df, pi=="Credit card")$w)/sum(rem_rural1.df$w))
(rem_rural_frac_debit_w = sum(subset(rem_rural1.df, pi=="Debit card")$w)/sum(rem_rural1.df$w))
(rem_rural_frac_prepaid_w = sum(subset(rem_rural1.df, pi=="Prepaid card")$w)/sum(rem_rural1.df$w))
(rem_rural_frac_check_w = sum(subset(rem_rural1.df, pi=="Check")$w)/sum(rem_rural1.df$w))
(rem_rural_frac_mobile_w = sum(subset(rem_rural1.df, pi=="Mobile app")$w)/sum(rem_rural1.df$w))
(rem_rural_frac_number_w = sum(subset(rem_rural1.df, pi=="Bank account number")$w)/sum(rem_rural1.df$w))
(rem_rural_frac_bill_w = sum(subset(rem_rural1.df, pi=="Online banking bill")$w)/sum(rem_rural1.df$w))
(rem_rural_frac_a2a_w = sum(subset(rem_rural1.df, pi=="Account to account")$w)/sum(rem_rural1.df$w))
(rem_rural_frac_deduction_w = sum(subset(rem_rural1.df, pi=="Income deduction")$w)/sum(rem_rural1.df$w))
(rem_rural_frac_other_w = sum(subset(rem_rural1.df, pi=="Other")$w)/sum(rem_rural1.df$w))

# verify sum up to 1
rem_rural_frac_credit_w+rem_rural_frac_debit_w+rem_rural_frac_prepaid_w+rem_rural_frac_check_w+rem_rural_frac_mobile_w +rem_rural_frac_number_w +rem_rural_frac_bill_w +rem_rural_frac_a2a_w +rem_rural_frac_deduction_w +rem_rural_frac_other_w

# weighted num payments mixed
# frac payments by PI by area
# weighted num payments mixed
table(rem_mixed1.df$pi)
(rem_mixed_num_credit_w = sum(subset(rem_mixed1.df, pi=="Credit card")$w))
(rem_mixed_num_debit_w = sum(subset(rem_mixed1.df, pi=="Debit card")$w))
(rem_mixed_num_prepaid_w = sum(subset(rem_mixed1.df, pi=="Prepaid card")$w))
(rem_mixed_num_check_w = sum(subset(rem_mixed1.df, pi=="Check")$w))
(rem_mixed_num_mobile_w = sum(subset(rem_mixed1.df, pi=="Mobile app")$w))
(rem_mixed_num_number_w = sum(subset(rem_mixed1.df, pi=="Bank account number")$w))
(rem_mixed_num_bill_w = sum(subset(rem_mixed1.df, pi=="Online banking bill")$w))
(rem_mixed_num_a2a_w = sum(subset(rem_mixed1.df, pi=="Account to account")$w))
(rem_mixed_num_deducation_w = sum(subset(rem_mixed1.df, pi=="Income deduction")$w))
(rem_mixed_num_other_w = sum(subset(rem_mixed1.df, pi=="Other")$w))

# weighted fraction payments mixed
table(rem_mixed1.df$pi)
(rem_mixed_frac_credit_w = sum(subset(rem_mixed1.df, pi=="Credit card")$w)/sum(rem_mixed1.df$w))
(rem_mixed_frac_debit_w = sum(subset(rem_mixed1.df, pi=="Debit card")$w)/sum(rem_mixed1.df$w))
(rem_mixed_frac_prepaid_w = sum(subset(rem_mixed1.df, pi=="Prepaid card")$w)/sum(rem_mixed1.df$w))
(rem_mixed_frac_check_w = sum(subset(rem_mixed1.df, pi=="Check")$w)/sum(rem_mixed1.df$w))
(rem_mixed_frac_mobile_w = sum(subset(rem_mixed1.df, pi=="Mobile app")$w)/sum(rem_mixed1.df$w))
(rem_mixed_frac_number_w = sum(subset(rem_mixed1.df, pi=="Bank account number")$w)/sum(rem_mixed1.df$w))
(rem_mixed_frac_bill_w = sum(subset(rem_mixed1.df, pi=="Online banking bill")$w)/sum(rem_mixed1.df$w))
(rem_mixed_frac_a2a_w = sum(subset(rem_mixed1.df, pi=="Account to account")$w)/sum(rem_mixed1.df$w))
(rem_mixed_frac_deduction_w = sum(subset(rem_mixed1.df, pi=="Income deduction")$w)/sum(rem_mixed1.df$w))
(rem_mixed_frac_other_w = sum(subset(rem_mixed1.df, pi=="Other")$w)/sum(rem_mixed1.df$w))

# verify sum up to 1
rem_mixed_frac_credit_w+rem_mixed_frac_debit_w+rem_mixed_frac_prepaid_w+rem_mixed_frac_check_w+rem_mixed_frac_mobile_w +rem_mixed_frac_number_w +rem_mixed_frac_bill_w +rem_mixed_frac_a2a_w +rem_mixed_frac_deduction_w +rem_mixed_frac_other_w

# weighted num payments urban
# frac payments by PI by area
# weighted num payments urban
table(rem_urban1.df$pi)
(rem_urban_num_credit_w = sum(subset(rem_urban1.df, pi=="Credit card")$w))
(rem_urban_num_debit_w = sum(subset(rem_urban1.df, pi=="Debit card")$w))
(rem_urban_num_prepaid_w = sum(subset(rem_urban1.df, pi=="Prepaid card")$w))
(rem_urban_num_check_w = sum(subset(rem_urban1.df, pi=="Check")$w))
(rem_urban_num_mobile_w = sum(subset(rem_urban1.df, pi=="Mobile app")$w))
(rem_urban_num_number_w = sum(subset(rem_urban1.df, pi=="Bank account number")$w))
(rem_urban_num_bill_w = sum(subset(rem_urban1.df, pi=="Online banking bill")$w))
(rem_urban_num_a2a_w = sum(subset(rem_urban1.df, pi=="Account to account")$w))
(rem_urban_num_deducation_w = sum(subset(rem_urban1.df, pi=="Income deduction")$w))
(rem_urban_num_other_w = sum(subset(rem_urban1.df, pi=="Other")$w))

# weighted fraction payments urban
table(rem_urban1.df$pi)
(rem_urban_frac_credit_w = sum(subset(rem_urban1.df, pi=="Credit card")$w)/sum(rem_urban1.df$w))
(rem_urban_frac_debit_w = sum(subset(rem_urban1.df, pi=="Debit card")$w)/sum(rem_urban1.df$w))
(rem_urban_frac_prepaid_w = sum(subset(rem_urban1.df, pi=="Prepaid card")$w)/sum(rem_urban1.df$w))
(rem_urban_frac_check_w = sum(subset(rem_urban1.df, pi=="Check")$w)/sum(rem_urban1.df$w))
(rem_urban_frac_mobile_w = sum(subset(rem_urban1.df, pi=="Mobile app")$w)/sum(rem_urban1.df$w))
(rem_urban_frac_number_w = sum(subset(rem_urban1.df, pi=="Bank account number")$w)/sum(rem_urban1.df$w))
(rem_urban_frac_bill_w = sum(subset(rem_urban1.df, pi=="Online banking bill")$w)/sum(rem_urban1.df$w))
(rem_urban_frac_a2a_w = sum(subset(rem_urban1.df, pi=="Account to account")$w)/sum(rem_urban1.df$w))
(rem_urban_frac_deduction_w = sum(subset(rem_urban1.df, pi=="Income deduction")$w)/sum(rem_urban1.df$w))
(rem_urban_frac_other_w = sum(subset(rem_urban1.df, pi=="Other")$w)/sum(rem_urban1.df$w))

# verify sum up to 1
rem_urban_frac_credit_w+rem_urban_frac_debit_w+rem_urban_frac_prepaid_w+rem_urban_frac_check_w+rem_urban_frac_mobile_w +rem_urban_frac_number_w +rem_urban_frac_bill_w +rem_urban_frac_a2a_w +rem_urban_frac_deduction_w +rem_urban_frac_other_w

# construct column vectors for data frame
(rem_credit.vec = c(rem_rural_num.vec[1], rem_mixed_num.vec[1], rem_urban_num.vec[1], rem_rural_frac.vec[1] , rem_mixed_frac.vec[1], rem_urban_frac.vec[1], rem_rural_frac_credit_w, rem_mixed_frac_credit_w, rem_urban_frac_credit_w))
#
(rem_debit.vec = c(rem_rural_num.vec[2], rem_mixed_num.vec[2], rem_urban_num.vec[2], rem_rural_frac.vec[2] , rem_mixed_frac.vec[2], rem_urban_frac.vec[2], rem_rural_frac_debit_w, rem_mixed_frac_debit_w, rem_urban_frac_debit_w))
#
(rem_prepaid.vec = c(rem_rural_num.vec[3], rem_mixed_num.vec[3], rem_urban_num.vec[3], rem_rural_frac.vec[3] , rem_mixed_frac.vec[3], rem_urban_frac.vec[3], rem_rural_frac_prepaid_w, rem_mixed_frac_prepaid_w, rem_urban_frac_prepaid_w))
#
(rem_check.vec = c(rem_rural_num.vec[4], rem_mixed_num.vec[4], rem_urban_num.vec[4], rem_rural_frac.vec[4] , rem_mixed_frac.vec[4], rem_urban_frac.vec[4], rem_rural_frac_check_w, rem_mixed_frac_check_w, rem_urban_frac_check_w))
#
(rem_mobile.vec = c(rem_rural_num.vec[5], rem_mixed_num.vec[5], rem_urban_num.vec[5], rem_rural_frac.vec[5] , rem_mixed_frac.vec[5], rem_urban_frac.vec[5], rem_rural_frac_mobile_w, rem_mixed_frac_mobile_w, rem_urban_frac_mobile_w))
#
(rem_number.vec = c(rem_rural_num.vec[6], rem_mixed_num.vec[6], rem_urban_num.vec[6], rem_rural_frac.vec[6] , rem_mixed_frac.vec[6], rem_urban_frac.vec[6], rem_rural_frac_number_w, rem_mixed_frac_number_w, rem_urban_frac_number_w))
#
(rem_bill.vec = c(rem_rural_num.vec[7], rem_mixed_num.vec[7], rem_urban_num.vec[7], rem_rural_frac.vec[7] , rem_mixed_frac.vec[7], rem_urban_frac.vec[7], rem_rural_frac_bill_w, rem_mixed_frac_bill_w, rem_urban_frac_bill_w))
#
(rem_a2a.vec = c(rem_rural_num.vec[8], rem_mixed_num.vec[8], rem_urban_num.vec[8], rem_rural_frac.vec[8] , rem_mixed_frac.vec[8], rem_urban_frac.vec[8], rem_rural_frac_a2a_w, rem_mixed_frac_a2a_w, rem_urban_frac_a2a_w))
#
(rem_deduction.vec = c(rem_rural_num.vec[9], rem_mixed_num.vec[9], rem_urban_num.vec[9], rem_rural_frac.vec[9] , rem_mixed_frac.vec[9], rem_urban_frac.vec[9], rem_rural_frac_deduction_w, rem_mixed_frac_deduction_w, rem_urban_frac_deduction_w))
#
(rem_other.vec = c(rem_rural_num.vec[10], rem_mixed_num.vec[10], rem_urban_num.vec[10], rem_rural_frac.vec[10] , rem_mixed_frac.vec[10], rem_urban_frac.vec[10], rem_rural_frac_other_w, rem_mixed_frac_other_w, rem_urban_frac_other_w))
#
# verify sum
rem_credit.vec + rem_debit.vec + rem_prepaid.vec + rem_check.vec + rem_mobile.vec + rem_number.vec + rem_bill.vec + rem_a2a.vec + rem_deduction.vec + rem_other.vec
#
nrow(rem_rural1.df)
nrow(rem_mixed1.df)
nrow(rem_urban1.df)

# Make it a data frame

(rem_area.vec = c("Rural", "Mixed", "Urban","Rural", "Mixed", "Urban","Rural", "Mixed", "Urban"))
#
(remote1.df = data.frame(Method = rem_area.vec, Credit = rem_credit.vec, Debit = rem_debit.vec, Prepaid = rem_prepaid.vec, Check = rem_check.vec, Mobile = rem_mobile.vec, Acc_No = rem_number.vec, OL_Bill = rem_bill.vec, A2A = rem_a2a.vec, Deduct = rem_deduction.vec, Other = rem_other.vec))

remote1.df[4:9, ] = lapply(remote1.df[4:9, ], function(x) {
  if (is.numeric(x)) sprintf("%.0f%%", x * 100) else x
})

print(xtable(remote1.df, digits=0), include.rownames = F, hline.after = c(0,3,6))

# info for Table 5
nrow(rem_rural1.df)# num payments
nrow(rem_mixed1.df)# num payments
nrow(rem_urban1.df)# num payments
#
length(unique(rem_rural1.df$id))# num respondents
length(unique(rem_mixed1.df$id))# num respondents
length(unique(rem_urban1.df$id))# num respondents

#p-values for z-test percentages comparing rural with urban only
#comparing FRAC remote rural vs. urban => sig
# two-proportion z-test (pooled)
#
# credit difference
rem_rural_num.vec[1]# rural credit remote payments
rem_urban_num.vec[1]# urban credit remote payments
nrow(rem_rural1.df)# total rural remote payments
nrow(rem_urban1.df)# total urban remote payments
prop.test(x = c(rem_rural_num.vec[1], rem_urban_num.vec[1]),
          n = c(nrow(rem_rural1.df), nrow(rem_urban1.df)),
          correct = FALSE)   # disable Yates correction to get pure z-test
#
# debit difference
rem_rural_num.vec[2]# rural debit remote payments
rem_urban_num.vec[2]# urban debit remote payments
nrow(rem_rural1.df)# total rural remote payments
nrow(rem_urban1.df)# total urban remote payments
prop.test(x = c(rem_rural_num.vec[2], rem_urban_num.vec[2]),
          n = c(nrow(rem_rural1.df), nrow(rem_urban1.df)),
          correct = FALSE)   # disable Yates correction to get pure z-test
#
# prepaid difference
rem_rural_num.vec[3]# rural prepaid remote payments
rem_urban_num.vec[3]# urban prepaid remote payments
nrow(rem_rural1.df)# total rural remote payments
nrow(rem_urban1.df)# total urban remote payments
prop.test(x = c(rem_rural_num.vec[3], rem_urban_num.vec[3]),
          n = c(nrow(rem_rural1.df), nrow(rem_urban1.df)),
          correct = FALSE)   # disable Yates correction to get pure z-test
#
# check difference
rem_rural_num.vec[4]# rural check remote payments
rem_urban_num.vec[4]# urban check remote payments
nrow(rem_rural1.df)# total rural remote payments
nrow(rem_urban1.df)# total urban remote payments
prop.test(x = c(rem_rural_num.vec[4], rem_urban_num.vec[4]),
          n = c(nrow(rem_rural1.df), nrow(rem_urban1.df)),
          correct = FALSE)   # disable Yates correction to get pure z-test
#
# mobile difference
rem_rural_num.vec[5]# rural mobile remote payments
rem_urban_num.vec[5]# urban mobile remote payments
nrow(rem_rural1.df)# total rural remote payments
nrow(rem_urban1.df)# total urban remote payments
prop.test(x = c(rem_rural_num.vec[5], rem_urban_num.vec[5]),
          n = c(nrow(rem_rural1.df), nrow(rem_urban1.df)),
          correct = FALSE)   # disable Yates correction to get pure z-test
#
# Account number difference
rem_rural_num.vec[6]# rural Account number remote payments
rem_urban_num.vec[6]# urban Account number remote payments
nrow(rem_rural1.df)# total rural remote payments
nrow(rem_urban1.df)# total urban remote payments
prop.test(x = c(rem_rural_num.vec[6], rem_urban_num.vec[6]),
          n = c(nrow(rem_rural1.df), nrow(rem_urban1.df)),
          correct = FALSE)   # disable Yates correction to get pure z-test
#
# online banking bill difference
rem_rural_num.vec[7]# rural online banking bill remote payments
rem_urban_num.vec[7]# urban online banking bill remote payments
nrow(rem_rural1.df)# total rural remote payments
nrow(rem_urban1.df)# total urban remote payments
prop.test(x = c(rem_rural_num.vec[7], rem_urban_num.vec[7]),
          n = c(nrow(rem_rural1.df), nrow(rem_urban1.df)),
          correct = FALSE)   # disable Yates correction to get pure z-test
#
# A2A difference
rem_rural_num.vec[8]# rural A2A remote payments
rem_urban_num.vec[8]# urban A2A remote payments
nrow(rem_rural1.df)# total rural remote payments
nrow(rem_urban1.df)# total urban remote payments
prop.test(x = c(rem_rural_num.vec[8], rem_urban_num.vec[8]),
          n = c(nrow(rem_rural1.df), nrow(rem_urban1.df)),
          correct = FALSE)   # disable Yates correction to get pure z-test
#
# income deduction difference
rem_rural_num.vec[9]# rural income deduction remote payments
rem_urban_num.vec[9]# urban income deduction remote payments
nrow(rem_rural1.df)# total rural remote payments
nrow(rem_urban1.df)# total urban remote payments
prop.test(x = c(rem_rural_num.vec[9], rem_urban_num.vec[9]),
          n = c(nrow(rem_rural1.df), nrow(rem_urban1.df)),
          correct = FALSE)   # disable Yates correction to get pure z-test
#
# other difference
rem_rural_num.vec[10]# rural other remote payments
rem_urban_num.vec[10]# urban other remote payments
nrow(rem_rural1.df)# total rural remote payments
nrow(rem_urban1.df)# total urban remote payments
prop.test(x = c(rem_rural_num.vec[10], rem_urban_num.vec[10]),
          n = c(nrow(rem_rural1.df), nrow(rem_urban1.df)),
          correct = FALSE)   # disable Yates correction to get pure z-test

# info for Table 6
nrow(rem_rural1.df)# num payments
nrow(rem_mixed1.df)
nrow(rem_urban1.df)
length((unique(rem_rural1.df$id)))# num respondents
length((unique(rem_mixed1.df$id)))# num respondents
length((unique(rem_urban1.df$id)))# num respondents

#End: Table 7: Remote payments####

#++++++++++++++++++++++++++++++

#Begin: Table 6 Selected spending categories####

names(ip_rural1.df)
table(ip_rural1.df$merch)#num payments by spending type
table(ip_mixed1.df$merch)#num payments by spending type
table(ip_urban1.df$merch)#num payments by spending type

#select  merchants 1=groceries, 4=fast food & coffee shops, 5=General merchandise stores, 16=P2P
ip_rural2.df = subset(ip_rural1.df, merch %in% c(1,4,5,16))
ip_mixed2.df = subset(ip_mixed1.df, merch %in% c(1,4,5,16))
ip_urban2.df = subset(ip_urban1.df, merch %in% c(1,4,5,16))

# new var: spend (category)
#1=Grocery stores, convenience stores without gas stations, pharmacies
#4=Fast food restaurants, coffee shops, cafeterias, food trucks
#5=General merchandise stores, department stores, other stores, online shopping
#16=Can be a gift or repayment to a family member, friend, or co-worker. Can be a payment to somebody who did a small job for you.
ip_rural2.df$spend = NA
ip_rural2.df$spend = ifelse(ip_rural2.df$merch==1, "Grocery", ip_rural2.df$spend)
ip_rural2.df$spend = ifelse(ip_rural2.df$merch==4, "Fast", ip_rural2.df$spend)
ip_rural2.df$spend = ifelse(ip_rural2.df$merch==5, "General", ip_rural2.df$spend)
ip_rural2.df$spend = ifelse(ip_rural2.df$merch==16, "P2P", ip_rural2.df$spend)
table(ip_rural2.df$spend, useNA = "always")
ip_rural2.df$spend = factor(ip_rural2.df$spend, levels = c("Grocery", "Fast", "General", "P2P"))
table(ip_rural2.df$spend, useNA = "always")
#
ip_mixed2.df$spend = NA
ip_mixed2.df$spend = ifelse(ip_mixed2.df$merch==1, "Grocery", ip_mixed2.df$spend)
ip_mixed2.df$spend = ifelse(ip_mixed2.df$merch==4, "Fast", ip_mixed2.df$spend)
ip_mixed2.df$spend = ifelse(ip_mixed2.df$merch==5, "General", ip_mixed2.df$spend)
ip_mixed2.df$spend = ifelse(ip_mixed2.df$merch==16, "P2P", ip_mixed2.df$spend)
table(ip_mixed2.df$spend, useNA = "always")
ip_mixed2.df$spend = factor(ip_mixed2.df$spend, levels = c("Grocery", "Fast", "General", "P2P"))
table(ip_mixed2.df$spend, useNA = "always")
#
ip_urban2.df$spend = NA
ip_urban2.df$spend = ifelse(ip_urban2.df$merch==1, "Grocery", ip_urban2.df$spend)
ip_urban2.df$spend = ifelse(ip_urban2.df$merch==4, "Fast", ip_urban2.df$spend)
ip_urban2.df$spend = ifelse(ip_urban2.df$merch==5, "General", ip_urban2.df$spend)
ip_urban2.df$spend = ifelse(ip_urban2.df$merch==16, "P2P", ip_urban2.df$spend)
table(ip_urban2.df$spend, useNA = "always")
ip_urban2.df$spend = factor(ip_urban2.df$spend, levels = c("Grocery", "Fast", "General", "P2P"))
table(ip_urban2.df$spend, useNA = "always")

# num payments by spend category
(num_ip_rural_spend.vec = unname(table(ip_rural2.df$spend)))
(num_ip_mixed_spend.vec = unname(table(ip_mixed2.df$spend)))
(num_ip_urban_spend.vec = unname(table(ip_urban2.df$spend)))

# frac payments by spend category (note: I divide by the entire sample (ip_rural1.df, not 2), not just by the selected spending categories)
(frac_ip_rural_spend.vec = num_ip_rural_spend.vec/nrow(ip_rural1.df))
(frac_ip_mixed_spend.vec = num_ip_mixed_spend.vec/nrow(ip_mixed1.df))
(frac_ip_urban_spend.vec = num_ip_urban_spend.vec/nrow(ip_urban1.df))

# Groceries num
(ip_rural_num_grocery = num_ip_rural_spend.vec[1])
(ip_mixed_num_grocery = num_ip_mixed_spend.vec[1])
(ip_urban_num_grocery = num_ip_urban_spend.vec[1])
#
# Groceries frac
(ip_rural_frac_grocery = frac_ip_rural_spend.vec[1])
(ip_mixed_frac_grocery = frac_ip_mixed_spend.vec[1])
(ip_urban_frac_grocery = frac_ip_urban_spend.vec[1])
#
# Groceries p-value: rural vs. urban
(prop_test_grocery = prop.test(x = c(ip_rural_num_grocery, ip_urban_num_grocery),
         n = c(nrow(ip_rural1.df), nrow(ip_urban1.df)),
          correct = FALSE))   # disable Yates correction to get pure z-test
(pval_grocery = ifelse(prop_test_grocery$p.value < 0.01, "Yes", "No" ))
#
#making column num groceries
(grocery_num.vec = c(ip_rural_num_grocery, ip_mixed_num_grocery, ip_urban_num_grocery, NA))# the NA is for the pval row
#
#making column frac groceries
(grocery_frac.vec = c(ip_rural_frac_grocery, ip_mixed_frac_grocery, ip_urban_frac_grocery, pval_grocery))

# Fast num (fast food & coffee shops), merch==2
(ip_rural_num_fast = num_ip_rural_spend.vec[2])
(ip_mixed_num_fast = num_ip_mixed_spend.vec[2])
(ip_urban_num_fast = num_ip_urban_spend.vec[2])
#
# Fast frac
(ip_rural_frac_fast = frac_ip_rural_spend.vec[2])
(ip_mixed_frac_fast = frac_ip_mixed_spend.vec[2])
(ip_urban_frac_fast = frac_ip_urban_spend.vec[2])
#
# Fast p-value: rural vs. urban
(prop_test_fast = prop.test(x = c(ip_rural_num_fast, ip_urban_num_fast), n = c(nrow(ip_rural1.df), nrow(ip_urban1.df)),
correct = FALSE))   # disable Yates correction to get pure z-test
(pval_fast = ifelse(prop_test_fast$p.value < 0.01, "Yes", "No" ))
#
#making column num fast
(fast_num.vec = c(ip_rural_num_fast, ip_mixed_num_fast, ip_urban_num_fast, NA))
#
#making column frac fast
(fast_frac.vec = c(ip_rural_frac_fast, ip_mixed_frac_fast, ip_urban_frac_fast, pval_fast))

# General stores num
(ip_rural_num_general = num_ip_rural_spend.vec[3])
(ip_mixed_num_general = num_ip_mixed_spend.vec[3])
(ip_urban_num_general = num_ip_urban_spend.vec[3])
#
# general frac
(ip_rural_frac_general = frac_ip_rural_spend.vec[3])
(ip_mixed_frac_general = frac_ip_mixed_spend.vec[3])
(ip_urban_frac_general = frac_ip_urban_spend.vec[3])
#
# general p-value: rural vs. urban
(prop_test_general = prop.test(x = c(ip_rural_num_general, ip_urban_num_general), n = c(nrow(ip_rural1.df), nrow(ip_urban1.df)), correct = FALSE))   # disable Yates correction to get pure z-test
(pval_general = ifelse(prop_test_general$p.value < 0.01, "Yes", "No" ))
#
#making column num general
(general_num.vec = c(ip_rural_num_general, ip_mixed_num_general, ip_urban_num_general, NA))
#
#making column frac general
(general_frac.vec = c(ip_rural_frac_general, ip_mixed_frac_general, ip_urban_frac_general, pval_general))

# p2p num
(ip_rural_num_p2p = num_ip_rural_spend.vec[4])
(ip_mixed_num_p2p = num_ip_mixed_spend.vec[4])
(ip_urban_num_p2p = num_ip_urban_spend.vec[4])
#
# p2p frac
(ip_rural_frac_p2p = frac_ip_rural_spend.vec[4])
(ip_mixed_frac_p2p = frac_ip_mixed_spend.vec[4])
(ip_urban_frac_p2p = frac_ip_urban_spend.vec[4])
#
# p2p p-value: rural vs. urban
(prop_test_p2p = prop.test(x = c(ip_rural_num_p2p, ip_urban_num_p2p),
                               n = c(nrow(ip_rural1.df), nrow(ip_urban1.df)),
                               correct = FALSE))   # disable Yates correction to get pure z-test
(pval_p2p = ifelse(prop_test_p2p$p.value < 0.01, "Yes", "No" ))
#
#making column num p2p
(p2p_num.vec = c(ip_rural_num_p2p, ip_mixed_num_p2p, ip_urban_num_p2p, NA))
#
#making column frac p2p
(p2p_frac.vec = c(ip_rural_frac_p2p, ip_mixed_frac_p2p, ip_urban_frac_p2p, pval_p2p))

# complete num column for table spend.t
(spend_num.vec = c(grocery_num.vec, fast_num.vec, general_num.vec, p2p_num.vec))

# complete frac column for table spend.t
(spend_frac.vec = c(grocery_frac.vec, fast_frac.vec, general_frac.vec, p2p_frac.vec))

# start cash column for table spend.t
table(ip_rural2.df$pi, useNA = "always")
# grocery cash rural
ip_rural_num_grocery# num grocery payments
(ip_rural_num_grocery_cash = nrow(subset(ip_rural2.df, spend == "Grocery" & pi == "Cash")) )# num grocery cash payments
(ip_rural_frac_grocery_cash = ip_rural_num_grocery_cash/ip_rural_num_grocery)
# grocery cash mixed
ip_mixed_num_grocery# num grocery payments
(ip_mixed_num_grocery_cash = nrow(subset(ip_mixed2.df, spend == "Grocery" & pi == "Cash")) )# num grocery cash payments
(ip_mixed_frac_grocery_cash = ip_mixed_num_grocery_cash/ip_mixed_num_grocery)
# grocery cash urban
ip_urban_num_grocery# num grocery payments
(ip_urban_num_grocery_cash = nrow(subset(ip_urban2.df, spend == "Grocery" & pi == "Cash")) )# num grocery cash payments
(ip_urban_frac_grocery_cash = ip_urban_num_grocery_cash/ip_urban_num_grocery)
# prop test rural vs urban grocery cash
(prop_test_grocery_cash = prop.test(x = c(ip_rural_num_grocery_cash, ip_urban_num_grocery_cash), n = c(ip_rural_num_grocery, ip_urban_num_grocery),
  correct = FALSE))   # disable Yates correction to get pure z-test
(pval_grocery_cash = ifelse(prop_test_grocery_cash$p.value < 0.01, "Yes", "No"))
#
# grocery credit card rural
ip_rural_num_grocery# num grocery payments
(ip_rural_num_grocery_credit = nrow(subset(ip_rural2.df, spend == "Grocery" & pi == "Credit card")) )# num grocery cash payments
(ip_rural_frac_grocery_credit = ip_rural_num_grocery_credit/ip_rural_num_grocery)
# grocery credit card mixed
ip_mixed_num_grocery# num grocery payments
(ip_mixed_num_grocery_credit = nrow(subset(ip_mixed2.df, spend == "Grocery" & pi == "Credit card")) )# num grocery cash payments
(ip_mixed_frac_grocery_credit = ip_mixed_num_grocery_credit/ip_mixed_num_grocery)
# grocery credit card urban
ip_urban_num_grocery# num grocery payments
(ip_urban_num_grocery_credit = nrow(subset(ip_urban2.df, spend == "Grocery" & pi == "Credit card")) )# num grocery cash payments
(ip_urban_frac_grocery_credit = ip_urban_num_grocery_credit/ip_urban_num_grocery)
# prop test rural vs urban grocery credit
(prop_test_grocery_credit = prop.test(x = c(ip_rural_num_grocery_credit, ip_urban_num_grocery_credit), n = c(ip_rural_num_grocery, ip_urban_num_grocery), correct = FALSE))
(pval_grocery_credit = ifelse(prop_test_grocery_credit$p.value < 0.01, "Yes", "No"))
#
# grocery debit card rural
ip_rural_num_grocery# num grocery payments
(ip_rural_num_grocery_debit = nrow(subset(ip_rural2.df, spend == "Grocery" & pi == "Debit card")) )# num grocery cash payments
(ip_rural_frac_grocery_debit = ip_rural_num_grocery_debit/ip_rural_num_grocery)
# grocery debit card mixed
ip_mixed_num_grocery# num grocery payments
(ip_mixed_num_grocery_debit = nrow(subset(ip_mixed2.df, spend == "Grocery" & pi == "Debit card")) )# num grocery cash payments
(ip_mixed_frac_grocery_debit = ip_mixed_num_grocery_debit/ip_mixed_num_grocery)
# grocery debit card urban
ip_urban_num_grocery# num grocery payments
(ip_urban_num_grocery_debit = nrow(subset(ip_urban2.df, spend == "Grocery" & pi == "Debit card")) )# num grocery cash payments
(ip_urban_frac_grocery_debit = ip_urban_num_grocery_debit/ip_urban_num_grocery)
# prop test rural vs urban grocery debit
(prop_test_grocery_debit = prop.test(x = c(ip_rural_num_grocery_debit, ip_urban_num_grocery_debit), n = c(ip_rural_num_grocery, ip_urban_num_grocery), correct = FALSE))
(pval_grocery_debit = ifelse(prop_test_grocery_debit$p.value < 0.01, "Yes", "No"))
#
# grocery prepaid card rural
ip_rural_num_grocery# num grocery payments
(ip_rural_num_grocery_prepaid = nrow(subset(ip_rural2.df, spend == "Grocery" & pi == "Prepaid card")) )# num grocery prepaid payments
(ip_rural_frac_grocery_prepaid = ip_rural_num_grocery_prepaid/ip_rural_num_grocery)
# grocery prepaid card mixed
ip_mixed_num_grocery# num grocery payments
(ip_mixed_num_grocery_prepaid = nrow(subset(ip_mixed2.df, spend == "Grocery" & pi == "Prepaid card")) )# num grocery prepaid payments
(ip_mixed_frac_grocery_prepaid = ip_mixed_num_grocery_prepaid/ip_mixed_num_grocery)
# grocery prepaid card urban
ip_urban_num_grocery# num grocery payments
(ip_urban_num_grocery_prepaid = nrow(subset(ip_urban2.df, spend == "Grocery" & pi == "Prepaid card")) )# num grocery prepaid payments
(ip_urban_frac_grocery_prepaid = ip_urban_num_grocery_prepaid/ip_urban_num_grocery)
# prop test rural vs urban grocery prepaid
(prop_test_grocery_prepaid = prop.test(x = c(ip_rural_num_grocery_prepaid, ip_urban_num_grocery_prepaid), n = c(ip_rural_num_grocery, ip_urban_num_grocery), correct = FALSE))
(pval_grocery_prepaid = ifelse(prop_test_grocery_prepaid$p.value < 0.01, "Yes", "No"))
#
# grocery check rural
ip_rural_num_grocery# num grocery payments
(ip_rural_num_grocery_check = nrow(subset(ip_rural2.df, spend == "Grocery" & pi == "Check")) )# num grocery check payments
(ip_rural_frac_grocery_check = ip_rural_num_grocery_check/ip_rural_num_grocery)
# grocery check mixed
ip_mixed_num_grocery# num grocery payments
(ip_mixed_num_grocery_check = nrow(subset(ip_mixed2.df, spend == "Grocery" & pi == "Check")) )# num grocery check payments
(ip_mixed_frac_grocery_check = ip_mixed_num_grocery_check/ip_mixed_num_grocery)
# grocery check urban
ip_urban_num_grocery# num grocery payments
(ip_urban_num_grocery_check = nrow(subset(ip_urban2.df, spend == "Grocery" & pi == "Check")) )# num grocery cash payments
(ip_urban_frac_grocery_check = ip_urban_num_grocery_check/ip_urban_num_grocery)
# prop test rural vs urban grocery check
(prop_test_grocery_check = prop.test(x = c(ip_rural_num_grocery_check, ip_urban_num_grocery_check), n = c(ip_rural_num_grocery, ip_urban_num_grocery), correct = FALSE))
(pval_grocery_check = ifelse(prop_test_grocery_check$p.value < 0.01, "Yes", "No"))
#
# grocery mobile rural
ip_rural_num_grocery# num grocery payments
(ip_rural_num_grocery_mobile = nrow(subset(ip_rural2.df, spend == "Grocery" & pi == "Mobile app")) )# num grocery mobile payments
(ip_rural_frac_grocery_mobile = ip_rural_num_grocery_mobile/ip_rural_num_grocery)
# grocery mobile mixed
ip_mixed_num_grocery# num grocery payments
(ip_mixed_num_grocery_mobile = nrow(subset(ip_mixed2.df, spend == "Grocery" & pi == "Mobile app")) )# num grocery mobile payments
(ip_mixed_frac_grocery_mobile = ip_mixed_num_grocery_mobile/ip_mixed_num_grocery)
# grocery mobile urban
ip_urban_num_grocery# num grocery payments
(ip_urban_num_grocery_mobile = nrow(subset(ip_urban2.df, spend == "Grocery" & pi == "Mobile app")) )# num grocery mobile payments
(ip_urban_frac_grocery_mobile = ip_urban_num_grocery_mobile/ip_urban_num_grocery)
# prop test rural vs urban grocery mobile
(prop_test_grocery_mobile = prop.test(x = c(ip_rural_num_grocery_mobile, ip_urban_num_grocery_mobile), n = c(ip_rural_num_grocery, ip_urban_num_grocery), correct = FALSE))
(pval_grocery_mobile = ifelse(prop_test_grocery_mobile$p.value < 0.01, "Yes", "No"))
#
# grocery other rural
ip_rural_num_grocery# num grocery payments
(ip_rural_num_grocery_other = nrow(subset(ip_rural2.df, spend == "Grocery" & pi == "Other")) )# num grocery other payments
(ip_rural_frac_grocery_other = ip_rural_num_grocery_other/ip_rural_num_grocery)
# grocery other mixed
ip_mixed_num_grocery# num grocery payments
(ip_mixed_num_grocery_other = nrow(subset(ip_mixed2.df, spend == "Grocery" & pi == "Other")) )# num grocery other payments
(ip_mixed_frac_grocery_other = ip_mixed_num_grocery_other/ip_mixed_num_grocery)
# grocery other urban
ip_urban_num_grocery# num grocery payments
(ip_urban_num_grocery_other = nrow(subset(ip_urban2.df, spend == "Grocery" & pi == "Other")) )# num grocery other payments
(ip_urban_frac_grocery_other = ip_urban_num_grocery_other/ip_urban_num_grocery)
# prop test rural vs urban grocery other
(prop_test_grocery_other = prop.test(x = c(ip_rural_num_grocery_other, ip_urban_num_grocery_other), n = c(ip_rural_num_grocery, ip_urban_num_grocery), correct = FALSE))
(pval_grocery_other = ifelse(prop_test_grocery_other$p.value < 0.01, "Yes", "No"))

## start Fast food / coffee shop PI columns
table(ip_rural2.df$pi, useNA = "always")
# fast cash rural
ip_rural_num_fast# num fast payments
(ip_rural_num_fast_cash = nrow(subset(ip_rural2.df, spend == "Fast" & pi == "Cash")) )# num fast cash payments
(ip_rural_frac_fast_cash = ip_rural_num_fast_cash/ip_rural_num_fast)
# fast cash mixed
ip_mixed_num_fast# num fast payments
(ip_mixed_num_fast_cash = nrow(subset(ip_mixed2.df, spend == "Fast" & pi == "Cash")) )# num fast cash payments
(ip_mixed_frac_fast_cash = ip_mixed_num_fast_cash/ip_mixed_num_fast)
# fast cash urban
ip_urban_num_fast# num fast payments
(ip_urban_num_fast_cash = nrow(subset(ip_urban2.df, spend == "Fast" & pi == "Cash")) )# num fast cash payments
(ip_urban_frac_fast_cash = ip_urban_num_fast_cash/ip_urban_num_fast)
# prop test rural vs urban fast cash
(prop_test_fast_cash = prop.test(x = c(ip_rural_num_fast_cash, ip_urban_num_fast_cash), n = c(ip_rural_num_fast, ip_urban_num_fast),
                                    correct = FALSE))   # disable Yates correction to get pure z-test
(pval_fast_cash = ifelse(prop_test_fast_cash$p.value < 0.01, "Yes", "No"))
#
# fast credit card rural
ip_rural_num_fast# num fast payments
(ip_rural_num_fast_credit = nrow(subset(ip_rural2.df, spend == "Fast" & pi == "Credit card")) )# num fast cash payments
(ip_rural_frac_fast_credit = ip_rural_num_fast_credit/ip_rural_num_fast)
# fast credit card mixed
ip_mixed_num_fast# num fast payments
(ip_mixed_num_fast_credit = nrow(subset(ip_mixed2.df, spend == "Fast" & pi == "Credit card")) )# num fast cash payments
(ip_mixed_frac_fast_credit = ip_mixed_num_fast_credit/ip_mixed_num_fast)
# fast credit card urban
ip_urban_num_fast# num fast payments
(ip_urban_num_fast_credit = nrow(subset(ip_urban2.df, spend == "Fast" & pi == "Credit card")) )# num fast cash payments
(ip_urban_frac_fast_credit = ip_urban_num_fast_credit/ip_urban_num_fast)
# prop test rural vs urban fast credit
(prop_test_fast_credit = prop.test(x = c(ip_rural_num_fast_credit, ip_urban_num_fast_credit), n = c(ip_rural_num_fast, ip_urban_num_fast), correct = FALSE))
(pval_fast_credit = ifelse(prop_test_fast_credit$p.value < 0.01, "Yes", "No"))
#
# fast debit card rural
ip_rural_num_fast# num fast payments
(ip_rural_num_fast_debit = nrow(subset(ip_rural2.df, spend == "Fast" & pi == "Debit card")) )# num fast cash payments
(ip_rural_frac_fast_debit = ip_rural_num_fast_debit/ip_rural_num_fast)
# fast debit card mixed
ip_mixed_num_fast# num fast payments
(ip_mixed_num_fast_debit = nrow(subset(ip_mixed2.df, spend == "Fast" & pi == "Debit card")) )# num fast cash payments
(ip_mixed_frac_fast_debit = ip_mixed_num_fast_debit/ip_mixed_num_fast)
# fast debit card urban
ip_urban_num_fast# num fast payments
(ip_urban_num_fast_debit = nrow(subset(ip_urban2.df, spend == "Fast" & pi == "Debit card")) )# num fast cash payments
(ip_urban_frac_fast_debit = ip_urban_num_fast_debit/ip_urban_num_fast)
# prop test rural vs urban fast debit
(prop_test_fast_debit = prop.test(x = c(ip_rural_num_fast_debit, ip_urban_num_fast_debit), n = c(ip_rural_num_fast, ip_urban_num_fast), correct = FALSE))
(pval_fast_debit = ifelse(prop_test_fast_debit$p.value < 0.01, "Yes", "No"))
#
# fast prepaid card rural
ip_rural_num_fast# num fast payments
(ip_rural_num_fast_prepaid = nrow(subset(ip_rural2.df, spend == "Fast" & pi == "Prepaid card")) )# num fast prepaid payments
(ip_rural_frac_fast_prepaid = ip_rural_num_fast_prepaid/ip_rural_num_fast)
# fast prepaid card mixed
ip_mixed_num_fast# num fast payments
(ip_mixed_num_fast_prepaid = nrow(subset(ip_mixed2.df, spend == "Fast" & pi == "Prepaid card")) )# num fast prepaid payments
(ip_mixed_frac_fast_prepaid = ip_mixed_num_fast_prepaid/ip_mixed_num_fast)
# fast prepaid card urban
ip_urban_num_fast# num fast payments
(ip_urban_num_fast_prepaid = nrow(subset(ip_urban2.df, spend == "Fast" & pi == "Prepaid card")) )# num fast prepaid payments
(ip_urban_frac_fast_prepaid = ip_urban_num_fast_prepaid/ip_urban_num_fast)
# prop test rural vs urban fast prepaid
(prop_test_fast_prepaid = prop.test(x = c(ip_rural_num_fast_prepaid, ip_urban_num_fast_prepaid), n = c(ip_rural_num_fast, ip_urban_num_fast), correct = FALSE))
(pval_fast_prepaid = ifelse(prop_test_fast_prepaid$p.value < 0.01, "Yes", "No"))
#
# fast check rural
ip_rural_num_fast# num fast payments
(ip_rural_num_fast_check = nrow(subset(ip_rural2.df, spend == "Fast" & pi == "Check")) )# num fast check payments
(ip_rural_frac_fast_check = ip_rural_num_fast_check/ip_rural_num_fast)
# fast check mixed
ip_mixed_num_fast# num fast payments
(ip_mixed_num_fast_check = nrow(subset(ip_mixed2.df, spend == "Fast" & pi == "Check")) )# num fast check payments
(ip_mixed_frac_fast_check = ip_mixed_num_fast_check/ip_mixed_num_fast)
# fast check urban
ip_urban_num_fast# num fast payments
(ip_urban_num_fast_check = nrow(subset(ip_urban2.df, spend == "Fast" & pi == "Check")) )# num fast cash payments
(ip_urban_frac_fast_check = ip_urban_num_fast_check/ip_urban_num_fast)
# prop test rural vs urban fast check
(prop_test_fast_check = prop.test(x = c(ip_rural_num_fast_check, ip_urban_num_fast_check), n = c(ip_rural_num_fast, ip_urban_num_fast), correct = FALSE))
(pval_fast_check = ifelse(prop_test_fast_check$p.value < 0.01, "Yes", "No"))
#
# fast mobile rural
ip_rural_num_fast# num fast payments
(ip_rural_num_fast_mobile = nrow(subset(ip_rural2.df, spend == "Fast" & pi == "Mobile app")) )# num fast mobile payments
(ip_rural_frac_fast_mobile = ip_rural_num_fast_mobile/ip_rural_num_fast)
# fast mobile mixed
ip_mixed_num_fast# num fast payments
(ip_mixed_num_fast_mobile = nrow(subset(ip_mixed2.df, spend == "Fast" & pi == "Mobile app")) )# num fast mobile payments
(ip_mixed_frac_fast_mobile = ip_mixed_num_fast_mobile/ip_mixed_num_fast)
# fast mobile urban
ip_urban_num_fast# num fast payments
(ip_urban_num_fast_mobile = nrow(subset(ip_urban2.df, spend == "Fast" & pi == "Mobile app")) )# num fast mobile payments
(ip_urban_frac_fast_mobile = ip_urban_num_fast_mobile/ip_urban_num_fast)
# prop test rural vs urban fast mobile
(prop_test_fast_mobile = prop.test(x = c(ip_rural_num_fast_mobile, ip_urban_num_fast_mobile), n = c(ip_rural_num_fast, ip_urban_num_fast), correct = FALSE))
(pval_fast_mobile = ifelse(prop_test_fast_mobile$p.value < 0.01, "Yes", "No"))
#
# fast other rural
ip_rural_num_fast# num fast payments
(ip_rural_num_fast_other = nrow(subset(ip_rural2.df, spend == "Fast" & pi == "Other")) )# num fast other payments
(ip_rural_frac_fast_other = ip_rural_num_fast_other/ip_rural_num_fast)
# fast other mixed
ip_mixed_num_fast# num fast payments
(ip_mixed_num_fast_other = nrow(subset(ip_mixed2.df, spend == "Fast" & pi == "Other")) )# num fast other payments
(ip_mixed_frac_fast_other = ip_mixed_num_fast_other/ip_mixed_num_fast)
# fast other urban
ip_urban_num_fast# num fast payments
(ip_urban_num_fast_other = nrow(subset(ip_urban2.df, spend == "Fast" & pi == "Other")) )# num fast other payments
(ip_urban_frac_fast_other = ip_urban_num_fast_other/ip_urban_num_fast)
# prop test rural vs urban fast other
(prop_test_fast_other = prop.test(x = c(ip_rural_num_fast_other, ip_urban_num_fast_other), n = c(ip_rural_num_fast, ip_urban_num_fast), correct = FALSE))
(pval_fast_other = ifelse(prop_test_fast_other$p.value < 0.01, "Yes", "No"))

## start General store PI columns
table(ip_rural2.df$pi, useNA = "always")
table(ip_rural2.df$spend, useNA = "always")
# general cash rural
ip_rural_num_general# num general payments
(ip_rural_num_general_cash = nrow(subset(ip_rural2.df, spend == "General" & pi == "Cash")) )# num general cash payments
(ip_rural_frac_general_cash = ip_rural_num_general_cash/ip_rural_num_general)
# general cash mixed
ip_mixed_num_general# num general payments
(ip_mixed_num_general_cash = nrow(subset(ip_mixed2.df, spend == "General" & pi == "Cash")) )# num general cash payments
(ip_mixed_frac_general_cash = ip_mixed_num_general_cash/ip_mixed_num_general)
# general cash urban
ip_urban_num_general# num general payments
(ip_urban_num_general_cash = nrow(subset(ip_urban2.df, spend == "General" & pi == "Cash")) )# num general cash payments
(ip_urban_frac_general_cash = ip_urban_num_general_cash/ip_urban_num_general)
# prop text rural vs urban general cash
(prop_test_general_cash = prop.test(x = c(ip_rural_num_general_cash, ip_urban_num_general_cash), n = c(ip_rural_num_general, ip_urban_num_general),
                                 correct = FALSE))   # disable Yates correction to get pure z-test
(pval_general_cash = ifelse(prop_test_general_cash$p.value < 0.01, "Yes", "No"))
#
# general credit card rural
ip_rural_num_general# num general payments
(ip_rural_num_general_credit = nrow(subset(ip_rural2.df, spend == "General" & pi == "Credit card")) )# num general cash payments
(ip_rural_frac_general_credit = ip_rural_num_general_credit/ip_rural_num_general)
# general credit card mixed
ip_mixed_num_general# num general payments
(ip_mixed_num_general_credit = nrow(subset(ip_mixed2.df, spend == "General" & pi == "Credit card")) )# num general cash payments
(ip_mixed_frac_general_credit = ip_mixed_num_general_credit/ip_mixed_num_general)
# general credit card urban
ip_urban_num_general# num general payments
(ip_urban_num_general_credit = nrow(subset(ip_urban2.df, spend == "General" & pi == "Credit card")) )# num general cash payments
(ip_urban_frac_general_credit = ip_urban_num_general_credit/ip_urban_num_general)
# prop test rural vs urban general credit
(prop_test_general_credit = prop.test(x = c(ip_rural_num_general_credit, ip_urban_num_general_credit), n = c(ip_rural_num_general, ip_urban_num_general), correct = FALSE))
(pval_general_credit = ifelse(prop_test_general_credit$p.value < 0.01, "Yes", "No"))
#
# general debit card rural
ip_rural_num_general# num general payments
(ip_rural_num_general_debit = nrow(subset(ip_rural2.df, spend == "General" & pi == "Debit card")) )# num general cash payments
(ip_rural_frac_general_debit = ip_rural_num_general_debit/ip_rural_num_general)
# general debit card mixed
ip_mixed_num_general# num general payments
(ip_mixed_num_general_debit = nrow(subset(ip_mixed2.df, spend == "General" & pi == "Debit card")) )# num general cash payments
(ip_mixed_frac_general_debit = ip_mixed_num_general_debit/ip_mixed_num_general)
# general debit card urban
ip_urban_num_general# num general payments
(ip_urban_num_general_debit = nrow(subset(ip_urban2.df, spend == "General" & pi == "Debit card")) )# num general cash payments
(ip_urban_frac_general_debit = ip_urban_num_general_debit/ip_urban_num_general)
# prop test rural vs urban general debit
(prop_test_general_debit = prop.test(x = c(ip_rural_num_general_debit, ip_urban_num_general_debit), n = c(ip_rural_num_general, ip_urban_num_general), correct = FALSE))
(pval_general_debit = ifelse(prop_test_general_debit$p.value < 0.01, "Yes", "No"))
#
# general prepaid card rural
ip_rural_num_general# num general payments
(ip_rural_num_general_prepaid = nrow(subset(ip_rural2.df, spend == "General" & pi == "Prepaid card")) )# num general prepaid payments
(ip_rural_frac_general_prepaid = ip_rural_num_general_prepaid/ip_rural_num_general)
# general prepaid card mixed
ip_mixed_num_general# num general payments
(ip_mixed_num_general_prepaid = nrow(subset(ip_mixed2.df, spend == "General" & pi == "Prepaid card")) )# num general prepaid payments
(ip_mixed_frac_general_prepaid = ip_mixed_num_general_prepaid/ip_mixed_num_general)
# general prepaid card urban
ip_urban_num_general# num general payments
(ip_urban_num_general_prepaid = nrow(subset(ip_urban2.df, spend == "General" & pi == "Prepaid card")) )# num general prepaid payments
(ip_urban_frac_general_prepaid = ip_urban_num_general_prepaid/ip_urban_num_general)
# prop test rural vs urban general prepaid
(prop_test_general_prepaid = prop.test(x = c(ip_rural_num_general_prepaid, ip_urban_num_general_prepaid), n = c(ip_rural_num_general, ip_urban_num_general), correct = FALSE))
(pval_general_prepaid = ifelse(prop_test_general_prepaid$p.value < 0.01, "Yes", "No"))
#
# general check rural
ip_rural_num_general# num general payments
(ip_rural_num_general_check = nrow(subset(ip_rural2.df, spend == "General" & pi == "Check")) )# num general check payments
(ip_rural_frac_general_check = ip_rural_num_general_check/ip_rural_num_general)
# general check mixed
ip_mixed_num_general# num general payments
(ip_mixed_num_general_check = nrow(subset(ip_mixed2.df, spend == "General" & pi == "Check")) )# num general check payments
(ip_mixed_frac_general_check = ip_mixed_num_general_check/ip_mixed_num_general)
# general check urban
ip_urban_num_general# num general payments
(ip_urban_num_general_check = nrow(subset(ip_urban2.df, spend == "General" & pi == "Check")) )# num general cash payments
(ip_urban_frac_general_check = ip_urban_num_general_check/ip_urban_num_general)
# prop test rural vs urban general check
(prop_test_general_check = prop.test(x = c(ip_rural_num_general_check, ip_urban_num_general_check), n = c(ip_rural_num_general, ip_urban_num_general), correct = FALSE))
(pval_general_check = ifelse(prop_test_general_check$p.value < 0.01, "Yes", "No"))
#
# general mobile rural
ip_rural_num_general# num general payments
(ip_rural_num_general_mobile = nrow(subset(ip_rural2.df, spend == "General" & pi == "Mobile app")) )# num general mobile payments
(ip_rural_frac_general_mobile = ip_rural_num_general_mobile/ip_rural_num_general)
# general mobile mixed
ip_mixed_num_general# num general payments
(ip_mixed_num_general_mobile = nrow(subset(ip_mixed2.df, spend == "General" & pi == "Mobile app")) )# num general mobile payments
(ip_mixed_frac_general_mobile = ip_mixed_num_general_mobile/ip_mixed_num_general)
# general mobile urban
ip_urban_num_general# num general payments
(ip_urban_num_general_mobile = nrow(subset(ip_urban2.df, spend == "General" & pi == "Mobile app")) )# num general mobile payments
(ip_urban_frac_general_mobile = ip_urban_num_general_mobile/ip_urban_num_general)
# prop test rural vs urban general mobile
(prop_test_general_mobile = prop.test(x = c(ip_rural_num_general_mobile, ip_urban_num_general_mobile), n = c(ip_rural_num_general, ip_urban_num_general), correct = FALSE))
(pval_general_mobile = ifelse(prop_test_general_mobile$p.value < 0.01, "Yes", "No"))
#
# general other rural
ip_rural_num_general# num general payments
(ip_rural_num_general_other = nrow(subset(ip_rural2.df, spend == "General" & pi == "Other")) )# num general other payments
(ip_rural_frac_general_other = ip_rural_num_general_other/ip_rural_num_general)
# general other mixed
ip_mixed_num_general# num general payments
(ip_mixed_num_general_other = nrow(subset(ip_mixed2.df, spend == "General" & pi == "Other")) )# num general other payments
(ip_mixed_frac_general_other = ip_mixed_num_general_other/ip_mixed_num_general)
# general other urban
ip_urban_num_general# num general payments
(ip_urban_num_general_other = nrow(subset(ip_urban2.df, spend == "General" & pi == "Other")) )# num general other payments
(ip_urban_frac_general_other = ip_urban_num_general_other/ip_urban_num_general)
# prop test rural vs urban general other
(prop_test_general_other = prop.test(x = c(ip_rural_num_general_other, ip_urban_num_general_other), n = c(ip_rural_num_general, ip_urban_num_general), correct = FALSE))
(pval_general_other = ifelse(prop_test_general_other$p.value < 0.01, "Yes", "No"))

## start P2P PI columns
table(ip_rural2.df$pi, useNA = "always")
table(ip_rural2.df$spend, useNA = "always")
# p2p cash rural
ip_rural_num_p2p# num p2p payments
(ip_rural_num_p2p_cash = nrow(subset(ip_rural2.df, spend == "P2P" & pi == "Cash")) )# num p2p cash payments
(ip_rural_frac_p2p_cash = ip_rural_num_p2p_cash/ip_rural_num_p2p)
# p2p cash mixed
ip_mixed_num_p2p# num p2p payments
(ip_mixed_num_p2p_cash = nrow(subset(ip_mixed2.df, spend == "P2P" & pi == "Cash")) )# num p2p cash payments
(ip_mixed_frac_p2p_cash = ip_mixed_num_p2p_cash/ip_mixed_num_p2p)
# p2p cash urban
ip_urban_num_p2p# num p2p payments
(ip_urban_num_p2p_cash = nrow(subset(ip_urban2.df, spend == "P2P" & pi == "Cash")) )# num p2p cash payments
(ip_urban_frac_p2p_cash = ip_urban_num_p2p_cash/ip_urban_num_p2p)
# prop text rural vs urban p2p cash
(prop_test_p2p_cash = prop.test(x = c(ip_rural_num_p2p_cash, ip_urban_num_p2p_cash), n = c(ip_rural_num_p2p, ip_urban_num_p2p),
correct = FALSE))   # disable Yates correction to get pure z-test
(pval_p2p_cash = ifelse(prop_test_p2p_cash$p.value < 0.01, "Yes", "No"))
#
# p2p credit card rural
ip_rural_num_p2p# num p2p payments
(ip_rural_num_p2p_credit = nrow(subset(ip_rural2.df, spend == "P2P" & pi == "Credit card")) )# num p2p credit payments
(ip_rural_frac_p2p_credit = ip_rural_num_p2p_credit/ip_rural_num_p2p)
# p2p credit card mixed
ip_mixed_num_p2p# num p2p payments
(ip_mixed_num_p2p_credit = nrow(subset(ip_mixed2.df, spend == "P2P" & pi == "Credit card")) )# num p2p credit payments
(ip_mixed_frac_p2p_credit = ip_mixed_num_p2p_credit/ip_mixed_num_p2p)
# p2p credit card urban
ip_urban_num_p2p# num p2p payments
(ip_urban_num_p2p_credit = nrow(subset(ip_urban2.df, spend == "P2P" & pi == "Credit card")) )# num p2p credit payments
(ip_urban_frac_p2p_credit = ip_urban_num_p2p_credit/ip_urban_num_p2p)
# prop test rural vs urban p2p credit
(prop_test_p2p_credit = prop.test(x = c(ip_rural_num_p2p_credit, ip_urban_num_p2p_credit), n = c(ip_rural_num_p2p, ip_urban_num_p2p), correct = FALSE))
(pval_p2p_credit = ifelse(prop_test_p2p_credit$p.value < 0.01, "Yes", "No"))
#
# p2p debit card rural
ip_rural_num_p2p# num p2p payments
(ip_rural_num_p2p_debit = nrow(subset(ip_rural2.df, spend == "P2P" & pi == "Debit card")) )# num p2p debit payments
(ip_rural_frac_p2p_debit = ip_rural_num_p2p_debit/ip_rural_num_p2p)
# p2p debit card mixed
ip_mixed_num_p2p# num p2p payments
(ip_mixed_num_p2p_debit = nrow(subset(ip_mixed2.df, spend == "P2P" & pi == "Debit card")) )# num p2p debit payments
(ip_mixed_frac_p2p_debit = ip_mixed_num_p2p_debit/ip_mixed_num_p2p)
# p2p debit card urban
ip_urban_num_p2p# num p2p payments
(ip_urban_num_p2p_debit = nrow(subset(ip_urban2.df, spend == "P2P" & pi == "Debit card")) )# num p2p debit payments
(ip_urban_frac_p2p_debit = ip_urban_num_p2p_debit/ip_urban_num_p2p)
# prop test rural vs urban p2p debit
(prop_test_p2p_debit = prop.test(x = c(ip_rural_num_p2p_debit, ip_urban_num_p2p_debit), n = c(ip_rural_num_p2p, ip_urban_num_p2p), correct = FALSE))
(pval_p2p_debit = ifelse(prop_test_p2p_debit$p.value < 0.01, "Yes", "No"))
#
# p2p prepaid card rural
ip_rural_num_p2p# num p2p payments
(ip_rural_num_p2p_prepaid = nrow(subset(ip_rural2.df, spend == "P2P" & pi == "Prepaid card")) )# num p2p prepaid payments
(ip_rural_frac_p2p_prepaid = ip_rural_num_p2p_prepaid/ip_rural_num_p2p)
# p2p prepaid card mixed
ip_mixed_num_p2p# num p2p payments
(ip_mixed_num_p2p_prepaid = nrow(subset(ip_mixed2.df, spend == "P2P" & pi == "Prepaid card")) )# num p2p prepaid payments
(ip_mixed_frac_p2p_prepaid = ip_mixed_num_p2p_prepaid/ip_mixed_num_p2p)
# p2p prepaid card urban
ip_urban_num_p2p# num p2p payments
(ip_urban_num_p2p_prepaid = nrow(subset(ip_urban2.df, spend == "P2P" & pi == "Prepaid card")) )# num p2p prepaid payments
(ip_urban_frac_p2p_prepaid = ip_urban_num_p2p_prepaid/ip_urban_num_p2p)
# prop test rural vs urban p2p prepaid
(prop_test_p2p_prepaid = prop.test(x = c(ip_rural_num_p2p_prepaid, ip_urban_num_p2p_prepaid), n = c(ip_rural_num_p2p, ip_urban_num_p2p), correct = FALSE))
(pval_p2p_prepaid = ifelse(prop_test_p2p_prepaid$p.value < 0.01, "Yes", "No"))
#
# p2p check rural
ip_rural_num_p2p# num p2p payments
(ip_rural_num_p2p_check = nrow(subset(ip_rural2.df, spend == "P2P" & pi == "Check")) )# num p2p check payments
(ip_rural_frac_p2p_check = ip_rural_num_p2p_check/ip_rural_num_p2p)
# p2p check mixed
ip_mixed_num_p2p# num p2p payments
(ip_mixed_num_p2p_check = nrow(subset(ip_mixed2.df, spend == "P2P" & pi == "Check")) )# num p2p check payments
(ip_mixed_frac_p2p_check = ip_mixed_num_p2p_check/ip_mixed_num_p2p)
# p2p check urban
ip_urban_num_p2p# num p2p payments
(ip_urban_num_p2p_check = nrow(subset(ip_urban2.df, spend == "P2P" & pi == "Check")) )# num p2p cash payments
(ip_urban_frac_p2p_check = ip_urban_num_p2p_check/ip_urban_num_p2p)
# prop test rural vs urban p2p check
(prop_test_p2p_check = prop.test(x = c(ip_rural_num_p2p_check, ip_urban_num_p2p_check), n = c(ip_rural_num_p2p, ip_urban_num_p2p), correct = FALSE))
(pval_p2p_check = ifelse(prop_test_p2p_check$p.value < 0.01, "Yes", "No"))
#
# p2p mobile rural
ip_rural_num_p2p# num p2p payments
(ip_rural_num_p2p_mobile = nrow(subset(ip_rural2.df, spend == "P2P" & pi == "Mobile app")) )# num p2p mobile payments
(ip_rural_frac_p2p_mobile = ip_rural_num_p2p_mobile/ip_rural_num_p2p)
# p2p mobile mixed
ip_mixed_num_p2p# num p2p payments
(ip_mixed_num_p2p_mobile = nrow(subset(ip_mixed2.df, spend == "P2P" & pi == "Mobile app")) )# num p2p mobile payments
(ip_mixed_frac_p2p_mobile = ip_mixed_num_p2p_mobile/ip_mixed_num_p2p)
# p2p mobile urban
ip_urban_num_p2p# num p2p payments
(ip_urban_num_p2p_mobile = nrow(subset(ip_urban2.df, spend == "P2P" & pi == "Mobile app")) )# num p2p mobile payments
(ip_urban_frac_p2p_mobile = ip_urban_num_p2p_mobile/ip_urban_num_p2p)
# prop test rural vs urban p2p mobile
(prop_test_p2p_mobile = prop.test(x = c(ip_rural_num_p2p_mobile, ip_urban_num_p2p_mobile), n = c(ip_rural_num_p2p, ip_urban_num_p2p), correct = FALSE))
(pval_p2p_mobile = ifelse(prop_test_p2p_mobile$p.value < 0.01, "Yes", "No"))
#
# p2p other rural
ip_rural_num_p2p# num p2p payments
(ip_rural_num_p2p_other = nrow(subset(ip_rural2.df, spend == "P2P" & pi == "Other")) )# num p2p other payments
(ip_rural_frac_p2p_other = ip_rural_num_p2p_other/ip_rural_num_p2p)
# p2p other mixed
ip_mixed_num_p2p# num p2p payments
(ip_mixed_num_p2p_other = nrow(subset(ip_mixed2.df, spend == "P2P" & pi == "Other")) )# num p2p other payments
(ip_mixed_frac_p2p_other = ip_mixed_num_p2p_other/ip_mixed_num_p2p)
# p2p other urban
ip_urban_num_p2p# num p2p payments
(ip_urban_num_p2p_other = nrow(subset(ip_urban2.df, spend == "P2P" & pi == "Other")) )# num p2p other payments
(ip_urban_frac_p2p_other = ip_urban_num_p2p_other/ip_urban_num_p2p)
# prop test rural vs urban p2p other
(prop_test_p2p_other = prop.test(x = c(ip_rural_num_p2p_other, ip_urban_num_p2p_other), n = c(ip_rural_num_p2p, ip_urban_num_p2p), correct = FALSE))
(pval_p2p_other = ifelse(prop_test_p2p_other$p.value < 0.01, "Yes", "No"))

table(ip_rural2.df$pi, useNA = "always")
table(ip_rural2.df$spend, useNA = "always")
# construct cash column for spend.t
(spend_cash.vec = c(ip_rural_frac_grocery_cash, ip_mixed_frac_grocery_cash, ip_urban_frac_grocery_cash, pval_grocery_cash, ip_rural_frac_fast_cash, ip_mixed_frac_fast_cash, ip_urban_frac_fast_cash, pval_fast_cash, ip_rural_frac_general_cash, ip_mixed_frac_general_cash, ip_urban_frac_general_cash, pval_general_cash, ip_rural_frac_p2p_cash, ip_mixed_frac_p2p_cash, ip_urban_frac_p2p_cash, pval_p2p_cash))
# construct credit column for spend.t
(spend_credit.vec = c(ip_rural_frac_grocery_credit, ip_mixed_frac_grocery_credit, ip_urban_frac_grocery_credit, pval_grocery_credit, ip_rural_frac_fast_credit, ip_mixed_frac_fast_credit, ip_urban_frac_fast_credit, pval_fast_credit, ip_rural_frac_general_credit, ip_mixed_frac_general_credit, ip_urban_frac_general_credit, pval_general_credit, ip_rural_frac_p2p_credit, ip_mixed_frac_p2p_credit, ip_urban_frac_p2p_credit, pval_p2p_credit))
# construct debit column for spend.t
(spend_debit.vec = c(ip_rural_frac_grocery_debit, ip_mixed_frac_grocery_debit, ip_urban_frac_grocery_debit, pval_grocery_debit, ip_rural_frac_fast_debit, ip_mixed_frac_fast_debit, ip_urban_frac_fast_debit, pval_fast_debit, ip_rural_frac_general_debit, ip_mixed_frac_general_debit, ip_urban_frac_general_debit, pval_general_debit, ip_rural_frac_p2p_debit, ip_mixed_frac_p2p_debit, ip_urban_frac_p2p_debit, pval_p2p_debit))
# construct prepaid column for spend.t
(spend_prepaid.vec = c(ip_rural_frac_grocery_prepaid, ip_mixed_frac_grocery_prepaid, ip_urban_frac_grocery_prepaid, pval_grocery_prepaid, ip_rural_frac_fast_prepaid, ip_mixed_frac_fast_prepaid, ip_urban_frac_fast_prepaid, pval_fast_prepaid, ip_rural_frac_general_prepaid, ip_mixed_frac_general_prepaid, ip_urban_frac_general_prepaid, pval_general_prepaid, ip_rural_frac_p2p_prepaid, ip_mixed_frac_p2p_prepaid, ip_urban_frac_p2p_prepaid, pval_p2p_prepaid))
# construct check column for spend.t
(spend_check.vec = c(ip_rural_frac_grocery_check, ip_mixed_frac_grocery_check, ip_urban_frac_grocery_check, pval_grocery_check, ip_rural_frac_fast_check, ip_mixed_frac_fast_check, ip_urban_frac_fast_check, pval_fast_check, ip_rural_frac_general_check, ip_mixed_frac_general_check, ip_urban_frac_general_check, pval_general_check, ip_rural_frac_p2p_check, ip_mixed_frac_p2p_check, ip_urban_frac_p2p_check, pval_p2p_check))
# construct mobile column for spend.t
(spend_mobile.vec = c(ip_rural_frac_grocery_mobile, ip_mixed_frac_grocery_mobile, ip_urban_frac_grocery_mobile, pval_grocery_mobile, ip_rural_frac_fast_mobile, ip_mixed_frac_fast_mobile, ip_urban_frac_fast_mobile, pval_fast_mobile, ip_rural_frac_general_mobile, ip_mixed_frac_general_mobile, ip_urban_frac_general_mobile, pval_general_mobile, ip_rural_frac_p2p_mobile, ip_mixed_frac_p2p_mobile, ip_urban_frac_p2p_mobile, pval_p2p_mobile))
# construct other column for spend.t
(spend_other.vec = c(ip_rural_frac_grocery_other, ip_mixed_frac_grocery_other, ip_urban_frac_grocery_other, pval_grocery_other, ip_rural_frac_fast_other, ip_mixed_frac_fast_other, ip_urban_frac_fast_other, pval_fast_other, ip_rural_frac_general_other, ip_mixed_frac_general_other, ip_urban_frac_general_other, pval_general_other, ip_rural_frac_p2p_other, ip_mixed_frac_p2p_other, ip_urban_frac_p2p_other, pval_p2p_other))

# turn the vectors into numeric and then % => pval (chr) are lost and will have to be reinserted afterwords
(spend_cash2.vec = as.numeric(spend_cash.vec))
length(spend_cash2.vec)
(spend_cash3.vec = paste0(round(spend_cash2.vec * 100, 0), "%"))
(spend_cash4.vec = c(spend_cash3.vec[1:3], spend_cash.vec[4], spend_cash3.vec[5:7], spend_cash.vec[8], spend_cash3.vec[9:11], spend_cash.vec[12], spend_cash3.vec[13:15], spend_cash.vec[16]))
#
(spend_credit2.vec = as.numeric(spend_credit.vec))
length(spend_credit2.vec)
(spend_credit3.vec = paste0(round(spend_credit2.vec * 100, 0), "%"))
(spend_credit4.vec = c(spend_credit3.vec[1:3], spend_credit.vec[4], spend_credit3.vec[5:7], spend_credit.vec[8], spend_credit3.vec[9:11], spend_credit.vec[12], spend_credit3.vec[13:15], spend_credit.vec[16]))
#
(spend_debit2.vec = as.numeric(spend_debit.vec))
length(spend_debit2.vec)
(spend_debit3.vec = paste0(round(spend_debit2.vec * 100, 0), "%"))
(spend_debit4.vec = c(spend_debit3.vec[1:3], spend_debit.vec[4], spend_debit3.vec[5:7], spend_debit.vec[8], spend_debit3.vec[9:11], spend_debit.vec[12], spend_debit3.vec[13:15], spend_debit.vec[16]))
#
(spend_prepaid2.vec = as.numeric(spend_prepaid.vec))
length(spend_prepaid2.vec)
(spend_prepaid3.vec = paste0(round(spend_prepaid2.vec * 100, 0), "%"))
(spend_prepaid4.vec = c(spend_prepaid3.vec[1:3], spend_prepaid.vec[4], spend_prepaid3.vec[5:7], spend_prepaid.vec[8], spend_prepaid3.vec[9:11], spend_prepaid.vec[12], spend_prepaid3.vec[13:15], spend_prepaid.vec[16]))
#
(spend_check2.vec = as.numeric(spend_check.vec))
length(spend_check2.vec)
(spend_check3.vec = paste0(round(spend_check2.vec * 100, 0), "%"))
(spend_check4.vec = c(spend_check3.vec[1:3], spend_check.vec[4], spend_check3.vec[5:7], spend_check.vec[8], spend_check3.vec[9:11], spend_check.vec[12],  spend_check3.vec[13:15], spend_check.vec[16]))
#
(spend_mobile2.vec = as.numeric(spend_mobile.vec))
length(spend_mobile2.vec)
(spend_mobile3.vec = paste0(round(spend_mobile2.vec * 100, 0), "%"))
(spend_mobile4.vec = c(spend_mobile3.vec[1:3], spend_mobile.vec[4], spend_mobile3.vec[5:7], spend_mobile.vec[8], spend_mobile3.vec[9:11], spend_mobile.vec[12], spend_mobile3.vec[13:15], spend_mobile.vec[16]))
#
(spend_other2.vec = as.numeric(spend_other.vec))
length(spend_other2.vec)
(spend_other3.vec = paste0(round(spend_other2.vec * 100, 0), "%"))
(spend_other4.vec = c(spend_other3.vec[1:3], spend_other.vec[4], spend_other3.vec[5:7], spend_other.vec[8], spend_other3.vec[9:11], spend_other.vec[12], spend_other3.vec[13:15], spend_other.vec[16]))
length(spend_other4.vec)
#
(spend_frac2.vec = as.numeric(spend_frac.vec))
length(spend_frac2.vec)
(spend_frac3.vec = paste0(round(spend_frac2.vec * 100, 0), "%"))
(spend_frac4.vec = c(spend_frac3.vec[1:3], spend_frac.vec[4], spend_frac3.vec[5:7], spend_frac.vec[8], spend_frac3.vec[9:11], spend_frac.vec[12], spend_frac3.vec[13:15], spend_frac.vec[16]))
length(spend_frac4.vec)

# Finalize spent.t table
(spend1.df = data.frame(Payments=spend_num.vec, Percent=spend_frac4.vec))
dim(spend1.df)
# add columns with PI
(spend2.df=cbind(spend1.df, Cash=spend_cash4.vec, Credit=spend_credit4.vec, Debit=spend_debit4.vec, Prepaid=spend_prepaid4.vec, Check=spend_check4.vec, Mobile=spend_mobile4.vec, Other=spend_other4.vec))
dim(spend2.df)
# replace NA with "No" NOTE: The column Payments NA should be blank in the final table
spend3.df = spend2.df
spend3.df[is.na(spend3.df)] = "No"
spend3.df
# add Area column
(spend_area.vec = c("Rural", "Mixed", "Urban", "Pval", "Rural", "Mixed", "Urban", "Pval", "Rural", "Mixed", "Urban", "Pval", "Rural", "Mixed", "Urban", "Pval"))
length(spend_area.vec)
(spend4.df = cbind(Area = spend_area.vec, spend3.df))

# Check: digits_matrix aligns as [rownames + rows] × [rownames + columns]
print(xtable(spend4.df, digits=0), include.rownames = F, hline.after = c(0,4,8,12))


#End: Table 6 Selected spending categories####

#++++++++++++++++++++++++++++++

#Begin: Reduced random forest (not in the paper)####
#reduced random forest model (delete ownership features)
# names(ip_m4.df)
# rf_ip_pi_reduced.model = pi ~ Amount + HH_income + Age + Gender + Area + Race + Hispanic + Education
# 
# set.seed(1955)
# 
# (forest_output =randomForest(rf_ip_pi_reduced.model, data = ip_m4.df, importance=T, na.action=na.roughfix))
# # defaults: mtry = rounded downwards sqrt(#predictors), nodesize=1, na.action =na.roughtfix => imputations.
# forest_output$type# verify classification tree (not regression tree)
# #forest_output$confusion based on the OOB sample (Instead, I use train-test subsamples for the confusion matrix)
# 
# # Table of variable importance for the entire sample
# forest_importance.df =importance(forest_output)
# str(forest_importance.df)
# (forest_importance2.df = as.data.frame(forest_importance.df))
# # Below, Plot of variable importance (not in paper)
# #varImpPlot(random_forest_output, type = 1, main ='', bg = "blue", cex=2)#default type 1&2,
# 
# # Rename rows
# (forest_importance3.df = round(forest_importance2.df, digits = 2))
# row.names(forest_importance3.df)
# #
# # Delete the GINI MDA from the importance table
# names(forest_importance3.df)
# dim(forest_importance3.df)
# (forest_importance4.df = forest_importance3.df[,1:8])

#End: Reduced random forest (not in the paper)####

#++++++++++++++++++++++++++++++

#Begin: Classification tree (not in the paper)####

# # tree model
# names(ip_m4.df)
# tree_ip_pi.model = pi ~ Amount + HH_income + Age + Gender + Area +Education +Race +Hispanic + Own_credit + Own_debit  + Credit_reward
# 
# set.seed(1955)
# tree1 = rpart(tree_ip_pi.model, data = ip_m4.df, method = "class", control = rpart.control(cp = 0.001))# Extremely-long tree first, then prune it
# #Below, plot a tree (Note: Longer than optimal, but needed for later prunning and redrawing). 
# prp(tree1, type = 3, box.palette = "auto", extra = 100, under = T, tweak = 1.0, varlen = 0, faclen = 0)#faclet=0 avoids abvreviations, tweak for char size
# #now search for optimal cp, rpart has cp table built in
# plotcp(tree1)# plot cp: 
# #names(tree1)
# tree1$cptable # List cp, number of splits and errors
# # Below, I choose cp to use for prunning. There are 2 methods:
# #Method 1: Use the plotcp and pick the highest relative error below the dashed line => may lead to a larger tree than needed.
# #
# #Method 2 (1-SE rule, preferred): Pick the lowest xerror in the cptable. Add the corresponding xstd. Then, pick the cp in the cptable with xerror < or = to this sum.
# #
# (cp.choice = tree1$cptable[10, "CP"]) # Corresponds to 9 splits (just for demonstration)
# prune1 = prune.rpart(tree1, cp=cp.choice)
# # plot prunned tree
# prp(prune1, type = 3, box.palette = "auto", legend.x=NA, legend.y=NA, extra = 100, under = T, varlen = 0, faclen = 0, Margin = 0.0, digits = -2, tweak = 0.9, cex = 1.0)
# #Note: Maximize the Plots window in RStudio before prp
# #faclen = 0 → no abbreviation (print full factor level names)
# #faclen = n → abbreviate factor levels to n characters tweak for char size
# #tweak also controls the fontsize (in addition or replacement for cex = 1.x, but it looks good this way)
# #varlen = -1 (default) → use full variable names
# #varlen = 0 → shortest unique abbreviation
# #varlen = n → truncate variable names to at most n characters
# # extra = 0 → show the predicted class only (default)
# #extra = 1 → show number of observations in the node
# #extra = 2 → show class probabilities
# #extra = 4 → show class percentages
# #extra = 5 → show class and probability
# #extra = 6 → show class, probability, and N
# #extra = 7 → show class, N, and percentage
# # For regression trees:  extra = 101 → show fitted value; extra = 104 → show fitted value and number of observations
# # Information for Fig 3 (tree)
# nrow(ip_m4.df)
# length(unique(ip_m4.df$id))

#End: Classification tree (not in the paper)####

#++++++++++++++++++++++++++++++
