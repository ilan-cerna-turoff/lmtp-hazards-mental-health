# File: Final LMTP analysis - India
# Date: 07/20/2023
# Author: Ilan Cerna-Turoff

#------------------------------------------------------------------------------
#### 0. LOAD DEVELOPER PACKAGE  ####
#------------------------------------------------------------------------------
#devtools::install_github("nt-williams/lmtp@separate-variable-sets", force = T)

library(tidyverse)
library(mice)
library(lmtp)
library(utils)
library(nnls)
library(earth)

#------------------------------------------------------------------------------
#### 1. LOAD DATA  ####
#------------------------------------------------------------------------------
df1 <- read.csv("india_final_data.csv", sep = ";")
commid <- read.csv("india_commid.csv", sep = ";")

#------------------------------------------------------------------------------
#### 2. IMPUTATION  ####
#------------------------------------------------------------------------------
df1 <- df1 %>%
  select(-c(momdied,divorce,otherlang,chchristian,chmuslim,chother,L1_adultlit,L2_adultlit)) %>%
  distinct() %>%
  mutate_at(c("careage","wellbeing","hhsize","L1_pop","A1","L2_pop","A2","gad7score",
              "phq8score"), as.numeric) %>%
  mutate_at(c("childid","chsex","mightdie","fatherill","motherill",
              "ownhouse","ownanimals","telugu","chhindu","bc","no_caste",
              "sc","carenone","chprimary","chsecondary","chuniversity","L1_city",
              "L1_sochel","L2_sochel"), as.factor)

#Calculate predictor matrix
ini <- mice(df1, maxit=0) 

#Use predictor matrix to exclude ID variable and outcomes
pred1 <- ini$predictorMatrix 
pred1[,'childid'] <- 0 
pred1[,'gad7score'] <- 0
pred1[,'phq8score'] <- 0

#Methods for covariate imputation
meth_analysis <- ini$meth
meth_analysis = c("","","","","","logreg","pmm","","","",
                  "","logreg","","","","","logreg","logreg","logreg","logreg",
                  "pmm","logreg","","pmm","logreg","","","")

#Run full imputation
imp <- mice(df1, m = 5, seed = 9999, meth = meth_analysis, pred = pred1)

#Merge with community IDs
full.imputed <- complete(imp, action = "long", include = TRUE)
imp.combined <- left_join(full.imputed, commid, by = "childid")

#Separate by outcomes
gad <- imp.combined %>%
  rename(Y = gad7score) %>%
  select(-phq8score) %>%
  distinct()

phq <- imp.combined %>%
  rename(Y = phq8score) %>%
  select(-gad7score) %>%
  distinct()

#Create censoring nodes
gad <- gad %>%
  mutate(C1 = as.factor(ifelse(is.na(A2), "censored", "uncensored")))

gad <- gad %>%
  mutate(C1 = recode(C1, "uncensored" = 1, "censored" = 0)) %>%
  add_column(C2 = NA) %>%
  mutate(C2 = C1) %>%
  mutate_at(c("C1","C2"), as.numeric) 

phq <- phq %>%
  mutate(C1 = as.factor(ifelse(is.na(A2), "censored", "uncensored")))

phq <- phq %>%
  mutate(C1 = recode(C1, "uncensored" = 1, "censored" = 0)) %>%
  add_column(C2 = NA) %>%
  mutate(C2 = C1) %>%
  mutate_at(c("C1","C2"), as.numeric)

#Reorder
gad <- gad %>%
  select(c(.imp,.id,commid,chsex,mightdie,careage,
           fatherill,motherill,wellbeing,hhsize,
           ownhouse,ownanimals,telugu,chhindu,bc,no_caste,sc,
           carenone,chprimary,chsecondary,chuniversity,L1_city,
           L1_pop,L1_sochel,A1,C1,
           L2_pop,L2_sochel,A2,C2,Y)) 

gad_girls <- gad %>%
  filter(chsex == 0) %>% 
  select(c(.imp,.id,commid,mightdie,careage,
           fatherill,motherill,wellbeing,hhsize,
           ownhouse,ownanimals,telugu,chhindu,bc,no_caste,sc,
           carenone,chprimary,chsecondary,chuniversity,L1_city,
           L1_pop,L1_sochel,A1,C1,
           L2_pop,L2_sochel,A2,C2,Y)) 

gad_boys <- gad %>%
  filter(chsex == 1) %>% 
  select(c(.imp,.id,commid,mightdie,careage,
           fatherill,motherill,wellbeing,hhsize,
           ownhouse,ownanimals,telugu,chhindu,bc,no_caste,sc,
           carenone,chprimary,chsecondary,chuniversity,L1_city,
           L1_pop,L1_sochel,A1,C1,
           L2_pop,L2_sochel,A2,C2,Y)) 

phq <- phq %>%
  select(c(.imp,.id,commid,chsex,mightdie,careage,
           fatherill,motherill,wellbeing,hhsize,
           ownhouse,ownanimals,telugu,chhindu,bc,no_caste,sc,
           carenone,chprimary,chsecondary,chuniversity,L1_city,
           L1_pop,L1_sochel,A1,C1,
           L2_pop,L2_sochel,A2,C2,Y)) 

phq_girls <- phq %>%
  filter(chsex == 0) %>% 
  select(c(.imp,.id,commid,mightdie,careage,
           fatherill,motherill,wellbeing,hhsize,
           ownhouse,ownanimals,telugu,chhindu,bc,no_caste,sc,
           carenone,chprimary,chsecondary,chuniversity,L1_city,
           L1_pop,L1_sochel,A1,C1,
           L2_pop,L2_sochel,A2,C2,Y)) 

phq_boys <- phq %>%
  filter(chsex == 1) %>% 
  select(c(.imp,.id,commid,mightdie,careage,
           fatherill,motherill,wellbeing,hhsize,
           ownhouse,ownanimals,telugu,chhindu,bc,no_caste,sc,
           carenone,chprimary,chsecondary,chuniversity,L1_city,
           L1_pop,L1_sochel,A1,C1,
           L2_pop,L2_sochel,A2,C2,Y)) 

#Convert back to imputed form
gad.full.imputed <- as.mids(gad)
phq.full.imputed <- as.mids(phq)

gad.full.imputed.girls <- as.mids(gad_girls)
phq.full.imputed.girls <- as.mids(phq_girls)

gad.full.imputed.boys <- as.mids(gad_boys)
phq.full.imputed.boys <- as.mids(phq_boys)
#head(gad.full.imputed$loggedEvents, 2) 

#Convert to list of five imputed dataframes
gad.originalstack <- complete(gad.full.imputed, "all")
phq.originalstack <- complete(phq.full.imputed, "all")

gad.originalstack.girls <- complete(gad.full.imputed.girls, "all")
phq.originalstack.girls <- complete(phq.full.imputed.girls, "all")

gad.originalstack.boys <- complete(gad.full.imputed.boys, "all")
phq.originalstack.boys <- complete(phq.full.imputed.boys, "all")

#Non-time varying covariates
W <- list(trt = c("ownhouse","ownanimals"), 
          cens = c("chsex","mightdie","careage","fatherill",
                   "motherill","wellbeing","hhsize","ownhouse","ownanimals",
                   "telugu","chhindu","bc","no_caste","sc",
                   "carenone","chprimary","chsecondary","chuniversity","L1_city"),
          outcome = c("chsex","mightdie","careage","fatherill",
                      "motherill","wellbeing","hhsize","ownhouse","ownanimals",
                      "telugu","chhindu","bc","no_caste","sc",
                      "carenone","chprimary","chsecondary","chuniversity","L1_city"))

W_gender <- list(trt = c("ownhouse","ownanimals"), 
                 cens = c("mightdie","careage","fatherill",
                          "motherill","wellbeing","hhsize","ownhouse","ownanimals",
                          "telugu","chhindu","bc","no_caste","sc",
                          "carenone","chprimary","chsecondary","chuniversity","L1_city"),
                 outcome = c("mightdie","careage","fatherill",
                             "motherill","wellbeing","hhsize","ownhouse","ownanimals",
                             "telugu","chhindu","bc","no_caste","sc",
                             "carenone","chprimary","chsecondary","chuniversity","L1_city"))
#Note: L1_city does not vary between A1 and A2. Coded as non-time varying.

#Time varying covariates
L <- list(trt = list(c(), 
                     c()), 
          cens = list(c("L1_pop","L1_sochel"), 
                      c("L2_pop","L2_sochel")), 
          outcome = list(c("L1_pop","L1_sochel"), 
                         c("L2_pop","L2_sochel")))

#Exposure nodes
A <- c("A1","A2") 

#Censored nodes
C <- c("C1","C2")

#Superlearner library
SL.library = c("SL.glm","SL.mean","SL.xgboost","SL.earth","SL.gam") 

#Create bounds for outcomes
boundgad <- as.numeric(c(0,21))
boundphq <- as.numeric(c(0,24))

#Create 0,1 and 1,0 shifts
shift_01 <- function(data,trt) {
  if(endsWith(trt,"1")) return(rep(0,nrow(data)))
  rep(1,nrow(data))
}

shift_10 <- function(data,trt) {
  if(endsWith(trt,"1")) return(rep(1,nrow(data)))
  rep(0,nrow(data))
}

#-------------------------------------------------------------------------------
#GAD - Imputation 1
set.seed(5555)

res.00 <- lmtp_sdr(gad.originalstack$`1`, A, "Y", baseline = W, time_vary = L, cens = NULL,
                   shift = static_binary_off, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundgad, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.11 <- lmtp_sdr(gad.originalstack$`1`, A, "Y", baseline = W, time_vary = L, cens = NULL,
                   shift = static_binary_on, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundgad, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.01 <- lmtp_sdr(gad.originalstack$`1`, A, "Y", baseline = W, time_vary = L, cens = NULL,
                   shift = shift_01, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundgad, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.10 <- lmtp_sdr(gad.originalstack$`1`, A, "Y", baseline = W, time_vary = L, cens = NULL,
                   shift = shift_10, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundgad, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.imp1.1 <- lmtp_contrast(res.00, ref = res.11)
res.imp1.2 <- lmtp_contrast(res.10, ref = res.11)
res.imp1.3 <- lmtp_contrast(res.01, ref = res.11)
#-------------------------------------------------------------------------------
#GAD - Imputation 2
set.seed(5555)

res.00 <- lmtp_sdr(gad.originalstack$`2`, A, "Y", baseline = W, time_vary = L, cens = NULL,
                   shift = static_binary_off, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundgad, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.11 <- lmtp_sdr(gad.originalstack$`2`, A, "Y", baseline = W, time_vary = L, cens = NULL,
                   shift = static_binary_on, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundgad, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.01 <- lmtp_sdr(gad.originalstack$`2`, A, "Y", baseline = W, time_vary = L, cens = NULL,
                   shift = shift_01, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundgad, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.10 <- lmtp_sdr(gad.originalstack$`2`, A, "Y", baseline = W, time_vary = L, cens = NULL,
                   shift = shift_10, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundgad, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.imp2.1 <- lmtp_contrast(res.00, ref = res.11)
res.imp2.2 <- lmtp_contrast(res.10, ref = res.11)
res.imp2.3 <- lmtp_contrast(res.01, ref = res.11)
#-------------------------------------------------------------------------------
#GAD - Imputation 3
set.seed(5555)

res.00 <- lmtp_sdr(gad.originalstack$`3`, A, "Y", baseline = W, time_vary = L, cens = NULL,
                   shift = static_binary_off, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundgad, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.11 <- lmtp_sdr(gad.originalstack$`3`, A, "Y", baseline = W, time_vary = L, cens = NULL,
                   shift = static_binary_on, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundgad, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.01 <- lmtp_sdr(gad.originalstack$`3`, A, "Y", baseline = W, time_vary = L, cens = NULL,
                   shift = shift_01, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundgad, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.10 <- lmtp_sdr(gad.originalstack$`3`, A, "Y", baseline = W, time_vary = L, cens = NULL,
                   shift = shift_10, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundgad, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.imp3.1 <- lmtp_contrast(res.00, ref = res.11)
res.imp3.2 <- lmtp_contrast(res.10, ref = res.11)
res.imp3.3 <- lmtp_contrast(res.01, ref = res.11)
#-------------------------------------------------------------------------------
#GAD - Imputation 4
set.seed(5555)

res.00 <- lmtp_sdr(gad.originalstack$`4`, A, "Y", baseline = W, time_vary = L, cens = NULL,
                   shift = static_binary_off, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundgad, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.11 <- lmtp_sdr(gad.originalstack$`4`, A, "Y", baseline = W, time_vary = L, cens = NULL,
                   shift = static_binary_on, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundgad, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.01 <- lmtp_sdr(gad.originalstack$`4`, A, "Y", baseline = W, time_vary = L, cens = NULL,
                   shift = shift_01, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundgad, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.10 <- lmtp_sdr(gad.originalstack$`4`, A, "Y", baseline = W, time_vary = L, cens = NULL,
                   shift = shift_10, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundgad, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.imp4.1 <- lmtp_contrast(res.00, ref = res.11)
res.imp4.2 <- lmtp_contrast(res.10, ref = res.11)
res.imp4.3 <- lmtp_contrast(res.01, ref = res.11)
#-------------------------------------------------------------------------------
#GAD - Imputation 5
set.seed(5555)

res.00 <- lmtp_sdr(gad.originalstack$`5`, A, "Y", baseline = W, time_vary = L, cens = NULL,
                   shift = static_binary_off, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundgad, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.11 <- lmtp_sdr(gad.originalstack$`5`, A, "Y", baseline = W, time_vary = L, cens = NULL,
                   shift = static_binary_on, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundgad, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.01 <- lmtp_sdr(gad.originalstack$`5`, A, "Y", baseline = W, time_vary = L, cens = NULL,
                   shift = shift_01, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundgad, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.10 <- lmtp_sdr(gad.originalstack$`5`, A, "Y", baseline = W, time_vary = L, cens = NULL,
                   shift = shift_10, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundgad, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.imp5.1 <- lmtp_contrast(res.00, ref = res.11)
res.imp5.2 <- lmtp_contrast(res.10, ref = res.11)
res.imp5.3 <- lmtp_contrast(res.01, ref = res.11)
#-------------------------------------------------------------------------------
#Combine - result 1
theta.bar <- (1/5)*(res.imp1.1$vals$theta + res.imp2.1$vals$theta + res.imp3.1$vals$theta + res.imp4.1$vals$theta + res.imp5.1$vals$theta)
var.W <- (1/5)*(res.imp1.1$vals$std.error^2 + res.imp2.1$vals$std.error^2 + res.imp3.1$vals$std.error^2 + res.imp4.1$vals$std.error^2 + res.imp5.1$vals$std.error^2)
var.B <- (1/4)*( (res.imp1.1$vals$theta - theta.bar)^2 
                 + (res.imp2.1$vals$theta - theta.bar)^2 
                 + (res.imp3.1$vals$theta - theta.bar)^2  
                 + (res.imp4.1$vals$theta - theta.bar)^2  
                 + (res.imp5.1$vals$theta - theta.bar)^2 )
var.tot <- var.W + var.B + var.B/5 ## Rubin's rule for variance
SE.tot <- sqrt(var.tot)

## CIs based on (theta.bar +/- 1.96*SE.tot)
CI_up <- theta.bar+1.96*SE.tot
CI_lo <- theta.bar-1.96*SE.tot

# p-values
zscore <- theta.bar/SE.tot
pvalue <- 2*pnorm(-abs(zscore))
#-------------------------------------------------------------------------------
#Combine - result 2
theta.bar <- (1/5)*(res.imp1.2$vals$theta + res.imp2.2$vals$theta + res.imp3.2$vals$theta + res.imp4.2$vals$theta + res.imp5.2$vals$theta)
var.W <- (1/5)*(res.imp1.2$vals$std.error^2 + res.imp2.2$vals$std.error^2 + res.imp3.2$vals$std.error^2 + res.imp4.2$vals$std.error^2 + res.imp5.2$vals$std.error^2)
var.B <- (1/4)*( (res.imp1.2$vals$theta - theta.bar)^2 
                 + (res.imp2.2$vals$theta - theta.bar)^2 
                 + (res.imp3.2$vals$theta - theta.bar)^2  
                 + (res.imp4.2$vals$theta - theta.bar)^2  
                 + (res.imp5.2$vals$theta - theta.bar)^2 )
var.tot <- var.W + var.B + var.B/5 ## Rubin's rule for variance
SE.tot <- sqrt(var.tot)

## CIs based on (theta.bar +/- 1.96*SE.tot)
CI_up <- theta.bar+1.96*SE.tot
CI_lo <- theta.bar-1.96*SE.tot

# p-values
zscore <- theta.bar/SE.tot
pvalue <- 2*pnorm(-abs(zscore))
#-------------------------------------------------------------------------------
#Combine - result 3
theta.bar <- (1/5)*(res.imp1.3$vals$theta + res.imp2.3$vals$theta + res.imp3.3$vals$theta + res.imp4.3$vals$theta + res.imp5.3$vals$theta)
var.W <- (1/5)*(res.imp1.3$vals$std.error^2 + res.imp2.3$vals$std.error^2 + res.imp3.3$vals$std.error^2 + res.imp4.3$vals$std.error^2 + res.imp5.3$vals$std.error^2)
var.B <- (1/4)*( (res.imp1.3$vals$theta - theta.bar)^2 
                 + (res.imp2.3$vals$theta - theta.bar)^2 
                 + (res.imp3.3$vals$theta - theta.bar)^2  
                 + (res.imp4.3$vals$theta - theta.bar)^2  
                 + (res.imp5.3$vals$theta - theta.bar)^2 )
var.tot <- var.W + var.B + var.B/5 ## Rubin's rule for variance
SE.tot <- sqrt(var.tot)

## CIs based on (theta.bar +/- 1.96*SE.tot)
CI_up <- theta.bar+1.96*SE.tot
CI_lo <- theta.bar-1.96*SE.tot

# p-values
zscore <- theta.bar/SE.tot
pvalue <- 2*pnorm(-abs(zscore))

#-------------------------------------------------------------------------------
#PHQ - Imputation 1
set.seed(5555)

res.00 <- lmtp_sdr(phq.originalstack$`1`, A, "Y", baseline = W, time_vary = L, cens = NULL,
                   shift = static_binary_off, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundphq, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.11 <- lmtp_sdr(phq.originalstack$`1`, A, "Y", baseline = W, time_vary = L, cens = NULL,
                   shift = static_binary_on, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundphq, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.01 <- lmtp_sdr(phq.originalstack$`1`, A, "Y", baseline = W, time_vary = L, cens = NULL,
                   shift = shift_01, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundphq, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.10 <- lmtp_sdr(phq.originalstack$`1`, A, "Y", baseline = W, time_vary = L, cens = NULL,
                   shift = shift_10, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundphq, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.imp1.1 <- lmtp_contrast(res.00, ref = res.11)
res.imp1.2 <- lmtp_contrast(res.10, ref = res.11)
res.imp1.3 <- lmtp_contrast(res.01, ref = res.11)
#-------------------------------------------------------------------------------
#PHQ - Imputation 2
set.seed(5555)

res.00 <- lmtp_sdr(phq.originalstack$`2`, A, "Y", baseline = W, time_vary = L, cens = NULL,
                   shift = static_binary_off, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundphq, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.11 <- lmtp_sdr(phq.originalstack$`2`, A, "Y", baseline = W, time_vary = L, cens = NULL,
                   shift = static_binary_on, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundphq, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.01 <- lmtp_sdr(phq.originalstack$`2`, A, "Y", baseline = W, time_vary = L, cens = NULL,
                   shift = shift_01, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundphq, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.10 <- lmtp_sdr(phq.originalstack$`2`, A, "Y", baseline = W, time_vary = L, cens = NULL,
                   shift = shift_10, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundphq, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.imp2.1 <- lmtp_contrast(res.00, ref = res.11)
res.imp2.2 <- lmtp_contrast(res.10, ref = res.11)
res.imp2.3 <- lmtp_contrast(res.01, ref = res.11)
#-------------------------------------------------------------------------------
#PHQ - Imputation 3
set.seed(5555)

res.00 <- lmtp_sdr(phq.originalstack$`3`, A, "Y", baseline = W, time_vary = L, cens = NULL,
                   shift = static_binary_off, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundphq, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.11 <- lmtp_sdr(phq.originalstack$`3`, A, "Y", baseline = W, time_vary = L, cens = NULL,
                   shift = static_binary_on, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundphq, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.01 <- lmtp_sdr(phq.originalstack$`3`, A, "Y", baseline = W, time_vary = L, cens = NULL,
                   shift = shift_01, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundphq, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.10 <- lmtp_sdr(phq.originalstack$`3`, A, "Y", baseline = W, time_vary = L, cens = NULL,
                   shift = shift_10, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundphq, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.imp3.1 <- lmtp_contrast(res.00, ref = res.11)
res.imp3.2 <- lmtp_contrast(res.10, ref = res.11)
res.imp3.3 <- lmtp_contrast(res.01, ref = res.11)
#-------------------------------------------------------------------------------
#PHQ - Imputation 4
set.seed(5555)

res.00 <- lmtp_sdr(phq.originalstack$`4`, A, "Y", baseline = W, time_vary = L, cens = NULL,
                   shift = static_binary_off, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundphq, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.11 <- lmtp_sdr(phq.originalstack$`4`, A, "Y", baseline = W, time_vary = L, cens = NULL,
                   shift = static_binary_on, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundphq, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.01 <- lmtp_sdr(phq.originalstack$`4`, A, "Y", baseline = W, time_vary = L, cens = NULL,
                   shift = shift_01, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundphq, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.10 <- lmtp_sdr(phq.originalstack$`4`, A, "Y", baseline = W, time_vary = L, cens = NULL,
                   shift = shift_10, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundphq, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.imp4.1 <- lmtp_contrast(res.00, ref = res.11)
res.imp4.2 <- lmtp_contrast(res.10, ref = res.11)
res.imp4.3 <- lmtp_contrast(res.01, ref = res.11)
#-------------------------------------------------------------------------------
#PHQ - Imputation 5
set.seed(5555)

res.00 <- lmtp_sdr(phq.originalstack$`5`, A, "Y", baseline = W, time_vary = L, cens = NULL,
                   shift = static_binary_off, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundphq, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.11 <- lmtp_sdr(phq.originalstack$`5`, A, "Y", baseline = W, time_vary = L, cens = NULL,
                   shift = static_binary_on, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundphq, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.01 <- lmtp_sdr(phq.originalstack$`5`, A, "Y", baseline = W, time_vary = L, cens = NULL,
                   shift = shift_01, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundphq, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.10 <- lmtp_sdr(phq.originalstack$`5`, A, "Y", baseline = W, time_vary = L, cens = NULL,
                   shift = shift_10, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundphq, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.imp5.1 <- lmtp_contrast(res.00, ref = res.11)
res.imp5.2 <- lmtp_contrast(res.10, ref = res.11)
res.imp5.3 <- lmtp_contrast(res.01, ref = res.11)
#-------------------------------------------------------------------------------
#Combine - result 1
theta.bar <- (1/5)*(res.imp1.1$vals$theta + res.imp2.1$vals$theta + res.imp3.1$vals$theta + res.imp4.1$vals$theta + res.imp5.1$vals$theta)
var.W <- (1/5)*(res.imp1.1$vals$std.error^2 + res.imp2.1$vals$std.error^2 + res.imp3.1$vals$std.error^2 + res.imp4.1$vals$std.error^2 + res.imp5.1$vals$std.error^2)
var.B <- (1/4)*( (res.imp1.1$vals$theta - theta.bar)^2 
                 + (res.imp2.1$vals$theta - theta.bar)^2 
                 + (res.imp3.1$vals$theta - theta.bar)^2  
                 + (res.imp4.1$vals$theta - theta.bar)^2  
                 + (res.imp5.1$vals$theta - theta.bar)^2 )
var.tot <- var.W + var.B + var.B/5 ## Rubin's rule for variance
SE.tot <- sqrt(var.tot)

## CIs based on (theta.bar +/- 1.96*SE.tot)
CI_up <- theta.bar+1.96*SE.tot
CI_lo <- theta.bar-1.96*SE.tot

# p-values
zscore <- theta.bar/SE.tot
pvalue <- 2*pnorm(-abs(zscore))
#-------------------------------------------------------------------------------
#Combine - result 2
theta.bar <- (1/5)*(res.imp1.2$vals$theta + res.imp2.2$vals$theta + res.imp3.2$vals$theta + res.imp4.2$vals$theta + res.imp5.2$vals$theta)
var.W <- (1/5)*(res.imp1.2$vals$std.error^2 + res.imp2.2$vals$std.error^2 + res.imp3.2$vals$std.error^2 + res.imp4.2$vals$std.error^2 + res.imp5.2$vals$std.error^2)
var.B <- (1/4)*( (res.imp1.2$vals$theta - theta.bar)^2 
                 + (res.imp2.2$vals$theta - theta.bar)^2 
                 + (res.imp3.2$vals$theta - theta.bar)^2  
                 + (res.imp4.2$vals$theta - theta.bar)^2  
                 + (res.imp5.2$vals$theta - theta.bar)^2 )
var.tot <- var.W + var.B + var.B/5 ## Rubin's rule for variance
SE.tot <- sqrt(var.tot)

## CIs based on (theta.bar +/- 1.96*SE.tot)
CI_up <- theta.bar+1.96*SE.tot
CI_lo <- theta.bar-1.96*SE.tot

# p-values
zscore <- theta.bar/SE.tot
pvalue <- 2*pnorm(-abs(zscore))
#-------------------------------------------------------------------------------
#Combine - result 3
theta.bar <- (1/5)*(res.imp1.3$vals$theta + res.imp2.3$vals$theta + res.imp3.3$vals$theta + res.imp4.3$vals$theta + res.imp5.3$vals$theta)
var.W <- (1/5)*(res.imp1.3$vals$std.error^2 + res.imp2.3$vals$std.error^2 + res.imp3.3$vals$std.error^2 + res.imp4.3$vals$std.error^2 + res.imp5.3$vals$std.error^2)
var.B <- (1/4)*( (res.imp1.3$vals$theta - theta.bar)^2 
                 + (res.imp2.3$vals$theta - theta.bar)^2 
                 + (res.imp3.3$vals$theta - theta.bar)^2  
                 + (res.imp4.3$vals$theta - theta.bar)^2  
                 + (res.imp5.3$vals$theta - theta.bar)^2 )
var.tot <- var.W + var.B + var.B/5 ## Rubin's rule for variance
SE.tot <- sqrt(var.tot)

## CIs based on (theta.bar +/- 1.96*SE.tot)
CI_up <- theta.bar+1.96*SE.tot
CI_lo <- theta.bar-1.96*SE.tot

# p-values
zscore <- theta.bar/SE.tot
pvalue <- 2*pnorm(-abs(zscore))

#-------------------------------------------------------------------------------
#GAD girls - Imputation 1
set.seed(5555)

res.00 <- lmtp_sdr(gad.originalstack.girls$`1`, A, "Y", baseline = W_gender, time_vary = L, cens = NULL,
                   shift = static_binary_off, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundgad, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.11 <- lmtp_sdr(gad.originalstack.girls$`1`, A, "Y", baseline = W_gender, time_vary = L, cens = NULL,
                   shift = static_binary_on, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundgad, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.01 <- lmtp_sdr(gad.originalstack.girls$`1`, A, "Y", baseline = W_gender, time_vary = L, cens = NULL,
                   shift = shift_01, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundgad, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.10 <- lmtp_sdr(gad.originalstack.girls$`1`, A, "Y", baseline = W_gender, time_vary = L, cens = NULL,
                   shift = shift_10, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundgad, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.imp1.1 <- lmtp_contrast(res.00, ref = res.11)
res.imp1.2 <- lmtp_contrast(res.10, ref = res.11)
res.imp1.3 <- lmtp_contrast(res.01, ref = res.11)
#-------------------------------------------------------------------------------
#GAD - Imputation 2
set.seed(5555)

res.00 <- lmtp_sdr(gad.originalstack.girls$`2`, A, "Y", baseline = W_gender, time_vary = L, cens = NULL,
                   shift = static_binary_off, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundgad, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.11 <- lmtp_sdr(gad.originalstack.girls$`2`, A, "Y", baseline = W_gender, time_vary = L, cens = NULL,
                   shift = static_binary_on, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundgad, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.01 <- lmtp_sdr(gad.originalstack.girls$`2`, A, "Y", baseline = W_gender, time_vary = L, cens = NULL,
                   shift = shift_01, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundgad, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.10 <- lmtp_sdr(gad.originalstack.girls$`2`, A, "Y", baseline = W_gender, time_vary = L, cens = NULL,
                   shift = shift_10, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundgad, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.imp2.1 <- lmtp_contrast(res.00, ref = res.11)
res.imp2.2 <- lmtp_contrast(res.10, ref = res.11)
res.imp2.3 <- lmtp_contrast(res.01, ref = res.11)
#-------------------------------------------------------------------------------
#GAD - Imputation 3
set.seed(5555)

res.00 <- lmtp_sdr(gad.originalstack.girls$`3`, A, "Y", baseline = W_gender, time_vary = L, cens = NULL,
                   shift = static_binary_off, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundgad, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.11 <- lmtp_sdr(gad.originalstack.girls$`3`, A, "Y", baseline = W_gender, time_vary = L, cens = NULL,
                   shift = static_binary_on, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundgad, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.01 <- lmtp_sdr(gad.originalstack.girls$`3`, A, "Y", baseline = W_gender, time_vary = L, cens = NULL,
                   shift = shift_01, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundgad, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.10 <- lmtp_sdr(gad.originalstack.girls$`3`, A, "Y", baseline = W_gender, time_vary = L, cens = NULL,
                   shift = shift_10, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundgad, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.imp3.1 <- lmtp_contrast(res.00, ref = res.11)
res.imp3.2 <- lmtp_contrast(res.10, ref = res.11)
res.imp3.3 <- lmtp_contrast(res.01, ref = res.11)
#-------------------------------------------------------------------------------
#GAD - Imputation 4
set.seed(5555)

res.00 <- lmtp_sdr(gad.originalstack.girls$`4`, A, "Y", baseline = W_gender, time_vary = L, cens = NULL,
                   shift = static_binary_off, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundgad, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.11 <- lmtp_sdr(gad.originalstack.girls$`4`, A, "Y", baseline = W_gender, time_vary = L, cens = NULL,
                   shift = static_binary_on, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundgad, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.01 <- lmtp_sdr(gad.originalstack.girls$`4`, A, "Y", baseline = W_gender, time_vary = L, cens = NULL,
                   shift = shift_01, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundgad, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.10 <- lmtp_sdr(gad.originalstack.girls$`4`, A, "Y", baseline = W_gender, time_vary = L, cens = NULL,
                   shift = shift_10, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundgad, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.imp4.1 <- lmtp_contrast(res.00, ref = res.11)
res.imp4.2 <- lmtp_contrast(res.10, ref = res.11)
res.imp4.3 <- lmtp_contrast(res.01, ref = res.11)
#-------------------------------------------------------------------------------
#GAD - Imputation 5
set.seed(5555)

res.00 <- lmtp_sdr(gad.originalstack.girls$`5`, A, "Y", baseline = W_gender, time_vary = L, cens = NULL,
                   shift = static_binary_off, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundgad, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.11 <- lmtp_sdr(gad.originalstack.girls$`5`, A, "Y", baseline = W_gender, time_vary = L, cens = NULL,
                   shift = static_binary_on, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundgad, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.01 <- lmtp_sdr(gad.originalstack.girls$`5`, A, "Y", baseline = W_gender, time_vary = L, cens = NULL,
                   shift = shift_01, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundgad, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.10 <- lmtp_sdr(gad.originalstack.girls$`5`, A, "Y", baseline = W_gender, time_vary = L, cens = NULL,
                   shift = shift_10, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundgad, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.imp5.1 <- lmtp_contrast(res.00, ref = res.11)
res.imp5.2 <- lmtp_contrast(res.10, ref = res.11)
res.imp5.3 <- lmtp_contrast(res.01, ref = res.11)
#-------------------------------------------------------------------------------
#Combine - result 1
theta.bar <- (1/5)*(res.imp1.1$vals$theta + res.imp2.1$vals$theta + res.imp3.1$vals$theta + res.imp4.1$vals$theta + res.imp5.1$vals$theta)
var.W <- (1/5)*(res.imp1.1$vals$std.error^2 + res.imp2.1$vals$std.error^2 + res.imp3.1$vals$std.error^2 + res.imp4.1$vals$std.error^2 + res.imp5.1$vals$std.error^2)
var.B <- (1/4)*( (res.imp1.1$vals$theta - theta.bar)^2 
                 + (res.imp2.1$vals$theta - theta.bar)^2 
                 + (res.imp3.1$vals$theta - theta.bar)^2  
                 + (res.imp4.1$vals$theta - theta.bar)^2  
                 + (res.imp5.1$vals$theta - theta.bar)^2 )
var.tot <- var.W + var.B + var.B/5 ## Rubin's rule for variance
SE.tot <- sqrt(var.tot)

## CIs based on (theta.bar +/- 1.96*SE.tot)
CI_up <- theta.bar+1.96*SE.tot
CI_lo <- theta.bar-1.96*SE.tot

# p-values
zscore <- theta.bar/SE.tot
pvalue <- 2*pnorm(-abs(zscore))
#-------------------------------------------------------------------------------
#Combine - result 2
theta.bar <- (1/5)*(res.imp1.2$vals$theta + res.imp2.2$vals$theta + res.imp3.2$vals$theta + res.imp4.2$vals$theta + res.imp5.2$vals$theta)
var.W <- (1/5)*(res.imp1.2$vals$std.error^2 + res.imp2.2$vals$std.error^2 + res.imp3.2$vals$std.error^2 + res.imp4.2$vals$std.error^2 + res.imp5.2$vals$std.error^2)
var.B <- (1/4)*( (res.imp1.2$vals$theta - theta.bar)^2 
                 + (res.imp2.2$vals$theta - theta.bar)^2 
                 + (res.imp3.2$vals$theta - theta.bar)^2  
                 + (res.imp4.2$vals$theta - theta.bar)^2  
                 + (res.imp5.2$vals$theta - theta.bar)^2 )
var.tot <- var.W + var.B + var.B/5 ## Rubin's rule for variance
SE.tot <- sqrt(var.tot)

## CIs based on (theta.bar +/- 1.96*SE.tot)
CI_up <- theta.bar+1.96*SE.tot
CI_lo <- theta.bar-1.96*SE.tot

# p-values
zscore <- theta.bar/SE.tot
pvalue <- 2*pnorm(-abs(zscore))
#-------------------------------------------------------------------------------
#Combine - result 3
theta.bar <- (1/5)*(res.imp1.3$vals$theta + res.imp2.3$vals$theta + res.imp3.3$vals$theta + res.imp4.3$vals$theta + res.imp5.3$vals$theta)
var.W <- (1/5)*(res.imp1.3$vals$std.error^2 + res.imp2.3$vals$std.error^2 + res.imp3.3$vals$std.error^2 + res.imp4.3$vals$std.error^2 + res.imp5.3$vals$std.error^2)
var.B <- (1/4)*( (res.imp1.3$vals$theta - theta.bar)^2 
                 + (res.imp2.3$vals$theta - theta.bar)^2 
                 + (res.imp3.3$vals$theta - theta.bar)^2  
                 + (res.imp4.3$vals$theta - theta.bar)^2  
                 + (res.imp5.3$vals$theta - theta.bar)^2 )
var.tot <- var.W + var.B + var.B/5 ## Rubin's rule for variance
SE.tot <- sqrt(var.tot)

## CIs based on (theta.bar +/- 1.96*SE.tot)
CI_up <- theta.bar+1.96*SE.tot
CI_lo <- theta.bar-1.96*SE.tot

# p-values
zscore <- theta.bar/SE.tot
pvalue <- 2*pnorm(-abs(zscore))

#-------------------------------------------------------------------------------
#GAD boys - Imputation 1
set.seed(5555)

res.00 <- lmtp_sdr(gad.originalstack.boys$`1`, A, "Y", baseline = W_gender, time_vary = L, cens = NULL,
                   shift = static_binary_off, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundgad, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.11 <- lmtp_sdr(gad.originalstack.boys$`1`, A, "Y", baseline = W_gender, time_vary = L, cens = NULL,
                   shift = static_binary_on, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundgad, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.01 <- lmtp_sdr(gad.originalstack.boys$`1`, A, "Y", baseline = W_gender, time_vary = L, cens = NULL,
                   shift = shift_01, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundgad, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.10 <- lmtp_sdr(gad.originalstack.boys$`1`, A, "Y", baseline = W_gender, time_vary = L, cens = NULL,
                   shift = shift_10, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundgad, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.imp1.1 <- lmtp_contrast(res.00, ref = res.11)
res.imp1.2 <- lmtp_contrast(res.10, ref = res.11)
res.imp1.3 <- lmtp_contrast(res.01, ref = res.11)
#-------------------------------------------------------------------------------
#GAD - Imputation 2
set.seed(5555)

res.00 <- lmtp_sdr(gad.originalstack.boys$`2`, A, "Y", baseline = W_gender, time_vary = L, cens = NULL,
                   shift = static_binary_off, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundgad, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.11 <- lmtp_sdr(gad.originalstack.boys$`2`, A, "Y", baseline = W_gender, time_vary = L, cens = NULL,
                   shift = static_binary_on, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundgad, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.01 <- lmtp_sdr(gad.originalstack.boys$`2`, A, "Y", baseline = W_gender, time_vary = L, cens = NULL,
                   shift = shift_01, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundgad, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.10 <- lmtp_sdr(gad.originalstack.boys$`2`, A, "Y", baseline = W_gender, time_vary = L, cens = NULL,
                   shift = shift_10, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundgad, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.imp2.1 <- lmtp_contrast(res.00, ref = res.11)
res.imp2.2 <- lmtp_contrast(res.10, ref = res.11)
res.imp2.3 <- lmtp_contrast(res.01, ref = res.11)
#-------------------------------------------------------------------------------
#GAD - Imputation 3
set.seed(5555)

res.00 <- lmtp_sdr(gad.originalstack.boys$`3`, A, "Y", baseline = W_gender, time_vary = L, cens = NULL,
                   shift = static_binary_off, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundgad, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.11 <- lmtp_sdr(gad.originalstack.boys$`3`, A, "Y", baseline = W_gender, time_vary = L, cens = NULL,
                   shift = static_binary_on, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundgad, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.01 <- lmtp_sdr(gad.originalstack.boys$`3`, A, "Y", baseline = W_gender, time_vary = L, cens = NULL,
                   shift = shift_01, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundgad, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.10 <- lmtp_sdr(gad.originalstack.boys$`3`, A, "Y", baseline = W_gender, time_vary = L, cens = NULL,
                   shift = shift_10, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundgad, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.imp3.1 <- lmtp_contrast(res.00, ref = res.11)
res.imp3.2 <- lmtp_contrast(res.10, ref = res.11)
res.imp3.3 <- lmtp_contrast(res.01, ref = res.11)
#-------------------------------------------------------------------------------
#GAD - Imputation 4
set.seed(5555)

res.00 <- lmtp_sdr(gad.originalstack.boys$`4`, A, "Y", baseline = W_gender, time_vary = L, cens = NULL,
                   shift = static_binary_off, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundgad, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.11 <- lmtp_sdr(gad.originalstack.boys$`4`, A, "Y", baseline = W_gender, time_vary = L, cens = NULL,
                   shift = static_binary_on, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundgad, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.01 <- lmtp_sdr(gad.originalstack.boys$`4`, A, "Y", baseline = W_gender, time_vary = L, cens = NULL,
                   shift = shift_01, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundgad, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.10 <- lmtp_sdr(gad.originalstack.boys$`4`, A, "Y", baseline = W_gender, time_vary = L, cens = NULL,
                   shift = shift_10, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundgad, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.imp4.1 <- lmtp_contrast(res.00, ref = res.11)
res.imp4.2 <- lmtp_contrast(res.10, ref = res.11)
res.imp4.3 <- lmtp_contrast(res.01, ref = res.11)
#-------------------------------------------------------------------------------
#GAD - Imputation 5
set.seed(5555)

res.00 <- lmtp_sdr(gad.originalstack.boys$`5`, A, "Y", baseline = W_gender, time_vary = L, cens = NULL,
                   shift = static_binary_off, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundgad, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.11 <- lmtp_sdr(gad.originalstack.boys$`5`, A, "Y", baseline = W_gender, time_vary = L, cens = NULL,
                   shift = static_binary_on, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundgad, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.01 <- lmtp_sdr(gad.originalstack.boys$`5`, A, "Y", baseline = W_gender, time_vary = L, cens = NULL,
                   shift = shift_01, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundgad, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.10 <- lmtp_sdr(gad.originalstack.boys$`5`, A, "Y", baseline = W_gender, time_vary = L, cens = NULL,
                   shift = shift_10, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundgad, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.imp5.1 <- lmtp_contrast(res.00, ref = res.11)
res.imp5.2 <- lmtp_contrast(res.10, ref = res.11)
res.imp5.3 <- lmtp_contrast(res.01, ref = res.11)
#-------------------------------------------------------------------------------
#Combine - result 1
theta.bar <- (1/5)*(res.imp1.1$vals$theta + res.imp2.1$vals$theta + res.imp3.1$vals$theta + res.imp4.1$vals$theta + res.imp5.1$vals$theta)
var.W <- (1/5)*(res.imp1.1$vals$std.error^2 + res.imp2.1$vals$std.error^2 + res.imp3.1$vals$std.error^2 + res.imp4.1$vals$std.error^2 + res.imp5.1$vals$std.error^2)
var.B <- (1/4)*( (res.imp1.1$vals$theta - theta.bar)^2 
                 + (res.imp2.1$vals$theta - theta.bar)^2 
                 + (res.imp3.1$vals$theta - theta.bar)^2  
                 + (res.imp4.1$vals$theta - theta.bar)^2  
                 + (res.imp5.1$vals$theta - theta.bar)^2 )
var.tot <- var.W + var.B + var.B/5 ## Rubin's rule for variance
SE.tot <- sqrt(var.tot)

## CIs based on (theta.bar +/- 1.96*SE.tot)
CI_up <- theta.bar+1.96*SE.tot
CI_lo <- theta.bar-1.96*SE.tot

# p-values
zscore <- theta.bar/SE.tot
pvalue <- 2*pnorm(-abs(zscore))
#-------------------------------------------------------------------------------
#Combine - result 2
theta.bar <- (1/5)*(res.imp1.2$vals$theta + res.imp2.2$vals$theta + res.imp3.2$vals$theta + res.imp4.2$vals$theta + res.imp5.2$vals$theta)
var.W <- (1/5)*(res.imp1.2$vals$std.error^2 + res.imp2.2$vals$std.error^2 + res.imp3.2$vals$std.error^2 + res.imp4.2$vals$std.error^2 + res.imp5.2$vals$std.error^2)
var.B <- (1/4)*( (res.imp1.2$vals$theta - theta.bar)^2 
                 + (res.imp2.2$vals$theta - theta.bar)^2 
                 + (res.imp3.2$vals$theta - theta.bar)^2  
                 + (res.imp4.2$vals$theta - theta.bar)^2  
                 + (res.imp5.2$vals$theta - theta.bar)^2 )
var.tot <- var.W + var.B + var.B/5 ## Rubin's rule for variance
SE.tot <- sqrt(var.tot)

## CIs based on (theta.bar +/- 1.96*SE.tot)
CI_up <- theta.bar+1.96*SE.tot
CI_lo <- theta.bar-1.96*SE.tot

# p-values
zscore <- theta.bar/SE.tot
pvalue <- 2*pnorm(-abs(zscore))
#-------------------------------------------------------------------------------
#Combine - result 3
theta.bar <- (1/5)*(res.imp1.3$vals$theta + res.imp2.3$vals$theta + res.imp3.3$vals$theta + res.imp4.3$vals$theta + res.imp5.3$vals$theta)
var.W <- (1/5)*(res.imp1.3$vals$std.error^2 + res.imp2.3$vals$std.error^2 + res.imp3.3$vals$std.error^2 + res.imp4.3$vals$std.error^2 + res.imp5.3$vals$std.error^2)
var.B <- (1/4)*( (res.imp1.3$vals$theta - theta.bar)^2 
                 + (res.imp2.3$vals$theta - theta.bar)^2 
                 + (res.imp3.3$vals$theta - theta.bar)^2  
                 + (res.imp4.3$vals$theta - theta.bar)^2  
                 + (res.imp5.3$vals$theta - theta.bar)^2 )
var.tot <- var.W + var.B + var.B/5 ## Rubin's rule for variance
SE.tot <- sqrt(var.tot)

## CIs based on (theta.bar +/- 1.96*SE.tot)
CI_up <- theta.bar+1.96*SE.tot
CI_lo <- theta.bar-1.96*SE.tot

# p-values
zscore <- theta.bar/SE.tot
pvalue <- 2*pnorm(-abs(zscore))

#-------------------------------------------------------------------------------
#PHQ girls - Imputation 1
set.seed(5555)

res.00 <- lmtp_sdr(phq.originalstack.girls$`1`, A, "Y", baseline = W_gender, time_vary = L, cens = NULL,
                   shift = static_binary_off, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundphq, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.11 <- lmtp_sdr(phq.originalstack.girls$`1`, A, "Y", baseline = W_gender, time_vary = L, cens = NULL,
                   shift = static_binary_on, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundphq, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.01 <- lmtp_sdr(phq.originalstack.girls$`1`, A, "Y", baseline = W_gender, time_vary = L, cens = NULL,
                   shift = shift_01, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundphq, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.10 <- lmtp_sdr(phq.originalstack.girls$`1`, A, "Y", baseline = W_gender, time_vary = L, cens = NULL,
                   shift = shift_10, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundphq, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.imp1.1 <- lmtp_contrast(res.00, ref = res.11)
res.imp1.2 <- lmtp_contrast(res.10, ref = res.11)
res.imp1.3 <- lmtp_contrast(res.01, ref = res.11)
#-------------------------------------------------------------------------------
#PHQ - Imputation 2
set.seed(5555)

res.00 <- lmtp_sdr(phq.originalstack.girls$`2`, A, "Y", baseline = W_gender, time_vary = L, cens = NULL,
                   shift = static_binary_off, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundphq, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.11 <- lmtp_sdr(phq.originalstack.girls$`2`, A, "Y", baseline = W_gender, time_vary = L, cens = NULL,
                   shift = static_binary_on, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundphq, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.01 <- lmtp_sdr(phq.originalstack.girls$`2`, A, "Y", baseline = W_gender, time_vary = L, cens = NULL,
                   shift = shift_01, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundphq, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.10 <- lmtp_sdr(phq.originalstack.girls$`2`, A, "Y", baseline = W_gender, time_vary = L, cens = NULL,
                   shift = shift_10, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundphq, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.imp2.1 <- lmtp_contrast(res.00, ref = res.11)
res.imp2.2 <- lmtp_contrast(res.10, ref = res.11)
res.imp2.3 <- lmtp_contrast(res.01, ref = res.11)
#-------------------------------------------------------------------------------
#PHQ - Imputation 3
set.seed(5555)

res.00 <- lmtp_sdr(phq.originalstack.girls$`3`, A, "Y", baseline = W_gender, time_vary = L, cens = NULL,
                   shift = static_binary_off, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundphq, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.11 <- lmtp_sdr(phq.originalstack.girls$`3`, A, "Y", baseline = W_gender, time_vary = L, cens = NULL,
                   shift = static_binary_on, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundphq, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.01 <- lmtp_sdr(phq.originalstack.girls$`3`, A, "Y", baseline = W_gender, time_vary = L, cens = NULL,
                   shift = shift_01, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundphq, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.10 <- lmtp_sdr(phq.originalstack.girls$`3`, A, "Y", baseline = W_gender, time_vary = L, cens = NULL,
                   shift = shift_10, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundphq, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.imp3.1 <- lmtp_contrast(res.00, ref = res.11)
res.imp3.2 <- lmtp_contrast(res.10, ref = res.11)
res.imp3.3 <- lmtp_contrast(res.01, ref = res.11)
#-------------------------------------------------------------------------------
#PHQ - Imputation 4
set.seed(5555)

res.00 <- lmtp_sdr(phq.originalstack.girls$`4`, A, "Y", baseline = W_gender, time_vary = L, cens = NULL,
                   shift = static_binary_off, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundphq, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.11 <- lmtp_sdr(phq.originalstack.girls$`4`, A, "Y", baseline = W_gender, time_vary = L, cens = NULL,
                   shift = static_binary_on, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundphq, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.01 <- lmtp_sdr(phq.originalstack.girls$`4`, A, "Y", baseline = W_gender, time_vary = L, cens = NULL,
                   shift = shift_01, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundphq, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.10 <- lmtp_sdr(phq.originalstack.girls$`4`, A, "Y", baseline = W_gender, time_vary = L, cens = NULL,
                   shift = shift_10, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundphq, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.imp4.1 <- lmtp_contrast(res.00, ref = res.11)
res.imp4.2 <- lmtp_contrast(res.10, ref = res.11)
res.imp4.3 <- lmtp_contrast(res.01, ref = res.11)
#-------------------------------------------------------------------------------
#PHQ - Imputation 5
set.seed(5555)

res.00 <- lmtp_sdr(phq.originalstack.girls$`5`, A, "Y", baseline = W_gender, time_vary = L, cens = NULL,
                   shift = static_binary_off, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundphq, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.11 <- lmtp_sdr(phq.originalstack.girls$`5`, A, "Y", baseline = W_gender, time_vary = L, cens = NULL,
                   shift = static_binary_on, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundphq, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.01 <- lmtp_sdr(phq.originalstack.girls$`5`, A, "Y", baseline = W_gender, time_vary = L, cens = NULL,
                   shift = shift_01, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundphq, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.10 <- lmtp_sdr(phq.originalstack.girls$`5`, A, "Y", baseline = W_gender, time_vary = L, cens = NULL,
                   shift = shift_10, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundphq, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.imp5.1 <- lmtp_contrast(res.00, ref = res.11)
res.imp5.2 <- lmtp_contrast(res.10, ref = res.11)
res.imp5.3 <- lmtp_contrast(res.01, ref = res.11)
#-------------------------------------------------------------------------------
#Combine - result 1
theta.bar <- (1/5)*(res.imp1.1$vals$theta + res.imp2.1$vals$theta + res.imp3.1$vals$theta + res.imp4.1$vals$theta + res.imp5.1$vals$theta)
var.W <- (1/5)*(res.imp1.1$vals$std.error^2 + res.imp2.1$vals$std.error^2 + res.imp3.1$vals$std.error^2 + res.imp4.1$vals$std.error^2 + res.imp5.1$vals$std.error^2)
var.B <- (1/4)*( (res.imp1.1$vals$theta - theta.bar)^2 
                 + (res.imp2.1$vals$theta - theta.bar)^2 
                 + (res.imp3.1$vals$theta - theta.bar)^2  
                 + (res.imp4.1$vals$theta - theta.bar)^2  
                 + (res.imp5.1$vals$theta - theta.bar)^2 )
var.tot <- var.W + var.B + var.B/5 ## Rubin's rule for variance
SE.tot <- sqrt(var.tot)

## CIs based on (theta.bar +/- 1.96*SE.tot)
CI_up <- theta.bar+1.96*SE.tot
CI_lo <- theta.bar-1.96*SE.tot

# p-values
zscore <- theta.bar/SE.tot
pvalue <- 2*pnorm(-abs(zscore))
#-------------------------------------------------------------------------------
#Combine - result 2
theta.bar <- (1/5)*(res.imp1.2$vals$theta + res.imp2.2$vals$theta + res.imp3.2$vals$theta + res.imp4.2$vals$theta + res.imp5.2$vals$theta)
var.W <- (1/5)*(res.imp1.2$vals$std.error^2 + res.imp2.2$vals$std.error^2 + res.imp3.2$vals$std.error^2 + res.imp4.2$vals$std.error^2 + res.imp5.2$vals$std.error^2)
var.B <- (1/4)*( (res.imp1.2$vals$theta - theta.bar)^2 
                 + (res.imp2.2$vals$theta - theta.bar)^2 
                 + (res.imp3.2$vals$theta - theta.bar)^2  
                 + (res.imp4.2$vals$theta - theta.bar)^2  
                 + (res.imp5.2$vals$theta - theta.bar)^2 )
var.tot <- var.W + var.B + var.B/5 ## Rubin's rule for variance
SE.tot <- sqrt(var.tot)

## CIs based on (theta.bar +/- 1.96*SE.tot)
CI_up <- theta.bar+1.96*SE.tot
CI_lo <- theta.bar-1.96*SE.tot

# p-values
zscore <- theta.bar/SE.tot
pvalue <- 2*pnorm(-abs(zscore))
#-------------------------------------------------------------------------------
#Combine - result 3
theta.bar <- (1/5)*(res.imp1.3$vals$theta + res.imp2.3$vals$theta + res.imp3.3$vals$theta + res.imp4.3$vals$theta + res.imp5.3$vals$theta)
var.W <- (1/5)*(res.imp1.3$vals$std.error^2 + res.imp2.3$vals$std.error^2 + res.imp3.3$vals$std.error^2 + res.imp4.3$vals$std.error^2 + res.imp5.3$vals$std.error^2)
var.B <- (1/4)*( (res.imp1.3$vals$theta - theta.bar)^2 
                 + (res.imp2.3$vals$theta - theta.bar)^2 
                 + (res.imp3.3$vals$theta - theta.bar)^2  
                 + (res.imp4.3$vals$theta - theta.bar)^2  
                 + (res.imp5.3$vals$theta - theta.bar)^2 )
var.tot <- var.W + var.B + var.B/5 ## Rubin's rule for variance
SE.tot <- sqrt(var.tot)

## CIs based on (theta.bar +/- 1.96*SE.tot)
CI_up <- theta.bar+1.96*SE.tot
CI_lo <- theta.bar-1.96*SE.tot

# p-values
zscore <- theta.bar/SE.tot
pvalue <- 2*pnorm(-abs(zscore))

#-------------------------------------------------------------------------------
#PHQ boys - Imputation 1
set.seed(5555)

res.00 <- lmtp_sdr(phq.originalstack.boys$`1`, A, "Y", baseline = W_gender, time_vary = L, cens = NULL,
                   shift = static_binary_off, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundphq, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.11 <- lmtp_sdr(phq.originalstack.boys$`1`, A, "Y", baseline = W_gender, time_vary = L, cens = NULL,
                   shift = static_binary_on, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundphq, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.01 <- lmtp_sdr(phq.originalstack.boys$`1`, A, "Y", baseline = W_gender, time_vary = L, cens = NULL,
                   shift = shift_01, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundphq, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.10 <- lmtp_sdr(phq.originalstack.boys$`1`, A, "Y", baseline = W_gender, time_vary = L, cens = NULL,
                   shift = shift_10, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundphq, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.imp1.1 <- lmtp_contrast(res.00, ref = res.11)
res.imp1.2 <- lmtp_contrast(res.10, ref = res.11)
res.imp1.3 <- lmtp_contrast(res.01, ref = res.11)
#-------------------------------------------------------------------------------
#PHQ - Imputation 2
set.seed(5555)

res.00 <- lmtp_sdr(phq.originalstack.boys$`2`, A, "Y", baseline = W_gender, time_vary = L, cens = NULL,
                   shift = static_binary_off, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundphq, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.11 <- lmtp_sdr(phq.originalstack.boys$`2`, A, "Y", baseline = W_gender, time_vary = L, cens = NULL,
                   shift = static_binary_on, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundphq, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.01 <- lmtp_sdr(phq.originalstack.boys$`2`, A, "Y", baseline = W_gender, time_vary = L, cens = NULL,
                   shift = shift_01, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundphq, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.10 <- lmtp_sdr(phq.originalstack.boys$`2`, A, "Y", baseline = W_gender, time_vary = L, cens = NULL,
                   shift = shift_10, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundphq, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.imp2.1 <- lmtp_contrast(res.00, ref = res.11)
res.imp2.2 <- lmtp_contrast(res.10, ref = res.11)
res.imp2.3 <- lmtp_contrast(res.01, ref = res.11)
#-------------------------------------------------------------------------------
#PHQ - Imputation 3
set.seed(5555)

res.00 <- lmtp_sdr(phq.originalstack.boys$`3`, A, "Y", baseline = W_gender, time_vary = L, cens = NULL,
                   shift = static_binary_off, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundphq, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.11 <- lmtp_sdr(phq.originalstack.boys$`3`, A, "Y", baseline = W_gender, time_vary = L, cens = NULL,
                   shift = static_binary_on, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundphq, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.01 <- lmtp_sdr(phq.originalstack.boys$`3`, A, "Y", baseline = W_gender, time_vary = L, cens = NULL,
                   shift = shift_01, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundphq, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.10 <- lmtp_sdr(phq.originalstack.boys$`3`, A, "Y", baseline = W_gender, time_vary = L, cens = NULL,
                   shift = shift_10, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundphq, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.imp3.1 <- lmtp_contrast(res.00, ref = res.11)
res.imp3.2 <- lmtp_contrast(res.10, ref = res.11)
res.imp3.3 <- lmtp_contrast(res.01, ref = res.11)
#-------------------------------------------------------------------------------
#PHQ - Imputation 4
set.seed(5555)

res.00 <- lmtp_sdr(phq.originalstack.boys$`4`, A, "Y", baseline = W_gender, time_vary = L, cens = NULL,
                   shift = static_binary_off, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundphq, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.11 <- lmtp_sdr(phq.originalstack.boys$`4`, A, "Y", baseline = W_gender, time_vary = L, cens = NULL,
                   shift = static_binary_on, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundphq, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.01 <- lmtp_sdr(phq.originalstack.boys$`4`, A, "Y", baseline = W_gender, time_vary = L, cens = NULL,
                   shift = shift_01, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundphq, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.10 <- lmtp_sdr(phq.originalstack.boys$`4`, A, "Y", baseline = W_gender, time_vary = L, cens = NULL,
                   shift = shift_10, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundphq, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.imp4.1 <- lmtp_contrast(res.00, ref = res.11)
res.imp4.2 <- lmtp_contrast(res.10, ref = res.11)
res.imp4.3 <- lmtp_contrast(res.01, ref = res.11)
#-------------------------------------------------------------------------------
#PHQ - Imputation 5
set.seed(5555)

res.00 <- lmtp_sdr(phq.originalstack.boys$`5`, A, "Y", baseline = W_gender, time_vary = L, cens = NULL,
                   shift = static_binary_off, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundphq, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.11 <- lmtp_sdr(phq.originalstack.boys$`5`, A, "Y", baseline = W_gender, time_vary = L, cens = NULL,
                   shift = static_binary_on, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundphq, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.01 <- lmtp_sdr(phq.originalstack.boys$`5`, A, "Y", baseline = W_gender, time_vary = L, cens = NULL,
                   shift = shift_01, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundphq, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.10 <- lmtp_sdr(phq.originalstack.boys$`5`, A, "Y", baseline = W_gender, time_vary = L, cens = NULL,
                   shift = shift_10, mtp = FALSE,
                   outcome_type = "continuous",
                   id = "commid", bounds = boundphq, learners_outcome = SL.library,
                   learners_trt = SL.library, .trim = 0.99, folds = 5)

res.imp5.1 <- lmtp_contrast(res.00, ref = res.11)
res.imp5.2 <- lmtp_contrast(res.10, ref = res.11)
res.imp5.3 <- lmtp_contrast(res.01, ref = res.11)
#-------------------------------------------------------------------------------
#Combine - result 1
theta.bar <- (1/5)*(res.imp1.1$vals$theta + res.imp2.1$vals$theta + res.imp3.1$vals$theta + res.imp4.1$vals$theta + res.imp5.1$vals$theta)
var.W <- (1/5)*(res.imp1.1$vals$std.error^2 + res.imp2.1$vals$std.error^2 + res.imp3.1$vals$std.error^2 + res.imp4.1$vals$std.error^2 + res.imp5.1$vals$std.error^2)
var.B <- (1/4)*( (res.imp1.1$vals$theta - theta.bar)^2 
                 + (res.imp2.1$vals$theta - theta.bar)^2 
                 + (res.imp3.1$vals$theta - theta.bar)^2  
                 + (res.imp4.1$vals$theta - theta.bar)^2  
                 + (res.imp5.1$vals$theta - theta.bar)^2 )
var.tot <- var.W + var.B + var.B/5 ## Rubin's rule for variance
SE.tot <- sqrt(var.tot)

## CIs based on (theta.bar +/- 1.96*SE.tot)
CI_up <- theta.bar+1.96*SE.tot
CI_lo <- theta.bar-1.96*SE.tot

# p-values
zscore <- theta.bar/SE.tot
pvalue <- 2*pnorm(-abs(zscore))
#-------------------------------------------------------------------------------
#Combine - result 2
theta.bar <- (1/5)*(res.imp1.2$vals$theta + res.imp2.2$vals$theta + res.imp3.2$vals$theta + res.imp4.2$vals$theta + res.imp5.2$vals$theta)
var.W <- (1/5)*(res.imp1.2$vals$std.error^2 + res.imp2.2$vals$std.error^2 + res.imp3.2$vals$std.error^2 + res.imp4.2$vals$std.error^2 + res.imp5.2$vals$std.error^2)
var.B <- (1/4)*( (res.imp1.2$vals$theta - theta.bar)^2 
                 + (res.imp2.2$vals$theta - theta.bar)^2 
                 + (res.imp3.2$vals$theta - theta.bar)^2  
                 + (res.imp4.2$vals$theta - theta.bar)^2  
                 + (res.imp5.2$vals$theta - theta.bar)^2 )
var.tot <- var.W + var.B + var.B/5 ## Rubin's rule for variance
SE.tot <- sqrt(var.tot)

## CIs based on (theta.bar +/- 1.96*SE.tot)
CI_up <- theta.bar+1.96*SE.tot
CI_lo <- theta.bar-1.96*SE.tot

# p-values
zscore <- theta.bar/SE.tot
pvalue <- 2*pnorm(-abs(zscore))
#-------------------------------------------------------------------------------
#Combine - result 3
theta.bar <- (1/5)*(res.imp1.3$vals$theta + res.imp2.3$vals$theta + res.imp3.3$vals$theta + res.imp4.3$vals$theta + res.imp5.3$vals$theta)
var.W <- (1/5)*(res.imp1.3$vals$std.error^2 + res.imp2.3$vals$std.error^2 + res.imp3.3$vals$std.error^2 + res.imp4.3$vals$std.error^2 + res.imp5.3$vals$std.error^2)
var.B <- (1/4)*( (res.imp1.3$vals$theta - theta.bar)^2 
                 + (res.imp2.3$vals$theta - theta.bar)^2 
                 + (res.imp3.3$vals$theta - theta.bar)^2  
                 + (res.imp4.3$vals$theta - theta.bar)^2  
                 + (res.imp5.3$vals$theta - theta.bar)^2 )
var.tot <- var.W + var.B + var.B/5 ## Rubin's rule for variance
SE.tot <- sqrt(var.tot)

## CIs based on (theta.bar +/- 1.96*SE.tot)
CI_up <- theta.bar+1.96*SE.tot
CI_lo <- theta.bar-1.96*SE.tot

# p-values
zscore <- theta.bar/SE.tot
pvalue <- 2*pnorm(-abs(zscore))