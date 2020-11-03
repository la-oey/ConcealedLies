setwd("/Users/loey/Desktop/Research/FakeNews/ConcealedLies/Exp1/analysis/")
library(tidyverse)
library(stats4)

raw <- read_csv("raw.csv")

glimpse(raw)
raw <- raw %>%
  mutate(catchQuestion = ifelse(catchResponse == -1, "NA", catchQuestion),
         catchQuestionAbbr = case_when(
           catchQuestion == "What was the proportion of red to blue marbles from your opponent's perspective?" ~ "baseRateOpp",
           catchQuestion == "What was the proportion of red to blue marbles from your perspective?" ~ "baseRatePla",
           catchQuestion == "How many red marbles did your opponent report drawing?" ~ "reportOpp",
           catchQuestion == "How many red marbles did you actually draw?" ~ "samplePla"
         ),
         subjID = as.factor(paste0("subj",str_pad(group_indices(.,subjID),3,pad="0"))))

bads <- raw %>% filter(catchQuestionAbbr %in% c("reportOpp", "samplePla")) %>% group_by(subjID) %>% 
  summarise(accuracy = mean(abs(catchResponse - catchKey)<=1)) %>% 
  mutate(badsubject = accuracy < .75)

badBR <- raw %>%
  filter(catchQuestionAbbr %in% c("baseRateOpp", "baseRatePla")) %>%
  group_by(subjID) %>%
  summarise(accuracy = mean(is.na(catchKey) | abs(catchResponse - catchKey) <= 50)) %>%
  mutate(badBRsubject = accuracy < .75)

raw <- raw %>%
  left_join(bads) %>%
  left_join(badBR, by="subjID")

df <- raw %>%
  filter(!badsubject, !badBRsubject, exptPart == "trial")


################################
###### Analysis Functions ######
################################


humanLie <- df %>%
  filter(roleCurrent == "bullshitter")

logitToProb <- function(logit){
  exp(logit) / (1+exp(logit))
}

probToLogit <- function(prob){
  log(prob / (1 - prob))
}

lieLogisticModel <- function(k, beta, mu){
  logodds = beta * (k-mu)
  logitToProb(pmin(10, pmax(-10, logodds)))
}

p.lie.k <- function(k, beta, mu){
  lieLogisticModel(k, beta, mu)
}


p.kstar.k <- function(k, kstar, alph){
  alph = logitToProb(alph)
  dbinom(kstar, 10, alph) / (1-dbinom(k, 10, alph))  
}

lieLogisticBinom <- function(k, kstar, beta, mu, alph){
  p.lie = p.lie.k(k, beta, mu)
  ifelse(k == kstar, 1 - p.lie, p.lie * p.kstar.k(k, kstar, alph))
}

binom.p <- function(kstar, alph){
  alph = logitToProb(alph)
  dbinom(kstar, 10, alph)
}



###################
## Analysis Loop ##
###################

indivFits <- data.frame(subjID = NULL, 
                        expt = NULL, 
                        prob = NULL, 
                        beta.est = NULL, 
                        beta.se = NULL, 
                        mu0.2_0.2.est = NULL, 
                        mu0.2_0.2.se = NULL, 
                        alph0.2_0.2.est = NULL, 
                        alph0.2_0.2.se = NULL,
                        mu0.2_0.5.est = NULL, 
                        mu0.2_0.5.se = NULL, 
                        alph0.2_0.5.est = NULL, 
                        alph0.2_0.5.se = NULL,
                        mu0.2_0.8.est = NULL, 
                        mu0.2_0.8.se = NULL, 
                        alph0.2_0.8.est = NULL, 
                        alph0.2_0.8.se = NULL,
                        mu0.5_0.2.est = NULL, 
                        mu0.5_0.2.se = NULL, 
                        alph0.5_0.2.est = NULL, 
                        alph0.5_0.2.se = NULL,
                        mu0.5_0.5.est = NULL, 
                        mu0.5_0.5.se = NULL, 
                        alph0.5_0.5.est = NULL, 
                        alph0.5_0.5.se = NULL,
                        mu0.5_0.8.est = NULL, 
                        mu0.5_0.8.se = NULL, 
                        alph0.5_0.8.est = NULL, 
                        alph0.5_0.8.se = NULL,
                        mu0.8_0.2.est = NULL, 
                        mu0.8_0.2.se = NULL, 
                        alph0.8_0.2.est = NULL, 
                        alph0.8_0.2.se = NULL,
                        mu0.8_0.5.est = NULL, 
                        mu0.8_0.5.se = NULL, 
                        alph0.8_0.5.est = NULL, 
                        alph0.8_0.5.se = NULL,
                        mu0.8_0.8.est = NULL, 
                        mu0.8_0.8.se = NULL, 
                        alph0.8_0.8.est = NULL, 
                        alph0.8_0.8.se = NULL)
#subjNum <- "subj022"
for(subjNum in unique(humanLie$subjID)){
  humanLie.subj <- filter(humanLie, subjID == subjNum)
  try({
    nLL.indiv <- function(beta, mu0.2_0.2, alph0.2_0.2, mu0.2_0.5, alph0.2_0.5, mu0.2_0.8, alph0.2_0.8,
                          mu0.5_0.2, alph0.5_0.2, mu0.5_0.5, alph0.5_0.5, mu0.5_0.8, alph0.5_0.8,
                          mu0.8_0.2, alph0.8_0.2, mu0.8_0.5, alph0.8_0.5, mu0.8_0.8, alph0.8_0.8){
      k = humanLie.subj$drawnRed
      kstar = humanLie.subj$reportedDrawn
      probLiar = humanLie.subj$probBullshitterRed
      probDetector = humanLie.subj$probBullshitDetectorRed
      mu = case_when(
        probLiar == 0.2 & probDetector == 0.2 ~ mu0.2_0.2,
        probLiar == 0.2 & probDetector == 0.5 ~ mu0.2_0.5,
        probLiar == 0.2 & probDetector == 0.8 ~ mu0.2_0.8,
        probLiar == 0.5 & probDetector == 0.2 ~ mu0.5_0.2,
        probLiar == 0.5 & probDetector == 0.5 ~ mu0.5_0.5,
        probLiar == 0.5 & probDetector == 0.8 ~ mu0.5_0.8,
        probLiar == 0.8 & probDetector == 0.2 ~ mu0.8_0.2,
        probLiar == 0.8 & probDetector == 0.5 ~ mu0.8_0.5,
        probLiar == 0.8 & probDetector == 0.8 ~ mu0.8_0.8,
      )
      alph = case_when(
        probLiar == 0.2 & probDetector == 0.2 ~ alph0.2_0.2,
        probLiar == 0.2 & probDetector == 0.5 ~ alph0.2_0.5,
        probLiar == 0.2 & probDetector == 0.8 ~ alph0.2_0.8,
        probLiar == 0.5 & probDetector == 0.2 ~ alph0.5_0.2,
        probLiar == 0.5 & probDetector == 0.5 ~ alph0.5_0.5,
        probLiar == 0.5 & probDetector == 0.8 ~ alph0.5_0.8,
        probLiar == 0.8 & probDetector == 0.2 ~ alph0.8_0.2,
        probLiar == 0.8 & probDetector == 0.5 ~ alph0.8_0.5,
        probLiar == 0.8 & probDetector == 0.8 ~ alph0.8_0.8,
      )
      betas = beta
      
      pred = lieLogisticBinom(k, kstar, betas, mu, alph)
      # likelihood of observed kstar for that k, given parameters
      neg.log.lik = -1*sum(log(pred))
      mus = c(mu0.2_0.2, mu0.2_0.5, mu0.2_0.8, mu0.5_0.2, mu0.5_0.5, mu0.5_0.8, mu0.8_0.2, mu0.8_0.5, mu0.8_0.8)
      alphs = c(alph0.2_0.2, alph0.2_0.5, alph0.2_0.8, alph0.5_0.2, alph0.5_0.5, alph0.5_0.8, alph0.8_0.2, alph0.8_0.5, alph0.8_0.8)
      neg.log.prior = sum(.0001*(mus-5)^2) - abs(beta)
      neg.log.lik + neg.log.prior
    }
    
    fit.subj <- summary(mle(nLL.indiv,
                            start=list(beta=rnorm(1, 0, 0.5),
                                       mu0.2_0.2=rnorm(1, 5, 3),
                                       alph0.2_0.2=rnorm(1, 0, 0.5),
                                       mu0.2_0.5=rnorm(1, 5, 3),
                                       alph0.2_0.5=rnorm(1, 0, 0.5),
                                       mu0.2_0.8=rnorm(1, 5, 3),
                                       alph0.2_0.8=rnorm(1, 0, 0.5),
                                       mu0.5_0.2=rnorm(1, 5, 3),
                                       alph0.5_0.2=rnorm(1, 0, 0.5),
                                       mu0.5_0.5=rnorm(1, 5, 3),
                                       alph0.5_0.5=rnorm(1, 0, 0.5),
                                       mu0.5_0.8=rnorm(1, 5, 3),
                                       alph0.5_0.8=rnorm(1, 0, 0.5),
                                       mu0.8_0.2=rnorm(1, 5, 3),
                                       alph0.8_0.2=rnorm(1, 0, 0.5),
                                       mu0.8_0.5=rnorm(1, 5, 3),
                                       alph0.8_0.5=rnorm(1, 0, 0.5),
                                       mu0.8_0.8=rnorm(1, 5, 3),
                                       alph0.8_0.8=rnorm(1, 0, 0.5)),
                            method = "BFGS"))
    newdf <- data.frame(subjID = subjNum,
                        beta.est = fit.subj@coef["beta","Estimate"],
                        beta.se = fit.subj@coef["beta","Std. Error"],
                        mu0.2_0.2.est = fit.subj@coef["mu0.2_0.2","Estimate"],
                        mu0.2_0.2.se = fit.subj@coef["mu0.2_0.2","Std. Error"],
                        alph0.2_0.2.est = fit.subj@coef["alph0.2_0.2","Estimate"],
                        alph0.2_0.2.se = fit.subj@coef["alph0.2_0.2","Std. Error"],
                        mu0.2_0.5.est = fit.subj@coef["mu0.2_0.5","Estimate"],
                        mu0.2_0.5.se = fit.subj@coef["mu0.2_0.5","Std. Error"],
                        alph0.2_0.5.est = fit.subj@coef["alph0.2_0.5","Estimate"],
                        alph0.2_0.5.se = fit.subj@coef["alph0.2_0.5","Std. Error"],
                        mu0.2_0.8.est = fit.subj@coef["mu0.2_0.8","Estimate"],
                        mu0.2_0.8.se = fit.subj@coef["mu0.2_0.8","Std. Error"],
                        alph0.2_0.8.est = fit.subj@coef["alph0.2_0.8","Estimate"],
                        alph0.2_0.8.se = fit.subj@coef["alph0.2_0.8","Std. Error"],
                        mu0.5_0.2.est = fit.subj@coef["mu0.5_0.2","Estimate"],
                        mu0.5_0.2.se = fit.subj@coef["mu0.5_0.2","Std. Error"],
                        alph0.5_0.2.est = fit.subj@coef["alph0.5_0.2","Estimate"],
                        alph0.5_0.2.se = fit.subj@coef["alph0.5_0.2","Std. Error"],
                        mu0.5_0.5.est = fit.subj@coef["mu0.5_0.5","Estimate"],
                        mu0.5_0.5.se = fit.subj@coef["mu0.5_0.5","Std. Error"],
                        alph0.5_0.5.est = fit.subj@coef["alph0.5_0.5","Estimate"],
                        alph0.5_0.5.se = fit.subj@coef["alph0.5_0.5","Std. Error"],
                        mu0.5_0.8.est = fit.subj@coef["mu0.5_0.8","Estimate"],
                        mu0.5_0.8.se = fit.subj@coef["mu0.5_0.8","Std. Error"],
                        alph0.5_0.8.est = fit.subj@coef["alph0.5_0.8","Estimate"],
                        alph0.5_0.8.se = fit.subj@coef["alph0.5_0.8","Std. Error"],
                        mu0.8_0.2.est = fit.subj@coef["mu0.8_0.2","Estimate"],
                        mu0.8_0.2.se = fit.subj@coef["mu0.8_0.2","Std. Error"],
                        alph0.8_0.2.est = fit.subj@coef["alph0.8_0.2","Estimate"],
                        alph0.8_0.2.se = fit.subj@coef["alph0.8_0.2","Std. Error"],
                        mu0.8_0.5.est = fit.subj@coef["mu0.8_0.5","Estimate"],
                        mu0.8_0.5.se = fit.subj@coef["mu0.8_0.5","Std. Error"],
                        alph0.8_0.5.est = fit.subj@coef["alph0.8_0.5","Estimate"],
                        alph0.8_0.5.se = fit.subj@coef["alph0.8_0.5","Std. Error"],
                        mu0.8_0.8.est = fit.subj@coef["mu0.8_0.8","Estimate"],
                        mu0.8_0.8.se = fit.subj@coef["mu0.8_0.8","Std. Error"],
                        alph0.8_0.8.est = fit.subj@coef["alph0.8_0.8","Estimate"],
                        alph0.8_0.8.se = fit.subj@coef["alph0.8_0.8","Std. Error"]
    )
    indivFits <- bind_rows(indivFits, newdf)
  }, TRUE)
}
head(indivFits, 20)

errorSubj <- setdiff(humanLie$subjID, indivFits$subjID) #subjects that threw an error in fitting data
length(errorSubj)

length(indivFits$subjID)

nSubj <- length(unique(indivFits$subjID))
lieMLE.pred <- data.frame(subj=rep(indivFits$subjID,each=11*11),
                          #p.liar=as.factor(rep(indivFits$probLiar,each=11*11)),
                          #p.detector=as.factor(rep(indivFits$probDetector,each=11*11)),
                          #expt=as.factor(rep(indivFits$expt,each=11*11)),
                          k=rep(rep(0:10,each=11),nSubj), 
                          kstar=rep(0:10, 11*nSubj),
                          beta=rep(indivFits$beta.est,each=11*11),
                          beta.se=rep(indivFits$beta.se,each=11*11),
                          mu=rep(indivFits$mu.est,each=11*11),
                          mu.se=rep(indivFits$mu.se,each=11*11),
                          alph=rep(indivFits$alph.est,each=11*11),
                          alph.se=rep(indivFits$alph.se,each=11*11)) %>%
  mutate(beta = ifelse(expt=="expt4", beta, -beta),
         p.lie.k = p.lie.k(k, beta, mu),
         p.kstar.k=lieLogisticBinom(k, kstar, beta, mu, alph),
         binom.kstar = binom.p(kstar, alph),
         logp.kstar.k = log(p.kstar.k))









# check that participants have enough data
glimpse(humanLie)
humanLie %>%
  group_by(subjID, probBullshitterRed, probBullshitDetectorRed, drawnRed) %>%
  count() %>%
  filter(subjID %in% indivFits$subjID) %>%
  View()

