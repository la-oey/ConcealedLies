setwd("/Users/loey/Desktop/Research/FakeNews/ConcealedLies/Exp1/analysis/")
library(tidyverse)
library(stats4)

raw <- read_csv("raw.csv")
my_red = c("#ffd5d6","#fc7f81","#fd2428")

glimpse(raw)
raw <- raw %>%
  mutate(catchQuestion = ifelse(catchResponse == -1, "NA", catchQuestion),
         catchQuestionAbbr = case_when(
           catchQuestion == "What was the proportion of red to blue marbles from your opponent's perspective?" ~ "baseRateOpp",
           catchQuestion == "What was the proportion of red to blue marbles from your perspective?" ~ "baseRatePla",
           catchQuestion == "How many red marbles did your opponent report drawing?" ~ "reportOpp",
           catchQuestion == "How many red marbles did you actually draw?" ~ "samplePla"
         ))

bads <- raw %>% filter(catchQuestionAbbr %in% c("reportOpp", "samplePla")) %>% group_by(subjID) %>% 
  summarise(accuracy = mean(abs(catchResponse - catchKey)<=1)) %>% 
  mutate(badsubject = accuracy < .75)

badBR <- raw %>%
  filter(catchQuestionAbbr %in% c("baseRateOpp", "baseRatePla")) %>%
  group_by(subjID) %>%
  summarise(accuracy = mean(is.na(catchKey) | abs(catchResponse - catchKey) <= 50)) %>%
  mutate(badBRsubject = accuracy < .75)

bads %>%
  filter(badsubject) %>%
  nrow()
length(unique(raw$subjID))
raw <- raw %>%
  left_join(bads) %>%
  left_join(badBR, by="subjID")

raw %>%
  filter(badsubject) %>%
  .$subjID %>%
  unique()

raw %>%
  filter(badBRsubject) %>%
  .$subjID %>%
  unique()

df <- raw %>%
  filter(!badsubject, !badBRsubject, exptPart == "trial")


df %>%
  filter(catchQuestion != "NA") %>%
  ggplot(aes(x=catchKey, y=catchResponse)) +
  geom_jitter() +
  facet_wrap(~catchQuestion, scales="free")




brResponse.overall <- raw %>%
  filter(!badsubject, catchQuestionAbbr %in% c("baseRateOpp", "baseRatePla")) %>%
  mutate(catchKey = as.factor(catchKey)) %>%
  group_by(catchQuestion, catchKey) %>%
  summarise(meanResponse = mean(catchResponse),
            varResponse = sd(catchResponse)/sqrt(n()),
            lower = meanResponse - varResponse,
            upper = meanResponse + varResponse)
raw %>%
  filter(!badsubject, catchQuestionAbbr %in% c("baseRateOpp", "baseRatePla")) %>%
  mutate(catchKey = as.factor(catchKey)) %>%
  group_by(subjID, catchQuestion, catchKey) %>%
  summarise(meanSubjResponse = mean(catchResponse)) %>%
  ggplot(aes(x=catchKey, y=meanSubjResponse, fill=catchKey)) +
  geom_point(aes(x=catchKey, y=as.numeric(as.character(catchKey))), colour="black") +
  geom_pointrange(data=brResponse.overall, aes(x=catchKey, y=meanResponse, ymin=lower, ymax=upper), colour="red", colour="black", pch=2) +
  geom_dotplot(binaxis = "y", binwidth=1.5, stackdir = "center", alpha=0.25) +
  guides(fill=FALSE) +
  facet_wrap(~catchQuestion) +
  theme_bw()


brResponse.opp <- raw %>%
  filter(!badsubject, catchQuestionAbbr == "baseRateOpp") %>%
  mutate(probBullshitDetectorRed = as.factor(probBullshitDetectorRed)) %>%
  group_by(roleCurrent, probBullshitDetectorRed) %>%
  summarise(meanResponse = mean(catchResponse),
            varResponse = sd(catchResponse)/sqrt(n()),
            lower = meanResponse - varResponse,
            upper = meanResponse + varResponse)

raw %>%
  filter(!badsubject, catchQuestionAbbr == "baseRateOpp") %>%
  mutate(probBullshitDetectorRed = as.factor(probBullshitDetectorRed)) %>%
  group_by(subjID, roleCurrent, probBullshitDetectorRed) %>%
  summarise(meanSubjResponse = mean(catchResponse)) %>%
  ggplot(aes(x=probBullshitDetectorRed, y=meanSubjResponse, fill=probBullshitDetectorRed)) +
  geom_point(aes(x=probBullshitDetectorRed, y=100*as.numeric(as.character(probBullshitDetectorRed))), colour="black") +
  geom_pointrange(data=brResponse.opp, aes(x=probBullshitDetectorRed, y=meanResponse, ymin=lower, ymax=upper), colour="red", pch=2) +
  geom_dotplot(binaxis = "y", binwidth=1.5, stackdir = "center", alpha=0.25) +
  guides(fill=FALSE) +
  facet_wrap(~roleCurrent) +
  theme_bw()



brResponse.summ <- raw %>%
  filter(!badsubject, !badBRsubject, catchQuestionAbbr == "baseRateOpp") %>%
  mutate(probBullshitDetectorRed = as.factor(probBullshitDetectorRed)) %>%
  group_by(roleCurrent, probBullshitDetectorRed) %>%
  summarise(meanResponse = mean(catchResponse),
            varResponse = sd(catchResponse)/sqrt(n()),
            lower = meanResponse - varResponse,
            upper = meanResponse + varResponse)

raw %>%
  filter(!badsubject, !badBRsubject, roleCurrent == "bullshitter", catchQuestionAbbr %in% c("baseRateOpp", "baseRatePla")) %>%
  mutate(probBullshitDetectorRed = as.factor(probBullshitDetectorRed),
         probBullshitterRed = as.factor(probBullshitterRed)) %>%
  ggplot(aes(x=probBullshitDetectorRed, y=catchResponse, fill=probBullshitDetectorRed)) +
  geom_point(aes(x=probBullshitDetectorRed, y=100*as.numeric(as.character(probBullshitDetectorRed))), colour="black") +
  geom_dotplot(binaxis = "y", binwidth=1.5, stackdir = "center", alpha=0.25) +
  stat_summary(colour="red", pch=2) +
  facet_grid(catchQuestion ~ probBullshitterRed) +
  guides(fill=FALSE) +
  theme_bw()

colorScheme = c(
  "0.2" = "blue",
  "0.5" = "purple",
  "0.8" = "red"
)

raw %>%
  filter(!badsubject, !badBRsubject,catchQuestionAbbr == "baseRatePla", roleCurrent == "bullshitDetector") %>%
  mutate(probBullshitDetectorRed = as.factor(probBullshitDetectorRed),
         probBullshitterRed = as.factor(probBullshitterRed),
         roleCurrent = ifelse(roleCurrent == "bullshitter", "sender", "receiver")) %>%
  ggplot(aes(x=probBullshitDetectorRed, y=catchResponse, fill=probBullshitDetectorRed)) +
  geom_point(aes(x=probBullshitDetectorRed, y=100*as.numeric(as.character(probBullshitDetectorRed))), colour="black") +
  geom_dotplot(binaxis = "y", binwidth=1.5, stackdir = "center", alpha=0.25) +
  stat_summary(colour="red", pch=2) +
  scale_x_discrete("receiver base rate") +
  scale_y_continuous("slider response") +
  scale_fill_manual(values=colorScheme) +
  guides(fill=FALSE) +
  theme_bw()
ggsave("img/receiver_playerProb.png", width=5, height=7)

raw %>%
  filter(!badsubject, !badBRsubject,catchQuestionAbbr == "baseRatePla", roleCurrent == "bullshitter") %>%
  mutate(probBullshitDetectorRed = as.factor(paste("receiver's base rate =", probBullshitDetectorRed)),
         probBullshitterRed = as.factor(probBullshitterRed),
         roleCurrent = ifelse(roleCurrent == "bullshitter", "sender", "receiver")) %>%
  ggplot(aes(x=probBullshitterRed, y=catchResponse, fill=probBullshitterRed)) +
  geom_point(aes(x=probBullshitterRed, y=100*as.numeric(as.character(probBullshitterRed))), colour="black") +
  geom_dotplot(binaxis = "y", binwidth=1.5, stackdir = "center", alpha=0.25) +
  stat_summary(colour="red", pch=2) +
  scale_x_discrete("sender base rate") +
  scale_y_continuous("slider response") +
  scale_fill_manual(values=colorScheme) +
  facet_grid(~probBullshitDetectorRed) +
  guides(fill=FALSE) +
  theme_bw()
ggsave("img/sender_playerProb.png", width=10, height=7)

raw %>%
  filter(!badsubject, catchQuestionAbbr == "baseRateOpp", roleCurrent == "bullshitDetector") %>%
  mutate(probBullshitDetectorRed = as.factor(paste("receiver's base rate =", probBullshitDetectorRed)),
         probBullshitterRed = as.factor(probBullshitterRed),
         roleCurrent = ifelse(roleCurrent == "bullshitter", "sender", "receiver")) %>%
  ggplot(aes(x=probBullshitterRed, y=catchResponse, fill=probBullshitterRed)) +
  geom_point(aes(x=probBullshitterRed, y=100*as.numeric(as.character(probBullshitterRed))), colour="black") +
  geom_dotplot(binaxis = "y", binwidth=1.5, stackdir = "center", alpha=0.25) +
  stat_summary(colour="red", pch=2) +
  scale_x_discrete("sender base rate") +
  scale_y_continuous("slider response") +
  scale_fill_manual(values=colorScheme) +
  facet_wrap(~probBullshitDetectorRed) +
  guides(fill=FALSE) +
  theme_bw()
ggsave("img/receiver_opponentProb_facet.png", width=10, height=7)

raw %>%
  filter(!badsubject, !badBRsubject,catchQuestionAbbr == "baseRateOpp", roleCurrent == "bullshitDetector") %>%
  mutate(probBullshitDetectorRed = as.factor(probBullshitDetectorRed),
         probBullshitterRed = as.factor(probBullshitterRed),
         roleCurrent = ifelse(roleCurrent == "bullshitter", "sender", "receiver")) %>%
  ggplot(aes(x=probBullshitDetectorRed, y=catchResponse, fill=probBullshitDetectorRed)) +
  geom_point(aes(x=probBullshitDetectorRed, y=100*as.numeric(as.character(probBullshitDetectorRed))), colour="black") +
  geom_dotplot(binaxis = "y", binwidth=1.5, stackdir = "center", alpha=0.25) +
  stat_summary(colour="red", pch=2) +
  scale_x_discrete("receiver base rate") +
  scale_y_continuous("slider response") +
  scale_fill_manual(values=colorScheme) +
  guides(fill=FALSE) +
  theme_bw()
ggsave("img/receiver_opponentProb.png", width=5, height=7)

raw %>%
  filter(!badsubject, !badBRsubject,catchQuestionAbbr == "baseRateOpp", roleCurrent == "bullshitter") %>%
  mutate(probBullshitDetectorRed = as.factor(probBullshitDetectorRed),
         probBullshitterRed = as.factor(paste("sender's base rate =", probBullshitterRed)),
         roleCurrent = ifelse(roleCurrent == "bullshitter", "sender", "receiver")) %>%
  ggplot(aes(x=probBullshitDetectorRed, y=catchResponse, fill=probBullshitDetectorRed)) +
  geom_point(aes(x=probBullshitDetectorRed, y=100*as.numeric(as.character(probBullshitDetectorRed))), colour="black") +
  geom_dotplot(binaxis = "y", binwidth=1.5, stackdir = "center", alpha=0.25) +
  stat_summary(colour="red", pch=2) +
  scale_x_discrete("receiver base rate") +
  scale_y_continuous("slider response") +
  scale_fill_manual(values=colorScheme) +
  facet_wrap(~probBullshitterRed) +
  guides(fill=FALSE) +
  theme_bw()
ggsave("img/sende_opponentProb.png", width=10, height=7)

## View Critical Data ##
df %>%
  filter(roleCurrent == "bullshitter") %>%
  mutate(probBullshitDetectorRed = as.factor(paste("bullshit detector = ", probBullshitDetectorRed)),
         probBullshitterRed = as.factor(paste("bullshitter = ", probBullshitterRed))) %>%
  ggplot(aes(x=drawnRed, y=reportedDrawn)) +
  geom_jitter() +
  stat_summary(colour="red", size=0.5) +
  facet_grid(probBullshitDetectorRed ~ probBullshitterRed) +
  theme_bw()



#################
#### lie MLE ####
#################

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


## adjust to lie detector? ##
nLL <- function(beta, mu0.2_6, alph0.2_6,
                  mu0.5_6, alph0.5_6,
                  mu0.8_6, alph0.8_6){
  k = humanLie$drawnRed
  kstar = humanLie$reportedDrawn
  prob = humanLie$probBullshitDetectorRed
  mu = case_when(
    prob == 0.2 ~ mu0.2_6,
    prob == 0.5 ~ mu0.5_6,
    prob == 0.8 ~ mu0.8_6
  )
  alph = case_when(
    prob == 0.2 ~ alph0.2_6,
    prob == 0.5 ~ alph0.5_6,
    prob == 0.8 ~ alph0.8_6
  )
  
  betas = beta
  pred = lieLogisticBinom(k, kstar, betas, mu, alph)
  # likelihood of observed kstar for that k, given parameters
  neg.log.lik = -1*sum(log(pred))
  mus = c(mu0.2_6, mu0.5_6, mu0.8_6)
  alphas = c(alph0.2_6, alph0.5_6, alph0.8_6)
  neg.log.prior = 0
  neg.log.lik+neg.log.prior
}


# as bernoulli instead of if statement
set.seed(100)
fit <- summary(mle(nLL,
                     start=list(beta=rnorm(1, 0, 0.5),
                                mu0.2_6=rnorm(1, 5, 3),
                                alph0.2_6=rnorm(1, 0, 0.5),
                                mu0.5_6=rnorm(1, 5, 3),
                                alph0.5_6=rnorm(1, 0, 0.5),
                                mu0.8_6=rnorm(1, 5, 3),
                                alph0.8_6=rnorm(1, 0, 0.5)),
                     method = "BFGS"))
fit

lieMLE.pred <- data.frame(k=rep(rep(0:10,each=11),3), 
                          kstar=rep(0:10, 11*3),
                          p=as.factor(c(rep(0.2,11*11), rep(0.5,11*11), rep(0.8,11*11))),
                          expt=as.factor(rep("expt6",11*11*3)),
                          betas=c(rep(fit@coef["beta","Estimate"],11*11*3)),
                          mu=c(rep(fit@coef["mu0.2_6","Estimate"],11*11),
                               rep(fit@coef["mu0.5_6","Estimate"],11*11),
                               rep(fit@coef["mu0.8_6","Estimate"],11*11)),
                          mu.se=c(rep(fit@coef["mu0.2_6","Std. Error"],11*11),
                                  rep(fit@coef["mu0.5_6","Std. Error"],11*11),
                                  rep(fit@coef["mu0.8_6","Std. Error"],11*11)),
                          alph=c(rep(fit@coef["alph0.2_6","Estimate"],11*11),
                                 rep(fit@coef["alph0.5_6","Estimate"],11*11),
                                 rep(fit@coef["alph0.8_6","Estimate"],11*11)),
                          alph.se=c(rep(fit@coef["alph0.2_6","Std. Error"],11*11),
                                    rep(fit@coef["alph0.5_6","Std. Error"],11*11),
                                    rep(fit@coef["alph0.8_6","Std. Error"],11*11))) %>%
  mutate(p.lie.k = p.lie.k(k, betas, mu),
         p.kstar.k=lieLogisticBinom(k, kstar, betas, mu, alph),
         binom.kstar = binom.p(kstar, alph),
         logp.kstar.k = log(p.kstar.k))

lieMLE.pred %>%
  select(p, expt, alph, alph.se) %>%
  unique() %>%
  mutate(p_expt = paste0(p, "_", expt),
         meanAlph = 10*logitToProb(alph)) %>%
  ggplot(aes(x=p, y=meanAlph)) +
  geom_bar(stat="identity", position="dodge") +
  geom_errorbar(aes(x=p, min=10*logitToProb(alph-alph.se), max=10*logitToProb(alph+alph.se)), width=.3, position=position_dodge(.9)) +
  scale_y_continuous("Mean Lie", limits=c(0,10))




## only using liar's probability? ##

nLL.2 <- function(beta, mu0.2_6, alph0.2_6,
                mu0.5_6, alph0.5_6,
                mu0.8_6, alph0.8_6){
  k = humanLie$drawnRed
  kstar = humanLie$reportedDrawn
  prob = humanLie$probBullshitterRed
  mu = case_when(
    prob == 0.2 ~ mu0.2_6,
    prob == 0.5 ~ mu0.5_6,
    prob == 0.8 ~ mu0.8_6
  )
  alph = case_when(
    prob == 0.2 ~ alph0.2_6,
    prob == 0.5 ~ alph0.5_6,
    prob == 0.8 ~ alph0.8_6
  )
  
  betas = beta
  pred = lieLogisticBinom(k, kstar, betas, mu, alph)
  # likelihood of observed kstar for that k, given parameters
  neg.log.lik = -1*sum(log(pred))
  mus = c(mu0.2_6, mu0.5_6, mu0.8_6)
  alphas = c(alph0.2_6, alph0.5_6, alph0.8_6)
  neg.log.prior = 0
  neg.log.lik+neg.log.prior
}

# as bernoulli instead of if statement
fit.2 <- summary(mle(nLL.2,
                   start=list(beta=rnorm(1, 0, 0.5),
                              mu0.2_6=rnorm(1, 5, 3),
                              alph0.2_6=rnorm(1, 0, 0.5),
                              mu0.5_6=rnorm(1, 5, 3),
                              alph0.5_6=rnorm(1, 0, 0.5),
                              mu0.8_6=rnorm(1, 5, 3),
                              alph0.8_6=rnorm(1, 0, 0.5)),
                   method = "BFGS"))
fit.2
fit

lieMLE.pred.2 <- data.frame(k=rep(rep(0:10,each=11),3), 
                          kstar=rep(0:10, 11*3),
                          p=as.factor(c(rep(0.2,11*11), rep(0.5,11*11), rep(0.8,11*11))),
                          expt=as.factor(rep("expt6",11*11*3)),
                          betas=c(rep(fit.2@coef["beta","Estimate"],11*11*3)),
                          mu=c(rep(fit.2@coef["mu0.2_6","Estimate"],11*11),
                               rep(fit.2@coef["mu0.5_6","Estimate"],11*11),
                               rep(fit.2@coef["mu0.8_6","Estimate"],11*11)),
                          mu.se=c(rep(fit.2@coef["mu0.2_6","Std. Error"],11*11),
                                  rep(fit.2@coef["mu0.5_6","Std. Error"],11*11),
                                  rep(fit.2@coef["mu0.8_6","Std. Error"],11*11)),
                          alph=c(rep(fit.2@coef["alph0.2_6","Estimate"],11*11),
                                 rep(fit.2@coef["alph0.5_6","Estimate"],11*11),
                                 rep(fit.2@coef["alph0.8_6","Estimate"],11*11)),
                          alph.se=c(rep(fit.2@coef["alph0.2_6","Std. Error"],11*11),
                                    rep(fit.2@coef["alph0.5_6","Std. Error"],11*11),
                                    rep(fit.2@coef["alph0.8_6","Std. Error"],11*11))) %>%
  mutate(p.lie.k = p.lie.k(k, betas, mu),
         p.kstar.k=lieLogisticBinom(k, kstar, betas, mu, alph),
         binom.kstar = binom.p(kstar, alph),
         logp.kstar.k = log(p.kstar.k))

lieMLE.pred.2 %>%
  select(p, expt, alph, alph.se) %>%
  unique() %>%
  mutate(p_expt = paste0(p, "_", expt),
         meanAlph = 10*logitToProb(alph)) %>%
  ggplot(aes(x=p, y=meanAlph)) +
  geom_bar(stat="identity", position="dodge") +
  geom_errorbar(aes(x=p, min=10*logitToProb(alph-alph.se), max=10*logitToProb(alph+alph.se)), width=.3, position=position_dodge(.9)) +
  scale_y_continuous("Mean Lie", limits=c(0,10))




#### Combined ####

nLL.comb <- function(beta, mu0.2_0.2, alph0.2_0.2, mu0.2_0.5, alph0.2_0.5, mu0.2_0.8, alph0.2_0.8,
                mu0.5_0.2, alph0.5_0.2, mu0.5_0.5, alph0.5_0.5, mu0.5_0.8, alph0.5_0.8,
                mu0.8_0.2, alph0.8_0.2, mu0.8_0.5, alph0.8_0.5, mu0.8_0.8, alph0.8_0.8){
  k = humanLie$drawnRed
  kstar = humanLie$reportedDrawn
  expt = humanLie$expt
  probLiar = humanLie$probBullshitterRed
  probDetector = humanLie$probBullshitDetectorRed
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
  mus = c(mu0.2_0.2,mu0.2_0.5,mu0.2_0.8,mu0.5_0.2,mu0.5_0.5,mu0.5_0.8,mu0.8_0.2,mu0.8_0.5,mu0.8_0.8)
  alphas = c(alph0.2_0.2,alph0.2_0.5,alph0.2_0.8,alph0.5_0.2,alph0.5_0.5,alph0.5_0.8,alph0.8_0.2,alph0.8_0.5,alph0.8_0.8)
  neg.log.prior = 0
  neg.log.lik+neg.log.prior
}

fit.comb <- summary(mle(nLL.comb,
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

lieMLE.pred.comb <- data.frame(k=rep(rep(0:10,each=11),3*3), 
                            kstar=rep(0:10, 11*3*3),
                            p.liar=as.factor(rep(c(rep(0.2,11*11), rep(0.5,11*11), rep(0.8,11*11)),3)),
                            p.detector=as.factor(rep(c(rep(0.2,11*11), rep(0.5,11*11), rep(0.8,11*11)), each=3)),
                            expt=as.factor(rep("expt6",11*11*3)),
                            betas=c(rep(fit.comb@coef["beta","Estimate"],11*11*3)),
                            mu=c(rep(fit.comb@coef["mu0.2_0.2","Estimate"],11*11),
                                 rep(fit.comb@coef["mu0.2_0.5","Estimate"],11*11),
                                 rep(fit.comb@coef["mu0.2_0.8","Estimate"],11*11),
                                 rep(fit.comb@coef["mu0.5_0.2","Estimate"],11*11),
                                 rep(fit.comb@coef["mu0.5_0.5","Estimate"],11*11),
                                 rep(fit.comb@coef["mu0.5_0.8","Estimate"],11*11),
                                 rep(fit.comb@coef["mu0.8_0.2","Estimate"],11*11),
                                 rep(fit.comb@coef["mu0.8_0.5","Estimate"],11*11),
                                 rep(fit.comb@coef["mu0.8_0.8","Estimate"],11*11)),
                            mu.se=c(rep(fit.comb@coef["mu0.2_0.2","Std. Error"],11*11),
                                    rep(fit.comb@coef["mu0.2_0.5","Std. Error"],11*11),
                                    rep(fit.comb@coef["mu0.2_0.8","Std. Error"],11*11),
                                    rep(fit.comb@coef["mu0.5_0.2","Std. Error"],11*11),
                                    rep(fit.comb@coef["mu0.5_0.5","Std. Error"],11*11),
                                    rep(fit.comb@coef["mu0.5_0.8","Std. Error"],11*11),
                                    rep(fit.comb@coef["mu0.8_0.2","Std. Error"],11*11),
                                    rep(fit.comb@coef["mu0.8_0.5","Std. Error"],11*11),
                                    rep(fit.comb@coef["mu0.8_0.8","Std. Error"],11*11)),
                            alph=c(rep(fit.comb@coef["alph0.2_0.2","Estimate"],11*11),
                                   rep(fit.comb@coef["alph0.2_0.5","Estimate"],11*11),
                                   rep(fit.comb@coef["alph0.2_0.8","Estimate"],11*11),
                                   rep(fit.comb@coef["alph0.5_0.2","Estimate"],11*11),
                                   rep(fit.comb@coef["alph0.5_0.5","Estimate"],11*11),
                                   rep(fit.comb@coef["alph0.5_0.8","Estimate"],11*11),
                                   rep(fit.comb@coef["alph0.8_0.2","Estimate"],11*11),
                                   rep(fit.comb@coef["alph0.8_0.5","Estimate"],11*11),
                                   rep(fit.comb@coef["alph0.8_0.8","Estimate"],11*11)),
                            alph.se=c(rep(fit.comb@coef["alph0.2_0.2","Std. Error"],11*11),
                                      rep(fit.comb@coef["alph0.2_0.5","Std. Error"],11*11),
                                      rep(fit.comb@coef["alph0.2_0.8","Std. Error"],11*11),
                                      rep(fit.comb@coef["alph0.5_0.2","Std. Error"],11*11),
                                      rep(fit.comb@coef["alph0.5_0.5","Std. Error"],11*11),
                                      rep(fit.comb@coef["alph0.5_0.8","Std. Error"],11*11),
                                      rep(fit.comb@coef["alph0.8_0.2","Std. Error"],11*11),
                                      rep(fit.comb@coef["alph0.8_0.5","Std. Error"],11*11),
                                      rep(fit.comb@coef["alph0.8_0.8","Std. Error"],11*11))) %>%
  mutate(p.lie.k = p.lie.k(k, betas, mu),
         p.kstar.k=lieLogisticBinom(k, kstar, betas, mu, alph),
         binom.kstar = binom.p(kstar, alph),
         logp.kstar.k = log(p.kstar.k))

lieMLE.pred.comb %>%
  select(p.liar, p.detector, expt, alph, alph.se) %>%
  unique() %>%
  mutate(p.liar = paste0("sender's base rate = ", 100*as.numeric(as.character(p.liar)), "% red"),
         p.detector = paste0(100*as.numeric(as.character(p.detector)), "% red"),
         meanAlph = 10*logitToProb(alph)) %>%
  ggplot(aes(x=p.detector, y=meanAlph, fill=p.detector)) +
  geom_bar(stat="identity", position="dodge", colour="black", width=1) +
  geom_errorbar(aes(x=p.detector, min=10*logitToProb(alph-alph.se), max=10*logitToProb(alph+alph.se)), width=.3, position=position_dodge(.9)) +
  scale_x_discrete("receiver's base rate") +
  scale_y_continuous("Mean Lie", limits=c(0,10), breaks=seq(0,10,2), expand=c(0,0)) +
  scale_fill_manual(values = my_red, guide=FALSE) +
  facet_wrap(~p.liar) +
  theme_bw() +
  theme(strip.background = element_rect(fill="gray40"),
        strip.text = element_text(colour="white",face="bold"))
ggsave("img/meanLie.png", width=7, height=4)

humanLie %>%
  select(probBullshitterRed, probBullshitDetectorRed, probGlobal) %>%
  distinct() %>%
  arrange(probBullshitterRed, probBullshitDetectorRed)

lieParams <- lieMLE.pred.comb %>%
  select(p.liar, p.detector, expt, alph, alph.se) %>%
  unique() %>%
  mutate(num.p.liar = as.numeric(as.character(p.liar)),
         num.p.detector = as.numeric(as.character(p.detector)),
         meanAlph = 10*logitToProb(alph))
glimpse(lieParams)

m <- lm(meanAlph ~ num.p.liar + num.p.detector, data=lieParams)
summary(m)
