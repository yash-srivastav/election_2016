## (1) Importing data
setwd("/Users/yashsrivastav/Dropbox/Personal_Projects/Election_Analysis/Data")
library(mfx);library(reshape2);library(ggplot2);library(Hmisc);library(dplyr)
library(xtable)
election_2016 <- read.csv("Clean_Data/election_2016.csv")
election_2020 <- read.csv("Clean_Data/election_2020.csv")
election_2016_lpm <- read.csv("Clean_Data/election_2016_lpm.csv")
election_2020_lpm <- read.csv("Clean_Data/election_2020_lpm.csv")

## (2) Data Exploration
# Correlation matrix
cor16 <- round(cor(election_2016_lpm %>%
               dplyr::select(college,unemp_rate,
                             wht,med_inc,Manufacturing,
                             life_exp,candidate_share),
             use = "complete.obs"),2)
cor20 <- round(cor(election_2020_lpm %>%
               dplyr::select(college,unemp_rate,
                             wht,med_inc,Manufacturing,
                             life_exp, case_rate,death_rate,
                             candidate_share),
             use = "complete.obs"),2)

cor16[upper.tri(cor16)] <- ""
cor20[upper.tri(cor20)] <- ""
print(xtable(cor16),type = "latex")


#Some important conditional expectations
mean(election_2020_lpm$med_inc[election_2020_lpm$outcome == 1],na.rm = TRUE)
mean(election_2020_lpm$med_inc[election_2020_lpm$outcome == 0],na.rm = TRUE)
mean(election_2020_lpm$college[election_2020_lpm$outcome == 1],na.rm = TRUE)
mean(election_2020_lpm$college[election_2020_lpm$outcome == 0],na.rm = TRUE)
mean(election_2020_lpm$Manufacturing[election_2020_lpm$outcome == 1],na.rm = TRUE)
mean(election_2020_lpm$Manufacturing[election_2020_lpm$outcome == 0],na.rm = TRUE)
mean(election_2020_lpm$death_rate[election_2020_lpm$outcome == 1],na.rm = TRUE)
mean(election_2020_lpm$death_rate[election_2020_lpm$outcome == 0],na.rm = TRUE)
mean(election_2020_lpm$wht[election_2020_lpm$outcome == 1],na.rm = TRUE)
mean(election_2020_lpm$wht[election_2020_lpm$outcome == 0],na.rm = TRUE)
mean(election_2020_lpm$life_exp[election_2020_lpm$outcome == 1],na.rm = TRUE)
mean(election_2020_lpm$life_exp[election_2020_lpm$outcome == 0],na.rm = TRUE)

mean(election_2016_lpm$med_inc[election_2016_lpm$outcome == 1],na.rm = TRUE)
mean(election_2016_lpm$med_inc[election_2016_lpm$outcome == 0],na.rm = TRUE)

#Testing for significance in differences between group means
t.test(med_inc ~ outcome, data = election_2020_lpm)
t.test(college ~ outcome, data = election_2020_lpm)
t.test(Manufacturing ~ outcome, data = election_2020_lpm)
t.test(death_rate ~ outcome, data = election_2020_lpm)
t.test(wht ~ outcome, data = election_2020_lpm)
t.test(life_exp ~ outcome, data = election_2020_lpm)

#Some histograms
hist.data.frame(election_2020_lpm %>%
                  dplyr::select(candidate_share,case_rate,death_rate,med_inc,
                         college,unemp_rate,wht,Manufacturing,life_exp) %>%
                  rename(mfg = Manufacturing,
                         trump_share = candidate_share))
hist.data.frame(election_2016_lpm %>%
                  ungroup() %>%
                  dplyr::select(candidate_share,med_inc,college,unemp_rate,
                                wht,Manufacturing,life_exp) %>%
                  rename(mfg = Manufacturing,
                         trump_share = candidate_share))

# Looking at county-level heterogeneities
var_20 <- election_2020_lpm %>%
  group_by(state) %>%
  summarise(cand_share = sqrt(var(candidate_share,na.rm = TRUE)))
var_16 <- election_2016_lpm %>%
  group_by(state) %>%
  summarise(cand_share = sqrt(var(candidate_share,na.rm = TRUE)))
var_both <- var_20 %>%
  inner_join(var_16, by = "state") %>%
  rename(var_share20 = cand_share.x,
         var_share16 = cand_share.y) %>%
  mutate(chg = log(var_share20) - log(var_share16))
rm(var_16,var_20)

# Attempt some mapping
library(maps)
us_states <- map_data("state")
us_states <- us_states %>%
  mutate(region = toupper(region)) %>%
  left_join(var_20, by = c("region" = "state"))
ggplot(data = us_states,
       aes(x = long, y = lat,
           group = group, fill = cand_share)) +
  geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45)

# Verifying that third party candidates don't win any counties
x <- election_2020 %>% 
  group_by(county,state_po) %>%
  mutate(outcome = ifelse(totalcandvotes == max(totalcandvotes,na.rm = TRUE),
                          1,
                          0))
x <- x %>%
  filter(party == "OTHER",
         outcome == 1)
rm(x)

## (2) Analysis
# Linear probability model
lpm2016 <- lm(outcome ~ log(med_inc) +
                college + unemp_rate +
                wht + Manufacturing +
                life_exp +
                as.factor(state_po),
              data = election_2016_lpm)
summary(lpm2016)
coeftest(lpm2016,vcov = vcovHC(lpm2016, type="HC1"))

lpm2020 <- lm(outcome ~ log(med_inc) +
                 college + unemp_rate +
                 wht + Manufacturing +
                 life_exp +
                 death_rate +
                 as.factor(state_po),
               data = election_2020_lpm)
summary(lpm2020)
coeftest(lpm2020,vcov = vcovHC(lpm2020, type="HC1"))

# Looking at both 2020 and 2016 elections
both_elections <- election_2020_lpm %>%
  filter(is.na(county) == F) %>%
  left_join(election_2016_lpm %>%
              mutate(candidate_share = candidate_share * 100) %>%
              dplyr::select(state_po,county,candidate_share,outcome) %>%
              filter(is.na(county) == F),
            by = c("state_po","county")) %>%
  rename(candidate_share2016 = candidate_share.y,
         candidate_share2020 = candidate_share.x,
         outcome2016 = outcome.y,
         outcome2020 = outcome.x) %>%
  filter(partydum == 2) %>%
  mutate(candidate_share2016 = candidate_share2016 * 100)
both_mod <- lm(outcome2020 ~ log(med_inc) +
                 unemp_rate +
                 college +
                 wht + Manufacturing +
                 life_exp + death_rate +
                 candidate_share2016 +
                 as.factor(state_po),
               data = both_elections)
summary(both_mod)
coeftest(both_mod,vcov = vcovHC(both_mod, type="HC1"))

# Looking at counties where 2016 vote != 2020 vote
diff_votes <- both_elections %>%
  filter(outcome2016 != outcome2020)
diff_votes <- diff_votes %>%
  left_join(election_2020 %>%
              group_by(state_po,county) %>%
              mutate(biden20 = ifelse(candidate_share == max(candidate_share),
                                    1,
                                    0)) %>%
              ungroup() %>% 
              filter(party == "DEMOCRAT") %>%
              dplyr::select(state_po,county,biden20),
            by = c("state_po","county"))

#number of counties that went Trump in 2016 and Biden in 2020
sum(diff_votes$outcome2016 == 1 & diff_votes$biden20 == 1)

# Try a model where we look at the change in median income and unemployment
election_2020_lpm_ <- election_2020_lpm %>%
  filter(is.na(county) == F) %>%
  left_join(election_2016_lpm %>%
              distinct(state_po,county,candidate_share,med_inc,unemp_rate) %>%
              filter(is.na(county) == F),
            by = c("state_po","county")) %>%
  dplyr::rename(candidate_share20 = candidate_share.y,
         candidate_share16 = candidate_share.y,
         med_inc20 = med_inc.x,
         med_inc16 = med_inc.y,
         unemp_rate20 = unemp_rate.x,
         unemp_rate16 = unemp_rate.y) %>%
  mutate(chg_unemp = log(unemp_rate20) - log(unemp_rate16),
         chg_medinc = log(med_inc20) - log(med_inc16),
         candidate_share16 = candidate_share16 * 100)

lpm_2020 <- lm(outcome ~ chg_medinc +
                 chg_unemp +
                 med_inc20 + unemp_rate20 +
                 college +
                 wht + Manufacturing +
                 life_exp + death_rate +
                 candidate_share16 +
                 as.factor(state_po),
               data = election_2020_lpm_)
summary(lm(outcome ~ candidate_share16, data = election_2020_lpm_))
summary(lpm_2020)
coeftest(lpm_2020,vcov = vcovHC(lpm_2020, type="HC1"))

#Logistic regression
logistic <- glm(outcome ~ chg_medinc +
                  chg_unemp +
                  med_inc20 + unemp_rate20 +
                  college +
                  wht + Manufacturing +
                  life_exp + death_rate +
                  candidate_share16 +
                  as.factor(state_po),
                data = election_2020_lpm_,
                family = "binomial")
summary(logistic)
mfx <- logitmfx(outcome ~ chg_medinc +
                  chg_unemp +
                  med_inc20 + unemp_rate20 +
                  college +
                  wht + Manufacturing +
                  life_exp + death_rate +
                  candidate_share16 +
                  as.factor(state_po),
                data = election_2020_lpm_,
                robust = TRUE,
                atmean = FALSE)
mfx


#Out of Sample Prediction
samp_rows <- sample(1:nrow(election_2020_lpm),0.8*nrow(election_2020_lpm))
train <- election_2020_lpm %>%
  slice(samp_rows)
test <- election_2020_lpm %>%
  slice(-samp_rows)

train_mod <- lm(outcome ~ log(med_inc) +
                  college + unemp_rate +
                  wht + Manufacturing +
                  life_exp +
                  death_rate,
                data = train)
pred <- predict(train_mod, newdata = test)
test <- test %>%
  mutate(pred_prob = pred,
         pred_outcome = ifelse(pred_prob < 0.5, 0, 1),
         match = ifelse(pred_outcome == outcome, 1, 0)) %>%
  filter(is.na(pred_outcome) == F)
sum(test$match == 1)/nrow(test)

#Using 2016 county data to predict 2020 election
samp_rows <- sample(1:nrow(election_2016_lpm),0.8*nrow(election_2016_lpm))
train <- election_2016_lpm %>%
  slice(samp_rows)
test <- election_2020_lpm %>%
  sample_n(0.2*nrow(election_2020_lpm))
train_mod <- lm(outcome ~ log(med_inc) +
                  college + unemp_rate +
                  wht + Manufacturing +
                  life_exp,
                data = train)
pred <- predict(train_mod, newdata = test)
test <- test %>%
  mutate(pred_prob = pred,
         pred_outcome = ifelse(pred_prob < 0.5, 0, 1),
         match = ifelse(pred_outcome == outcome, 1, 0)) %>%
  filter(is.na(pred_outcome) == F)
sum(test$match == 1)/nrow(test)

# Tables
library(stargazer)
# Summary Stat Tables
stargazer(election_2016_lpm %>%
            dplyr::select(med_inc:wht,Manufacturing,life_exp,outcome),
          style = "aer",
          omit.summary.stat = c("p25","p75"),
          median = TRUE)
stargazer(election_2020_lpm %>%
            dplyr::select(med_inc:wht,Manufacturing,case_rate,
                          death_rate,outcome),
          style = "aer",
          omit.summary.stat = c("p25","p75"),
          median = TRUE)
# Regression tables
stargazer(lpm2016,lpm2020,title = "Baseline Model",align = TRUE,
          omit = "state_po")
stargazer(lpm_2020, both_mod, title = "Baseline with Additional Specifications",
          align = TRUE, omit = "state_po")
stargazer(lpm2016,lpm_2020,title = "Model Results", align = TRUE,
          omit = "state_po")

