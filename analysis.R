## (1) Importing data
library(mfx)
election_2016 <- read.csv("Clean_Data/election_2016.csv")
election_2016_lpm <- read.csv("Clean_Data/election_2016_lpm.csv")
election_2020 <- read.csv("Clean_Data/election_2020.csv")
election_2020_lpm <- read.csv("Clean_Data/election_2020_lpm.csv")

## (2) Data Exploration
# Correlation matrix
cor20 <- cor(election_2020_lpm %>%
                   select(college,unemp_rate,
                          wht,med_inc,Goods.producing,
                          Natural.resources.and.mining,
                          Construction,Manufacturing,
                          Education.and.health.services,
                          life_exp,candidate_share),
                 use = "complete.obs")

## (2) Analysis
# Linear probability model
lpm2016 <- lm(outcome ~ log(med_inc) +
                college + unemp_rate +
                wht + Manufacturing +
                life_exp + Education.and.health.services +
                as.factor(state_po),
              data = election_2016_lpm)
summary(lpm2016)
coeftest(lpm2016,vcov = vcovHC(lpm2016, type="HC1"))

lpm2020 <- lm(outcome ~ log(med_inc) +
                 college + unemp_rate +
                 wht + Manufacturing +
                 life_exp + case_rate +
                 Education.and.health.services +  
                 as.factor(state_po),
               data = election_2020_lpm)
summary(lpm2020)
coeftest(lpm2020,vcov = vcovHC(lpm2020, type="HC1"))

#Logistic regression
logistic <- glm(outcome ~ log(med_inc) +
                  college + unemp_rate +
                  wht + Manufacturing +
                  life_exp + case_rate +
                  as.factor(state_po),
                data = election_2020_lpm,
                family = "binomial")
summary(logistic)
logitmfx(outcome ~ log(med_inc) +
           college + unemp_rate +
           wht + Manufacturing +
           life_exp + case_rate +
           as.factor(state_po),
         data = election_2020_lpm,
         robust = TRUE)

# Looking at both 2020 and 2016 elections
both_elections <- election_2020_lpm %>%
  filter(is.na(county) == F) %>%
  left_join(election_2016_lpm %>%
              dplyr::select(state_po,county,candidate_share,outcome) %>%
              filter(is.na(county) == F),
            by = c("state_po","county")) %>%
  rename(candidate_share2016 = candidate_share.y,
         candidate_share2020 = candidate_share.x,
         outcome2016 = outcome.y,
         outcome2020 = outcome.x) %>%
  filter(partydum == 2)
both_mod <- lm(candidate_share2020 ~ log(med_inc) +
                 college + unemp_rate +
                 wht + Manufacturing +
                 life_exp + case_rate +
                 candidate_share2016 +
                 as.factor(state_po),
               data = both_elections)
summary(both_mod)

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
              select(state_po,county,biden20),
            by = c("state_po","county"))

# Try a model where we include candidate share from previous election as predictor
# of current election
election_2020_lpm <- election_2020_lpm %>%
  left_join(election_2016_lpm %>%
              distinct(state_po,county_name,candidate_share),
            by = c("state_po","county_name")) %>%
  rename(candidate_share2016 = candidate_share.y)

lpm_2020 <- lm(outcome ~ log(med_inc) +
                 college + unemp_rate +
                 wht + Manufacturing +
                 life_exp + case_rate +
                 `Education and health services` +
                 as.factor(state_po),
               data = election_2020_lpm)
summary(lpm_2020)

# Tables
library(stargazer)
stargazer(as.data.frame(election_2016_lpm[,c(32,14:15,18,23,29,31,34)]))
stargazer(lpm2016,lpm2020,title = "Model Results",align = TRUE,
          omit = "state_po")
