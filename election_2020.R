library(stringr);library(ggplot2); library(tidyr);library(readxl);library(mfx)
setwd("/Users/yashsrivastav/Dropbox/Personal_Projects/Election_Analysis/Data")
#https://www.bls.gov/cew/downloadable-data-files.htm 
med_inc <- read_excel("Unemployment.xlsx")
voting <- read.csv("countypres_2000-2020.csv")
education <- read_excel("Education.xls")
unemp <- read_excel("Unemployment.xlsx")
ind_emp <- read_excel("emp_industry_20/allhlcn20.xlsx")
demographics <- read.csv("cc-est2019-alldata.csv")
life_expectancy <- read_excel("life_expectancy_2019_2020.xlsx")
covid <- read.csv("us-counties-covid.csv")

voting <- voting %>%
  group_by(year,state_po,county_fips,candidate) %>%
  mutate(totalcandvotes = sum(candidatevotes,na.rm = TRUE)) %>%
  ungroup() %>%
  select(-candidatevotes) %>%
  mutate(mode = "TOTAL") %>%
  distinct()

election_2020 <- voting %>%
  filter(year == 2020) %>%
  mutate(county_name = tolower(county_name),
         county_name = str_replace_all(county_name,"( city)$",""))
med_inc <- med_inc %>%
  dplyr::select(2,3,91) %>%
  mutate(county_name = tolower(Area_name),
         county_name = str_replace_all(county_name," county,.+",""),
         county_name = str_replace_all(county_name," city,.+",""))
educ_pct <- education %>%
  dplyr::select(State,`Area name`,44:47) %>%
  mutate(county_name = tolower(`Area name`),
         county_name = str_replace_all(county_name," county",""),
         county_name = str_replace_all(county_name,"( city)$","")) %>%
  relocate(county_name, .after = `Area name`)
unemp_20 <- unemp %>%
  dplyr::select(2,3,90) %>%
  mutate(county_name = tolower(Area_name),
         county_name = str_replace_all(county_name," county,.+",""),
         county_name = str_replace_all(county_name," city,.+",""))
demographics_19 <- demographics %>%
  filter(YEAR == 12) %>%
  group_by(CTYNAME,STNAME) %>%
  summarise(blk = (sum(BA_MALE,na.rm = TRUE) + sum(BA_FEMALE,na.rm = TRUE))/sum(TOT_POP,na.rm = TRUE),
            wht = (sum(WA_MALE,na.rm = TRUE) + sum(WA_FEMALE,na.rm = TRUE))/sum(TOT_POP,na.rm = TRUE),
            tot_pop = sum(TOT_POP,na.rm = TRUE)) %>%
  mutate(CTYNAME = iconv(CTYNAME,'UTF-8', 'ASCII'),
         county_name = tolower(CTYNAME),
         county_name = str_replace_all(county_name," county",""),
         county_name = str_replace_all(county_name,"( city)$",""),
         STNAME = toupper(STNAME)) %>%
  relocate(county_name, .after = CTYNAME) %>%
  left_join((voting %>%
               distinct(state,state_po)),
            by = c("STNAME" = "state"))
ind_emp_county <- ind_emp %>%
  rename(county = Area,
         empq = `Employment Location Quotient Relative to U.S.`,
         state_po = `St Name`) %>%
  mutate(Industry = str_replace_all(Industry,"^(\\d+ )","")) %>%
  filter(Industry == "Goods-producing" |
           Industry == "Natural resources and mining" |
           Industry == "Construction" |
           Industry == "Manufacturing" |
           Industry == "Service-providing" |
           Industry == "Trade, transportation, and utilities" |
           Industry == "Information" |
           Industry == "Financial activities" |
           Industry == "Professional and business services" |
           Industry == "Education and health services" |
           Industry == "Leisure and hospitality",
         `Area Type` == "County") %>%
  dplyr::select(state_po,county,Industry,empq) %>%
  pivot_wider(names_from = Industry,
              values_from = empq) %>%
  mutate(county_name = tolower(county),
         county_name = str_replace_all(county_name," county,.+",""),
         county_name = str_replace_all(county_name," city,.+",""),
         STNAME = toupper(state_po)) %>%
  dplyr::select(-state_po) %>%
  left_join((voting %>%
               distinct(state,state_po)),
            by = c("STNAME" = "state")) %>%
  relocate(c(STNAME,state_po),
           .before = county) %>%
  relocate(county_name,
           .after = county)

life_expectancy <- life_expectancy %>%
  dplyr::select(2,3,4) %>%
  mutate(county_name = tolower(County),
         county_name = str_replace_all(county_name,"( city)$",""),
         .after = County) %>%
  mutate(State = toupper(State)) %>%
  left_join(election_2020 %>% distinct(state,state_po),
            by = c("State" = "state"))

covid_nov <- covid %>%
  mutate(date = as.Date(date)) %>%
  filter(date == "2020-11-01")
covid_nov <- covid_nov %>%
  mutate(state = toupper(state),
         county_name = str_replace_all(county,"( city)$",""),
         county_name = tolower(county_name)) %>%
  left_join(election_2020 %>% distinct(state,state_po),
            by = "state")


election_2020 <- election_2020 %>% 
  left_join((med_inc %>%
               dplyr::select(State,Median_Household_Income_2019,
                      county_name)),
            by = c("county_name",
                   "state_po" = "State")) %>%
  left_join(educ_pct %>% dplyr::select(-c(`Area name`)),
            by = c("state_po" = "State",
                   "county_name")) %>%
  left_join(unemp_20 %>% select(-Area_name),
            by = c("state_po" = "State",
                   "county_name")) %>%
  left_join((demographics_19 %>%
               dplyr::select(blk,wht,county_name,state_po,
                      tot_pop)),
            by = c("state_po","county_name")) %>%
  left_join(ind_emp_county %>% 
              dplyr::select(-c(STNAME,county)),
            by = c("state_po","county_name")) %>%
  left_join(life_expectancy %>%
              dplyr::select(-c(State,County)),
            by = c("state_po","county_name")) %>%
  left_join(covid_nov %>%
              dplyr::select(c(cases,deaths,county_name,state_po)),
            by = c("state_po","county_name"))

election_2020 <- election_2020 %>%
  dplyr::select(-c(county_fips,version,mode))
election_2020 <- election_2020 %>%
  mutate(candidate_share = totalcandvotes/totalvotes,
         partydum = case_when(party == "DEMOCRAT" ~ 1,
                              party == "REPUBLICAN" ~ 2,
                              party != "DEMOCRAT" & party != "REPUBLICAN" ~ 3 ),
         case_rate = cases/tot_pop,
         death_rate = deaths/tot_pop)
election_2020 <- election_2020 %>%
  mutate(med_inc = as.numeric(Median_Household_Income_2019)) %>%
  dplyr::select(-Median_Household_Income_2019)
election_2020 <- election_2020 %>%
  rename(hs_less = 10,
         hs = 11,
         some_college = 12,
         college = 13)
election_2020 <- election_2020 %>%
  rename(county = CTYNAME,
         unemp_rate = Unemployment_rate_2020,
         mfg = Manufacturing,
         life_exp = Life.Expectancy)
rm(educ_pct,unemp_20,demographics,ind_emp,
   ind_emp_county,med_inc,education,demographics_19,covid,
   covid_nov)

## Linear Probability Model
election_2020_lpm <- election_2020 %>%
  group_by(county_name,state_po) %>%
  mutate(outcome = ifelse(candidate_share == max(candidate_share),
                          1,
                          0))
election_2020_lpm <- election_2020_lpm %>%
  filter(partydum == 2)
#scale all ratio variables by 100
election_2020_lpm[c(18:19,21:31,36:37)] <- lapply(election_2020_lpm[c(18:19,21:31,36:37)],
                                                  function(x) x*100)
lpm_2020 <- lm(outcome ~ log(med_inc) +
            college + unemp_rate +
            wht + mfg +
            life_exp + case_rate +
            Education.and.health.services +  
            as.factor(state_po),
          data = election_2020_lpm)
summary(lpm_2020)

logistic <- glm(outcome ~ log(med_inc) +
            college + unemp_rate +
            wht + mfg +
            life_exp + case_rate +
            as.factor(state_po),
          data = election_2020_lpm,
          family = "binomial")
summary(logistic)

logitmfx(outcome ~ log(med_inc) +
           college + unemp_rate +
           wht + mfg +
           life_exp + case_rate +
           as.factor(state_po),
         data = election_2020_lpm,
         robust = TRUE)

## Looking at both 2020 and 2016 elections (need to run election_2016.R first)
both_elections <- election_2020 %>%
  left_join(election_2016 %>%
              dplyr::select(state_po,county_name,party,candidate_share),
            by = c("state_po","county_name","party")) %>%
  rename(candidate_share2016 = candidate_share.y,
         candidate_share2020 = candidate_share.x) %>%
  filter(partydum == 2)
both_mod <- lm(candidate_share2020 ~ log(med_inc) +
                 college + Unemployment_rate_2020 +
                 wht + Manufacturing +
                 `Life Expectancy` + case_rate +
                 candidate_share2016 +
                 as.factor(state_po),
               data = both_elections)
summary(both_mod)

# write.csv(election_2020,"Clean_Data/election_2020.csv")
# write.csv(election_2020_lpm,"Clean_Data/election_2020_lpm.csv")
# election_2020 <- read.csv("Clean_Data/election_2020.csv")
# election_2020_lpm <- read.csv("Clean_Data/election_2020_lpm.csv")

## Tables
library(stargazer)
stargazer(as.data.frame(election_2020_lpm[,c(11,15:16,19,24,30,32,36:38)]))


## Additional investigation
stpct20 <- election_2020_lpm %>%
  group_by(state) %>%
  summarise(trump_support = mean(outcome,na.rm = TRUE))

#correlation matrix
cor_mat20 <- cor(election_2020_lpm %>%
                 select(college,Unemployment_rate_2020,
                        wht,med_inc,Goods.producing,
                        Natural.resources.and.mining,
                        Construction,Manufacturing,
                        Education.and.health.services,
                        Life.Expectancy,candidate_share),
               use = "complete.obs")
