library(stringr);library(ggplot2); library(tidyr);library(readxl)
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

election_2020 <- voting %>%
  filter(year == 2020) %>%
  mutate(county_name = tolower(county_name))
med_inc <- med_inc %>%
  select(2,3,91) %>%
  mutate(county_name = tolower(str_replace_all(Area_name," County,.+","")))
educ_pct <- education %>%
  select(2,3,44:47) %>%
  mutate(county_name = str_replace_all(`Area name`," County",""),
         county_name = tolower(county_name))
unemp_20 <- unemp %>%
  select(2,3,90) %>%
  mutate(county_name = str_replace_all(Area_name," County,.+",""),
         county_name = tolower(county_name))
demographics_19 <- demographics %>%
  filter(YEAR == 12) %>%
  group_by(CTYNAME,STNAME) %>%
  summarise(blk = (sum(BA_MALE,na.rm = TRUE) + sum(BA_FEMALE,na.rm = TRUE))/sum(TOT_POP,na.rm = TRUE),
            wht = (sum(WA_MALE,na.rm = TRUE) + sum(WA_FEMALE,na.rm = TRUE))/sum(TOT_POP,na.rm = TRUE),
            tot_pop = sum(TOT_POP,na.rm = TRUE)) %>%
  mutate(county_name = str_replace_all(CTYNAME," County",""),
         county_name = tolower(county_name),
         STNAME = toupper(STNAME)) %>%
  left_join((voting %>%
               distinct(state,state_po)),
            by = c("STNAME" = "state"))
ind_emp_county <- ind_emp %>%
  rename(cnty = Area,
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
  select(state_po,cnty,Industry,empq) %>%
  pivot_wider(names_from = Industry,
              values_from = empq) %>%
  mutate(county_name = str_replace_all(cnty," County.+",""),
         county_name = tolower(county_name),
         STNAME = toupper(state_po)) %>%
  select(-state_po) %>%
  left_join((voting %>%
               distinct(state,state_po)),
            by = c("STNAME" = "state")) %>%
  relocate(c(STNAME,state_po),
           .before = cnty) %>%
  relocate(county_name,
           .after = cnty)



life_expectancy <- life_expectancy %>%
  select(2,3,4) %>%
  mutate(county_name = tolower(County),
         .after = County) %>%
  mutate(State = toupper(State)) %>%
  left_join(election_2020 %>% distinct(state,state_po),
            by = c("State" = "state"))

covid_nov <- covid %>%
  mutate(date = as.Date(date)) %>%
  filter(date == "2020-11-01")
covid_nov <- covid_nov %>%
  mutate(state = toupper(state),
         county_name = tolower(county)) %>%
  left_join(election_2020 %>% distinct(state,state_po),
            by = "state")


election_2020 <- election_2020 %>% 
  left_join((med_inc %>%
               select(State,Median_Household_Income_2019,
                      county_name)),
            by = c("county_name",
                   "state_po" = "State")) %>%
  left_join(educ_pct %>% select(-c(`Area name`)),
            by = c("state_po" = "State",
                   "county_name")) %>%
  left_join(unemp_20 %>% select(-Area_name),
            by = c("state_po" = "State",
                   "county_name")) %>%
  left_join((demographics_19 %>%
               select(blk,wht,county_name,state_po,
                      tot_pop)),
            by = c("state_po","county_name")) %>%
  left_join(ind_emp_county %>% 
              select(-c(STNAME,cnty)),
            by = c("state_po","county_name")) %>%
  left_join(life_expectancy %>%
              select(-c(State,County)),
            by = c("state_po","county_name")) %>%
  left_join(covid_nov %>%
              select(c(cases,deaths,county_name,state_po)),
            by = c("state_po","county_name"))

election_2020 <- election_2020 %>%
  select(-c(county_fips,version,mode))
election_2020 <- election_2020 %>%
  mutate(candidate_share = candidatevotes/totalvotes,
         partydum = case_when(party == "DEMOCRAT" ~ 1,
                              party == "REPUBLICAN" ~ 2,
                              party != "DEMOCRAT" & party != "REPUBLICAN" ~ 3 ),
         case_rate = cases/tot_pop,
         death_rate = deaths/tot_pop)
election_2020 <- election_2020 %>%
  mutate(med_inc = as.numeric(Median_Household_Income_2019)) %>%
  select(-Median_Household_Income_2019)
election_2020 <- election_2020 %>%
  rename(hs_less = 10,
         hs = 11,
         some_college = 12,
         college = 13)
election_2020 <- election_2020 %>%
  mutate(hs_less = hs_less/100,
         hs = hs/100,
         some_college = some_college/100,
         college = college/100,
         Unemployment_rate_2020 = Unemployment_rate_2020/100)
rm(educ_pct,unemp_20,demographics,ind_emp,
   ind_emp_county,med_inc,education,demographics_19,covid,
   covid_nov)

election_2020_rep <- election_2020 %>%
  filter(partydum == 2)
# ggplot(data = election_2016_rep, aes(x = log(med_inc),
#                                      y = candidate_share)) +
#   geom_point()

mod <- lm(candidate_share ~ log(med_inc) +
            college + Unemployment_rate_2020 +
            wht + Manufacturing +
            `Life Expectancy` + case_rate +
            as.factor(state_po),
          data = election_2020_rep)
summary(mod)

cor_mat <- cor(election_2020_rep %>%
                 select(hs_less,hs,some_college,
                        college,Unemployment_rate_2020,
                        wht,`Goods-producing`,
                        `Natural resources and mining`,
                        Construction,Manufacturing,
                        `Service-providing`,`Financial activities`,
                        `Education and health services`,
                        `Life Expectancy`,candidate_share,
                        med_inc),
               use = "complete.obs")

## Linear Probability Model
election_2020_lpm <- election_2020 %>%
  group_by(county_name,state_po) %>%
  mutate(outcome = ifelse(candidate_share >
                            (sum(candidate_share,na.rm = TRUE) - candidate_share),
                          1,
                          0))
election_2020_lpm <- election_2020_lpm %>%
  filter(partydum == 2)

lpm <- lm(outcome ~ log(med_inc) +
            college + Unemployment_rate_2020 +
            wht + Manufacturing +
            `Life Expectancy` + case_rate +
            as.factor(state_po),
          data = election_2020_lpm)
summary(lpm)

lm <- glm(outcome ~ log(med_inc) +
            college + Unemployment_rate_2020 +
            wht + Manufacturing +
            `Life Expectancy` + case_rate +
            as.factor(state_po),
          data = election_2020_lpm,
          family = "binomial")
summary(lm)
