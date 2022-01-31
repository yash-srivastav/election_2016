setwd("/Users/yashsrivastav/Dropbox/Personal_Projects/Election_Analysis/Data")
#https://www.bls.gov/cew/downloadable-data-files.htm 
## (1) Import data
library(stringr);library(ggplot2); library(tidyr);library(readxl);
library(datasets)
voting <- read.csv("countypres_2000-2020.csv")
med_inc <- read_excel("Unemployment.xlsx")
education <- read_excel("Education.xls")
unemp <- read_excel("Unemployment.xlsx")
ind_emp <- read_excel("emp_industry_20/allhlcn20.xlsx")
demographics <- read.csv("cc-est2019-alldata.csv")
life_expectancy <- read_excel("life_expectancy_2019_2020.xlsx")
covid <- read.csv("us-counties-covid.csv")
state <- state.name

## (2) Clean data before aggregation
voting <- voting %>%
  group_by(year,state_po,county_fips,candidate) %>%
  mutate(totalcandvotes = sum(candidatevotes,na.rm = TRUE)) %>%
  ungroup() %>%
  select(-candidatevotes) %>%
  mutate(mode = "TOTAL") %>%
  distinct()

election_2020 <- voting %>%
  filter(year == 2020,
         is.na(county_fips) == F,
         ) %>%
  mutate(county_name = tolower(county_name))

med_inc <- med_inc %>%
  dplyr::select(2,3,91) %>%
  filter((Area_name %in% state) == F) %>%
  mutate(county_name = tolower(Area_name),
         county_name = str_replace_all(county_name," county,.+",""))

educ_pct <- education %>%
  filter((`Area name` %in% state) == F,
         str_detect(`Area name`,"United States") == F) %>%
  dplyr::select(State,`Area name`,44:47) %>%
  mutate(county_name = tolower(`Area name`),
         county_name = str_replace_all(county_name," county","")) %>%
  relocate(county_name, .after = `Area name`)

unemp_20 <- unemp %>%
  filter((Area_name %in% state) == F,
         str_detect(Area_name,"United States") == F) %>%
  dplyr::select(2,3,90) %>%
  mutate(county_name = tolower(Area_name),
         county_name = str_replace_all(county_name," county,.+",""))

demographics_19 <- demographics %>%
  filter(YEAR == 12) %>%
  group_by(CTYNAME,STNAME) %>%
  summarise(blk = (sum(BA_MALE,na.rm = TRUE) + sum(BA_FEMALE,na.rm = TRUE))/sum(TOT_POP,na.rm = TRUE),
            wht = (sum(WA_MALE,na.rm = TRUE) + sum(WA_FEMALE,na.rm = TRUE))/sum(TOT_POP,na.rm = TRUE),
            tot_pop = sum(TOT_POP,na.rm = TRUE)) %>%
  mutate(CTYNAME = iconv(CTYNAME,'UTF-8', 'ASCII'),
         county_name = tolower(CTYNAME),
         county_name = str_replace_all(county_name," county",""),
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
  filter(is.na(County) == F) %>%
  dplyr::select(2:4) %>%
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

## (3) Merge data
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

## (4) Modify county-level election data
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
         life_exp = `Life Expectancy`)
election_2020 <- election_2020 %>%
  relocate(county, .before = county_name) %>%
  relocate(c(case_rate,death_rate,med_inc), .before = hs_less) %>%
  relocate(c(cases,deaths,tot_pop), .before = case_rate) %>%
  relocate(candidate_share, .after = totalcandvotes)
rm(educ_pct,unemp_20,demographics,ind_emp,
   ind_emp_county,med_inc,education,demographics_19,covid,
   covid_nov,life_expectancy,unemp,voting)

## (5) Creating cross-sectional data 
election_2020_lpm <- election_2020 %>%
  group_by(county_name,state_po) %>%
  mutate(outcome = ifelse(candidate_share == max(candidate_share),
                          1,
                          0))
election_2020_lpm <- election_2020_lpm %>%
  filter(partydum == 2)
#scale all ratio variables by 100 for interpretation with LPM
election_2020_lpm[c(15,16,23:35)] <- lapply(election_2020_lpm[c(15,16,23:35)],
                                                  function(x) x*100)
# write.csv(election_2020,"Clean_Data/election_2020.csv",row.names = FALSE)
# write.csv(election_2020_lpm,"Clean_Data/election_2020_lpm.csv",row.names = FALSE)
# election_2020 <- read.csv("Clean_Data/election_2020.csv")
# election_2020_lpm <- read.csv("Clean_Data/election_2020_lpm.csv")

## (6) Additional data checks
# Examining county data to look for duplicates or county name changes
dst <- voting %>% 
  distinct(county_name,county_fips) %>%
  group_by(county_fips) %>%
  filter(n() > 1)
nrow(dst %>% distinct(county_fips))
dst1 <- election_2020_lpm %>%
  group_by(state_po,county_name) %>%
  filter(n() > 1)
