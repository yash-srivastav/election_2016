setwd("/Users/yashsrivastav/Dropbox/Personal_Projects/Election_Analysis/Data")
#https://www.bls.gov/cew/downloadable-data-files.htm 

## (1) Import data and libraries
library(stringr);library(ggplot2); library(tidyr);library(readxl);library(datasets)
voting <- read.csv("countypres_2000-2020.csv")
med_inc <- read.csv("2015 Median Income by County.csv")
education <- read_excel("Education.xls")
unemp <- read_excel("Unemployment.xlsx")
ind_emp <- read_excel("emp_industry_15/allhlcn15.xlsx")
demographics <- read.csv("cc-est2019-alldata.csv")
life_expectancy <- read.csv("U.S._Life_Expectancy_at_Birth_by_State_and_Census_Tract_-_2010-2015.csv")
state <- state.name
## (2) Clean data
voting <- voting %>%
  group_by(year,state_po,county_fips,candidate) %>%
  mutate(totalcandvotes = sum(candidatevotes,na.rm = TRUE)) %>%
  ungroup() %>%
  dplyr::select(-candidatevotes) %>%
  mutate(mode = "TOTAL") %>%
  distinct()

adjcty <- voting %>%
  filter(year == 2020) %>%
  distinct(state_po,county_name, .keep_all = TRUE) %>%
  dplyr::select(county_name,county_fips)
  

election_2016 <- voting %>%
  filter(year == 2016,
         is.na(county_fips) == F) %>%
  left_join(adjcty, by = "county_fips") %>%
  dplyr::select(-county_name.x) %>%
  rename(county = county_name.y) %>%
  mutate(county_name = tolower(county))
rm(adjcty)

med_inc <- med_inc %>%
  mutate(county_name = tolower(County),
         county_name = str_replace_all(county_name," county","")) %>%
  relocate(county_name, .after = County)

educ_pct <- education %>%
  filter((`Area name` %in% state) == F,
         str_detect(`Area name`,"United States") == F) %>%
  dplyr::select(State,`Area name`,44:47) %>%
  mutate(county_name = tolower(`Area name`),
         county_name = str_replace_all(county_name," county","")) %>%
  relocate(county_name, .after = `Area name`)

unemp_15 <- unemp %>%
  filter((Area_name %in% state) == F,
         str_detect(Area_name,"United States") == F) %>%
  dplyr::select(2,3,70) %>%
  mutate(county_name = tolower(Area_name),
         county_name = str_replace_all(county_name," county,.+","")) %>%
  relocate(county_name, .after = Area_name)

demographics_16 <- demographics %>%
  filter(YEAR == 9) %>%
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
  relocate(county_name, .after = county) %>%
  dplyr::select(-state_po) %>%
  left_join((voting %>%
               distinct(state,state_po)),
            by = c("STNAME" = "state")) %>%
  relocate(c(STNAME,state_po),
           .before = county) %>%
  relocate(county_name,
           .after = county)

life_expectancy <- life_expectancy %>%
  filter(str_detect(County,"blank") == F) %>%
  mutate(state_po = str_extract(County,"(?<=, ).+")) %>%
  relocate(state_po,
           .after = County) %>%
  group_by(County,state_po) %>%
  summarise(life_expectancy = mean(Life.Expectancy, na.rm = TRUE)) %>%
  mutate(county_name = tolower(County),
         county_name = str_replace_all(county_name," county,.+",""),
         .after = County)
life_expectancy <- life_expectancy %>%
  filter(is.na(life_expectancy) == F,
         is.na(state_po) == F) %>%
  ungroup()

## (3) Merge data
election_2016 <- election_2016 %>% 
  left_join((med_inc %>%
               dplyr::select(State.Code,county_name,
                      Median.household.income)),
            by = c("county_name",
                   "state_po" = "State.Code")) %>%
  left_join(educ_pct,
            by = c("state_po" = "State",
                   "county_name")) %>%
  left_join(unemp_15,
            by = c("state_po" = "State",
                   "county_name")) %>%
  left_join((demographics_16 %>%
               dplyr::select(blk,wht,tot_pop,county_name,state_po)),
            by = c("state_po","county_name")) %>%
  left_join(ind_emp_county %>% 
              dplyr::select(-c(STNAME)),
            by = c("state_po","county_name")) %>%
  left_join(life_expectancy %>%
              dplyr::select(-County),
            by = c("state_po","county_name"))

## (4) Modify election data
election_2016 <- election_2016 %>%
  dplyr::select(-c(county_fips,version,mode,CTYNAME,county.x,
                   county_name,county.y,Area_name))
  
election_2016 <- election_2016 %>%
  mutate(candidate_share = totalcandvotes/totalvotes,
         partydum = case_when(party == "DEMOCRAT" ~ 1,
                              party == "REPUBLICAN" ~ 2,
                              party != "DEMOCRAT" & party != "REPUBLICAN" ~ 3 ))
election_2016 <- election_2016 %>%
  mutate(med_inc = as.numeric(Median.household.income)) %>%
  dplyr::select(-Median.household.income)
election_2016 <- election_2016 %>%
  rename(hs_less = 10,
         hs = 11,
         some_college = 12,
         college = 13)

election_2016 <- election_2016 %>%
  rename(unemp_rate = Unemployment_rate_2015,
         life_exp = life_expectancy,
         county = `Area name`) %>%
  relocate(candidate_share, .after = totalcandvotes) %>%
  relocate(county, .after = state_po) %>%
  relocate(med_inc, .before = hs_less) 

rm(educ_pct,unemp_15,demographics_16,ind_emp,
   ind_emp_county,med_inc,education,demographics,
   life_expectancy,unemp)

## (5) Creating cross sectional dataset
election_2016_lpm <- election_2016 %>%
  group_by(county,state_po) %>%
  mutate(outcome = ifelse(candidate_share == max(candidate_share),
                          1,
                          0))
election_2016_lpm <- election_2016_lpm %>%
  filter(partydum == 2)

election_2016_lpm[c(17:18,20:30)] <- lapply(election_2016_lpm[c(17:18,20:30)],
                                                  function(x) x*100)

# write.csv(election_2016,"Clean_Data/election_2016.csv",row.names = FALSE)
# write.csv(election_2016_lpm,"Clean_Data/election_2016_lpm.csv",row.names = FALSE)
# election_2016 <- read.csv("Clean_Data/election_2016.csv")
# election_2016_lpm <- read.csv("Clean_Data/election_2016_lpm.csv")

