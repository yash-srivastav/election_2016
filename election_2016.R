library(stringr);library(ggplot2); library(tidyr);library(readxl)
setwd("/Users/yashsrivastav/Dropbox/Personal Projects/Election_Analysis/Data")
#https://www.bls.gov/cew/downloadable-data-files.htm 
med_inc <- read.csv("2015 Median Income by County.csv")
voting <- read.csv("countypres_2000-2020.csv")
education <- read_excel("Education.xls")
unemp <- read_excel("Unemployment.xlsx")
ind_emp <- read_excel("emp_industry/allhlcn15.xlsx")
demographics <- read.csv("cc-est2019-alldata.csv")
life_expectancy <- read.csv("U.S._Life_Expectancy_at_Birth_by_State_and_Census_Tract_-_2010-2015.csv")

election_2016 <- voting %>%
  filter(year == 2016) %>%
  mutate(county_name = tolower(county_name))
med_inc <- med_inc %>%
  mutate(county_name = tolower(str_replace_all(County," County","")))
educ_pct <- education %>%
  select(2,3,44:47) %>%
  mutate(county_name = str_replace_all(`Area name`," County",""),
         county_name = tolower(county_name))
unemp_15 <- unemp %>%
  select(2,3,70) %>%
  mutate(county_name = str_replace_all(Area_name," County,.+",""),
         county_name = tolower(county_name))
demographics_16 <- demographics %>%
  filter(YEAR == 9) %>%
  group_by(CTYNAME,STNAME) %>%
  summarise(blk = (sum(BA_MALE,na.rm = TRUE) + sum(BA_FEMALE,na.rm = TRUE))/sum(TOT_POP,na.rm = TRUE),
            wht = (sum(WA_MALE,na.rm = TRUE) + sum(WA_FEMALE,na.rm = TRUE))/sum(TOT_POP,na.rm = TRUE)) %>%
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
  mutate(state_po = str_extract(County,"(?<=, ).+")) %>%
  relocate(state_po,
           .after = County) %>%
  group_by(County,state_po) %>%
  summarise(life_expectancy = mean(Life.Expectancy, na.rm = TRUE)) %>%
  mutate(county_name = str_replace_all(County," County,.+",""),
         county_name = tolower(county_name),
         .after = County)
life_expectancy <- life_expectancy %>%
  filter(is.na(life_expectancy) == F,
         is.na(state_po) == F) %>%
  ungroup()


election_2016 <- election_2016 %>% 
  left_join((med_inc %>%
               select(County,county_name,Population,
                      Median.household.income,State.Code)),
            by = c("county_name",
                   "state_po" = "State.Code")) %>%
  left_join(educ_pct,
            by = c("state_po" = "State",
                   "county_name")) %>%
  left_join(unemp_15,
            by = c("state_po" = "State",
                   "county_name")) %>%
  left_join((demographics_16 %>%
               select(blk,wht,county_name,state_po)),
            by = c("state_po","county_name")) %>%
  left_join(ind_emp_county %>% 
              select(-c(STNAME,cnty)),
            by = c("state_po","county_name")) %>%
  left_join(life_expectancy %>%
              select(-County),
            by = c("state_po","county_name"))

election_2016 <- election_2016 %>%
  select(-c(county_fips,version,mode))
election_2016 <- election_2016 %>%
  mutate(candidate_share = candidatevotes/totalvotes,
         partydum = case_when(party == "DEMOCRAT" ~ 1,
                              party == "REPUBLICAN" ~ 2,
                              party != "DEMOCRAT" & party != "REPUBLICAN" ~ 3 ))
election_2016 <- election_2016 %>%
  mutate(med_inc = as.numeric(Median.household.income)) %>%
  select(-Median.household.income)
election_2016 <- election_2016 %>%
  rename(hs_less = 13,
         hs = 14,
         some_college = 15,
         college = 16)
election_2016 <- election_2016 %>%
  select(-c(county_name,
            `Area name`,
            CTYNAME,
            Area_name)) %>%
  relocate(County,
           .after = state_po)
election_2016 <- election_2016 %>%
  mutate(hs_less = hs_less/100,
         hs = hs/100,
         some_college = some_college/100,
         college = college/100,
         Unemployment_rate_2015 = Unemployment_rate_2015/100)
rm(educ_pct,unemp_15,demographics_16,ind_emp,
   ind_emp_county,med_inc,education,demographics)

election_2016_rep <- election_2016 %>%
  filter(partydum == 2)
# ggplot(data = election_2016_rep, aes(x = log(med_inc),
#                                      y = candidate_share)) +
#   geom_point()

mod <- lm(candidate_share ~ log(med_inc) +
            college + Unemployment_rate_2015 +
            wht + Manufacturing + `Education and health services` +
            life_expectancy + as.factor(state_po),
          data = election_2016_rep)
summary(mod)

cor_mat <- cor(election_2016_rep %>%
                 select(hs_less,hs,some_college,
                        college,Unemployment_rate_2015,
                        wht,`Goods-producing`,
                        `Natural resources and mining`,
                        Construction,Manufacturing,
                        `Service-providing`,`Financial activities`,
                        `Education and health services`,
                        life_expectancy,candidate_share,
                        med_inc),
               use = "complete.obs")



