#Getting estimates for worldwide COVID deaths as of November 2020
covid_data <- read_excel("owid-covid-data.xlsx")
deaths_2020 <- covid_data %>%
  filter(date <= "2020-11-01") %>%
  group_by(location) %>%
  summarise(deaths = sum(new_deaths,na.rm = TRUE))
