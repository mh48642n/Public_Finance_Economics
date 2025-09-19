rm(list = ls())

library(tidyverse)
library(readxl)
library(zoo)
library(lubridate)
library(xts)

setwd("C:/Users/marvi/OneDrive/Documents/GitHub/Data/macro_datasets")

piece_1 <- read.csv("personal_income.csv")
piece_2 <- read_xls("acm_term_premium.xls", sheet = "Daily")
piece_3 <- read.csv("govt_receipts.csv")
piece_4 <- read.csv("yields_datasets.csv")
piece_5 <- read.csv("public_debt_fred.csv")
piece_6 <- read.csv("bea_gdp_govt.csv")
piece_7 <- read.csv("auction_data.csv") 
piece_8 <- read.csv("workers_data.csv")
piece_9 <- read.csv("private_fixed_investment.csv")

#personal_income
piece_1 <- piece_1 %>%
  mutate(dates = as.Date(dates, format = "%m/%d/%Y")) %>%
  select(2:4, 29, 31, 36, 42) %>%
  mutate_at(2:7, ~ str_replace(., ",", "")) %>%
  mutate(across(where(is.character), ~parse_number(.))) %>%
  rename(
    personal_inc = Personal.income, employee_comp = Compensation.of.employees,
    dis_personal_inc = Equals..Disposable.personal.income, pce = Personal.consumption.expenditures,
    personal_saving = Equals..Personal.saving, population = Population..midperiod..thousands.
  ) 

#acm_premiums
piece_2 <-  piece_2 %>% 
  mutate(dates = as.Date(dates, format = "%d-%B-%Y")) %>%
  select(1, 12:13, 16, 18, 21) 
  
#adding dates to acm datasets cause dates are missing
date_sets <- data.frame(seq(as.Date("1961-06-14"), as.Date("2025-04-14"), by = "day"))
date_sets <- date_sets %>%
  rename(dates = seq.as.Date..1961.06.14....as.Date..2025.04.14....by....day..)

piece_2 <- left_join(date_sets, piece_2, by= "dates")

piece_2 <- piece_2 %>% 
  fill(2:6, .direction = "down")

#govt receipts
piece_3 <-  piece_3 %>% 
  mutate(dates = as.Date(dates, format = "%m/%d/%Y")) %>%
  select(c(2,4)) %>%
  mutate_at(2, ~ str_replace(., ",", "")) %>%
  mutate(across(where(is.character), ~parse_number(.))) %>%
  rename(current_taxes = Current.tax.receipts)

#yields data
piece_4 <- piece_4 %>%
  mutate(dates = as.Date(dates, format = "%m/%d/%Y")) %>%
  select(c(2:3,7,9,11))%>%
  mutate_at(2:5, ~ str_replace(., ",", "")) %>%
  mutate(across(where(is.character), ~parse_number(.))) %>%
  fill(2:5, .direction = "down")
  
#public debt data
piece_5 <- piece_5 %>%
  mutate(dates = as.Date(dates)) %>%
  rename(public_debt = GFDEBTN) %>%
  select("dates", "public_debt")

#spending function parts: Investment and Government spending
piece_6 <- piece_6 %>% 
  mutate(dates = as.Date(dates, format = "%m/%d/%Y")) %>%
  select(2:3, 9, 20:21) %>%
  rename(gdp = Gross.domestic.product, domestic_investment = Gross.private.domestic.investment, 
         fed_govt_spending = Federal, govt_spending = Government.consumption.expenditures.and.gross.investment)%>%
  mutate_at(2:4, ~ str_replace(., ",", "")) %>%
  mutate(across(where(is.character), ~parse_number(.)))


# These next couple of mutates are all for the aution data
piece_7 <- piece_7 %>%
  mutate(issue_date = ifelse((issue_date == original_issue_date) | (original_issue_date == ""), issue_date, original_issue_date),
         security_term = ifelse((security_term == original_security_term) | (original_security_term == ""), security_term, original_security_term)) %>% 
  select("issue_date", "maturity_date", "security_type", 
         "security_term", "offering_amt", "high_discnt_rate", 
         "high_investment_rate", "low_discnt_rate", "low_investment_rate") %>%
  filter(security_type %in% c("Bond", "Bill", "Note"))  %>%
  mutate(issue_date = as.Date(issue_date, format = "%m/%d/%Y"),
         maturity_date = as.Date(maturity_date, format = "%m/%d/%Y"),
         issue_quarter = as.character(quarter(issue_date)),
         maturity_qrtr = as.character(quarter(maturity_date)),
         year_issue = as.character(year(issue_date)),
         year_maturity = as.character(year(maturity_date)),
         ) %>% 
  arrange(issue_date)

piece_7 <- piece_7 %>%
  mutate(dates = ceiling_date(as.Date(as.yearqtr(paste0(year_issue, " Q", issue_quarter)), 
          format("%Y Q%q"), frac = 0), "quarter")) %>%
  select("dates", "security_type") %>%
  group_by(dates, security_type) %>% 
  summarize(count_securities = n()) %>%
  pivot_wider(names_from = security_type,
              values_from = count_securities) %>%
  mutate_at(2:4, ~replace(., is.na(.), 0)) 
  
piece_8 <- piece_8 %>%
  mutate(dates = as.Date(dates)) %>%
  select(-c(1))

piece_9 <- piece_9 %>%
  mutate(dates = as.Date(dates)) %>%
  select(-c(1)) %>%
  mutate_at(2:29, ~ str_replace(., ",", "")) %>%
  mutate(across(where(is.character), ~parse_number(.))) %>%
  select(c(1,3)) %>%
  rename(firm_capital = Nonresidential)

#frames <- c(piece_3, piece_4, piece_5)

#join first and that handle NANs
other <- left_join(piece_3, piece_1, join_by(dates == dates))
other <- left_join(other, piece_2, join_by(dates == dates))
other <- left_join(other, piece_4)
other <- left_join(other, piece_6)
other <- left_join(other, piece_5)
other <- left_join(other, piece_7)
other <- left_join(other, piece_8)
other <- left_join(other, piece_9)

setwd("~/GitHub/Data/master_thesis_data")

write.csv(other, "base_data.csv", row.names = FALSE)

corp_A <- read.csv("~/GitHub/Data/macro_datasets/corporate_income.csv")
corp_B <- read.csv("~/GitHub/Data/macro_datasets/corporate_incomeA.csv")
corp_C <- read.csv("~/GitHub/Data/macro_datasets/corporate_incomeB.csv")


columns <- function(x){
  x <- x %>%
    select("dates", "Corporate.profits.with.inventory.valuation.and.capital.consumption.adjustments",
            "Domestic.industries", "Financial", "Nonfinancial", 
                 "Corporate.profits.with.inventory.valuation.adjustment")
  return(x)
}

corp_A$dates <- as.Date(corp_A$dates, format = "%m/%d/%Y")
corp_B$dates <- as.Date(corp_B$dates, format = "%m/%d/%Y")
corp_C$dates <- as.Date(corp_C$dates, format = "%m/%d/%Y")
    
corp_A <- columns(corp_A)
corp_B <- columns(corp_B)
corp_C <- columns(corp_C)

corps <- merge(corp_A, corp_B, all = TRUE)
corps <- merge(corps, corp_C, all = TRUE)

corps <- corps %>% 
  mutate_at(2:6, ~str_replace(., ",", "")) %>%
  mutate(across(where(is.character), ~parse_number(.))) 
  
write.csv(corps, "corporate_profits_join.csv", row.names = FALSE)

#other[,c(2:35)] <- apply(other[, c(2:35)], 1, pct_change)

#dates <- seq(as.Date("1994-01-01"), as.Date("2024-10-01"), by = "quarter")
#other <- apply(frame[,-c(1, 20)], 2, convert) 
#adjusted <- frame %>% 
#  mutate_at(c(7:17, 21:25), ~.*conversion)



#below we write csvs for real dollar values of stationary and nonstationary datasets

#nominal non-stationary

#non-stationary
#write.csv(adjusted, "non_stationary.csv", row.names = FALSE)

#stationary
#write.csv(st_adjust, "stationary.csv", row.names = FALSE)






