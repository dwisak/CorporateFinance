library(simfinapi)
library(dplyr)
library(ggplot2)
library(reshape)
library(reshape2)
library(Rcpp)
library(magrittr)
library(plotly)
library(kableExtra)
library(viridis)
library(tidyquant)

setwd("/home/max/WU/CorporateFinance/CorporateFinance/HW2")

WD = getwd()
# source the API KEY https://simfin.com/

source("/home/max/WU/CorporateFinance/CorporateFinance/HW1/ConstantVariables.R")

# Own API Key from 'https://simfin.com/login'
#API_KEY <- "fMCZMXOSNa1WMr9lGTlOKCUXiJjNm7rT"
simfinapi::sfa_set_api_key(api_key = API_KEY)
simfinapi::sfa_set_cache_dir(path=paste0(WD,"/responses"), create = TRUE)

# 'Rcpp' package needed
entities = simfinapi::sfa_get_entities()

TICKER = 'COP'

SMFIN_ID = entities %>% filter(ticker==TICKER) %>% select(simfin_id) %>% as.numeric()


# helper:
get_fin_data = function(statement='bs',years=2015:2020, quarters=c("q1", "q2", "q3", "q4"),
                        ticker='COP', simfin_id=378120, quarterly=FALSE){
  
  # if quarterly data should be fetched >>>
  if(quarterly){
    # create empty data frame
    df.bs.quarterly = data.frame()
    
    for (year in years){
      for (quarter in quarters){
        # fetch the data form the api
        df.row = simfinapi::sfa_get_statement(
          ticker = ticker,
          simfin_id = simfin_id,
          statement = statement,
          period = quarter,
          fyear = year,
        )
        
        # row bind the data frame with the returned row
        df.bs.quarterly = rbind(df.bs.quarterly, df.row)
      }
    }
    
    # create time column
    df.bs.quarterly$time = sapply(years, FUN = function(year) paste0(year, quarters)) %>% c
    
    return(df.bs.quarterly)
    
  }else{
    # if not quarterly data should be fetched >>>
    df.bs.yearly = data.frame()
    
    for (year in years){
      
      # fetch the data form the api
      df.row = simfinapi::sfa_get_statement(
        ticker = ticker,
        simfin_id = simfin_id,
        statement = statement,
        period = "fy",
        fyear = year,
      )
      
      # row bind the data frame with the returned row
      df.bs.yearly = rbind(df.bs.yearly, df.row)
      
    }
    
    # create time column
    df.bs.yearly$time = years
    
    return(df.bs.yearly)
    
  }
  
}


df.COP = tq_get("COP", get='stock.prices',
                from="2011-01-01", to="2020-12-31")



df.SP500 = tq_get("^GSPC", get='stock.prices',
                  from="2011-01-01", to="2020-12-31")




# why use arithmetic returns instead of log returns for beta:
# https://www.researchgate.net/post/What_are_the_pros_and_cons_of_different_methods_to_calculate_asset_returns

# The arithmetic mean (average) is the best estimate of future expected returns
#under a strong assumption. It makes the implicit assumption that each equity secu-
#  rity return is an independent observation from a stationary underlying probability
#distribution. Under the independence assumption, then the arithmetic average, when
#compounded over many periods, is the one that gives the expected value (i.e., the
#mean) of the probability distribution of expected ending values. The assumption of
#The assumption of independence is not empirically supported for equity stock market returns. Studies by
# Fama and French,11 Lo and MacKinlay,12 and Porterba and Summers,13 indicate the
# existence of some long-term negative autocorrelation in stock returns. Put differently,
# negative autocorrelation means that stock prices may overreact in the short term and
# that they will eventually correct (revert to the mean) over longer time periods.

df.COP.daily = df.COP %>% tq_transmute(select="adjusted",
                                           mutate_fun=periodReturn, 
                                           period="daily", 
                                           type="arithmetic")

df.COP.weekly = df.COP %>% tq_transmute(select="adjusted",
                                       mutate_fun=periodReturn, 
                                       period="weekly", 
                                       type="arithmetic")

df.COP.monthly = df.COP %>% tq_transmute(select="adjusted",
                                       mutate_fun=periodReturn, 
                                       period="monthly", 
                                       type="arithmetic")


df.SP500.daily = df.SP500 %>% tq_transmute(select="adjusted",
                                                   mutate_fun=periodReturn, 
                                                   period="daily", 
                                                   type="arithmetic")

df.SP500.weekly = df.SP500 %>% tq_transmute(select="adjusted",
                                                    mutate_fun=periodReturn, 
                                                    period="weekly", 
                                                    type="arithmetic") 

df.SP500.monthly = df.SP500 %>% tq_transmute(select="adjusted",
                                                     mutate_fun=periodReturn, 
                                                     period="monthly", 
                                                     type="arithmetic") 




# daily beta
beta.daily = runCov(df.COP.daily$daily.returns,
                    df.SP500.daily$daily.returns, n=200) /
  runVar(df.SP500.daily$daily.returns, n=200)

# weekly beta
beta.weekly = runCov(df.COP.weekly$weekly.returns,
                     df.SP500.weekly$weekly.returns, n=104) /
  runVar(df.SP500.weekly$weekly.returns, n=104)

# monthly beta
beta.monthly = runCov(df.COP.monthly$monthly.returns,
                      df.SP500.monthly$monthly.returns, n=60) /
  runVar(df.SP500.monthly$monthly.returns, n=60)

df.COP.daily$beta = beta.daily
df.COP.weekly$beta = beta.weekly
df.COP.monthly$beta = beta.monthly


#df.COP.daily = df.COP.daily %>%
#  filter(between(date, as.Date("2016-01-01"),as.Date("2020-12-31")))

#df.COP.weekly = df.COP.weekly %>%
#  filter(between(date, as.Date("2016-01-01"),as.Date("2020-12-31")))

#df.COP.monthly = df.COP.monthly %>%
#  filter(between(date, as.Date("2016-01-01"),as.Date("2020-12-31")))

# ---- Write the data frame into output ---
#write.csv(df.COP.daily, file = "OutputFiles/COP_beta_daily.csv")
#write.csv(df.COP.weekly, file = "OutputFiles/COP_beta_weekly.csv")
#write.csv(df.COP.monthly, file = "OutputFiles/COP_beta_monthly.csv")



# Another approach to computing the beta is by fitting a linear regression 
# both approaches are valid

df.riskfree = data.frame(date=df.SP500$date)


df.riskfree.monthly = tq_get("TB3MS", get='economic.data',
                     from="2011-01-01", to="2020-12-31",
                     period="monthly")

# covert percentage to values
df.riskfree.monthly$price = df.riskfree.monthly$price / 100 

# create a "last-riskfree rate" data frame
df.riskfree.daily = df.riskfree %>% left_join(df.riskfree.monthly, by="date")
df.riskfree.daily = df.riskfree.daily %>%  tidyr::fill(everything(df.riskfree.daily), 
                                                       .direction = "down")
colnames(df.riskfree.daily)[3] = "risk_free_rate"


# sp500 - daily
df.SP500.daily = df.SP500.daily %>% left_join(df.riskfree.daily, by="date") 
df.SP500.daily$excess_return = df.SP500.daily$daily.returns - df.SP500.daily$risk_free_rate
df.regression.daily = df.SP500.daily %>% left_join(df.COP.daily, by="date") 
df.regression.daily$excess_return.y = df.regression.daily$daily.returns.y - df.regression.daily$risk_free_rate 

# sp500 - weekly
df.SP500.weekly = df.SP500.weekly %>% left_join(df.riskfree.daily, by="date") 
df.SP500.weekly$excess_return = df.SP500.weekly$weekly.returns - df.SP500.weekly$risk_free_rate
df.regression.weekly = df.SP500.weekly %>% left_join(df.COP.weekly, by="date") 
df.regression.weekly$excess_return.y = df.regression.weekly$weekly.returns.y - df.regression.weekly$risk_free_rate 

# sp500 - monthly
df.SP500.monthly = df.SP500.monthly %>% left_join(df.riskfree.daily, by="date") 
df.SP500.monthly$excess_return = df.SP500.monthly$monthly.returns - df.SP500.monthly$risk_free_rate
df.regression.monthly = df.SP500.monthly %>% left_join(df.COP.monthly, by="date") 
df.regression.monthly$excess_return.y = df.regression.monthly$monthly.returns.y - df.regression.monthly$risk_free_rate 


df.regression.daily = df.regression.daily %>% select(!beta) %>% na.omit()
df.regression.daily$regression.beta = rollRegres::roll_regres(data = df.regression.daily,
                                                formula = "excess_return.y ~ excess_return",
                                                width = 200)$coef[,2]
df.regression.daily = df.regression.daily %>% 
  filter(between(date, as.Date("2016-01-01"),as.Date("2020-12-31"))) %>% 
  select(date,regression.beta)
  

df.regression.weekly= df.regression.weekly %>% select(!beta) %>% na.omit()
df.regression.weekly$regression.beta = rollRegres::roll_regres(data = df.regression.weekly,
                                                              formula = "excess_return.y ~ excess_return",
                                                              width = 104)$coef[,2]
df.regression.weekly = df.regression.weekly %>% 
  filter(between(date, as.Date("2016-01-01"),as.Date("2020-12-31"))) %>% 
  select(date, regression.beta)


df.regression.monthly= df.regression.monthly %>% select(!beta) %>% na.omit()
df.regression.monthly$regression.beta = rollRegres::roll_regres(data = df.regression.monthly,
                                                               formula = "excess_return.y ~ excess_return",
                                                               width = 60)$coef[,2]

df.regression.monthly = df.regression.monthly %>% 
  filter(between(date, as.Date("2016-01-01"),as.Date("2020-12-31"))) %>% 
  select(date, regression.beta)

# merge beta data frames - this is kinda sloppy 
df.daily_tmp = read.csv("OutputFiles/COP_beta_daily.csv")
df.daily_tmp$date = df.daily_tmp$date %>% as.Date()
df.regression.daily = df.regression.daily %>% 
  left_join(df.daily_tmp, by="date")

rm(df.daily_tmp)
  
#write.csv(df.regression.daily %>% select(!X), file = "OutputFiles/COP_beta_daily.csv")

df.weekly_tmp = read.csv("OutputFiles/COP_beta_weekly.csv")
df.weekly_tmp$date = df.weekly_tmp$date %>% as.Date()
df.regression.weekly = df.regression.weekly %>% 
  left_join(df.weekly_tmp, by="date")

rm(df.weekly_tmp)

#write.csv(df.regression.weekly %>% select(!X), file = "OutputFiles/COP_beta_weekly.csv")


df.monthly_tmp = read.csv("OutputFiles/COP_beta_monthly.csv")
df.monthly_tmp$date = df.monthly_tmp$date %>% as.Date()
df.regression.monthly = df.regression.monthly %>% 
  left_join(df.monthly_tmp, by="date")

rm(df.monthly_tmp)
#write.csv(df.regression.monthly %>% select(!X), file = "OutputFiles/COP_beta_monthly.csv")

### Ex. 3 ####

# 3.1 Asset (unlevered) Beta:  E/(E+D) * beta_equity + D/(E+D) * beta_debt

#load("~/WU/CorporateFinance/CorporateFinance/.RData")


# filter the weekly betas for the report dates
# -> we take the weekly betas from exercise 1

price_dates = c("2016-03-31", "2016-06-30", "2016-09-30", "2016-12-30",
                "2017-03-31", "2017-06-30", "2017-09-29", "2017-12-29",
                "2018-03-29", "2018-06-29", "2018-09-28", "2018-12-31",
                "2019-03-29", "2019-06-28", "2019-09-30", "2019-12-31", 
                "2020-03-31", "2020-06-30", "2020-09-30", "2020-12-31") %>% as.Date()


df.results.quarterly$equity_beta = c(NA,NA,NA,NA,
                                     df.COP.weekly$beta[c(12,25,39,52,
                                                   65,78,91,104,
                                                   117,130,143,156,
                                                   169,182,195,208,
                                                   221,234,247,261)])


# [HARD CODE] the rating from the table in the book 12.3
df.results.quarterly$debt_rating = c("A-", "A-", "A-", "A-",
                                     rep("A", 20))
df.results.quarterly$debt_beta = ifelse(df.results.quarterly$debt_rating == "A", 0.05, 0.1)

# compute the total debt (current_portion_of_long_term_debt is 0 for all quarters)
df.results.quarterly$total_debt =  df.bs.quarterly$long_term_debt + df.bs.quarterly$short_term_debt

# for completeness add market cap in the df for the quarterly results
df.results.quarterly$market_cap = market_cap.quarterly$market_cap


####Asset or unlevered betas: E/(E+D)*beta_equity+D/(E+D)*beta_debt
df.results.quarterly$beta_unlevered = df.results.quarterly$market_cap * df.results.quarterly$equity_beta/ (
  df.results.quarterly$market_cap + df.results.quarterly$total_debt) +
  df.results.quarterly$total_debt * df.results.quarterly$debt_beta / (
    df.results.quarterly$market_cap + df.results.quarterly$total_debt)



write.csv(df.results.quarterly, file = "OutputFiles/COP_quarterly_results.csv")





