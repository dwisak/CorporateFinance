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


df.COP.daily = df.COP.daily %>%
  filter(between(date, as.Date("2016-01-01"),as.Date("2020-12-31")))

df.COP.weekly = df.COP.weekly %>%
  filter(between(date, as.Date("2016-01-01"),as.Date("2020-12-31")))

df.COP.monthly = df.COP.monthly %>%
  filter(between(date, as.Date("2016-01-01"),as.Date("2020-12-31")))

write.csv(df.COP.daily, file = "OutputFiles/COP_beta_daily.csv")
write.csv(df.COP.weekly, file = "OutputFiles/COP_beta_weekly.csv")
write.csv(df.COP.monthly, file = "OutputFiles/COP_beta_monthly.csv")

