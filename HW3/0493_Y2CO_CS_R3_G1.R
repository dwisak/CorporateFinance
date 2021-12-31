library(simfinapi)
library(dplyr)
library(ggplot2)
library(reshape2)
library(Rcpp)
library(magrittr)
library(plotly)
library(tidyquant)

setwd("/home/max/WU/CorporateFinance/CorporateFinance/HW3")
rm(list = ls())
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

# getting daily prices for COP
df.COP = tq_get("COP", get='stock.prices',
                from="2014-01-01", to="2021-12-31")
write.csv(df.COP,file = "~/WU/CorporateFinance/CorporateFinance/HW3/InputFiles/COP_daily_prices.csv")

# computing weekly returns for COP
df.COP.weekly = df.COP %>% tq_transmute(select="adjusted",
                                        mutate_fun=periodReturn, 
                                        period="weekly", 
                                        type="arithmetic")


# loading the Fama-French data
df.FF = read.csv("~/WU/CorporateFinance/CorporateFinance/HW3/InputFiles/F-F_Research_Data_Factors_weekly.csv",
                 skip = 4) %>% as_tibble()
# rename colnames
colnames(df.FF)[1] = "date"

# transform date column
df.FF$date = as.Date(as.character(df.FF$date),format="%Y%m%d")

# cut the rows of df.FF until 2014-01-03 (first date in df.COP.weekly)
df.FF = df.FF %>% filter(between(date, as.Date("2014-01-03"),as.Date("2021-12-31")))

# cut the rows of df.COP.weekly since the FF data is only available until 2021-10-29
df.COP.weekly = df.COP.weekly %>% filter(between(date, as.Date("2014-01-03"),as.Date("2021-10-29")))

# compute excess return for COP by using the rf of FF data 
df.FF$COP.weekly = df.COP.weekly$weekly.returns 
df.FF$COP.weekly_m_RF = (df.FF$COP.weekly - df.FF$RF) * 100

# rolling weekly regression
df.regression.results = rollRegres::roll_regres(data = df.FF,
                        formula = "COP.weekly_m_RF ~ Mkt.RF + SMB + HML",
                        width = 104)$coef %>% as_tibble

# add the date column to the new data frame
df.regression.results$date = df.FF$date

# filter the data for the timeframe in the question (Ex. 12)
df.regression.results = df.regression.results %>% 
  filter(between(date, as.Date("2017-01-01"),as.Date("2021-12-31")))

# write the data frame to the output directory
write.csv(df.regression.results,file = "~/WU/CorporateFinance/CorporateFinance/HW3/OutputFiles/Ex12_regression_results.csv")

##### EX 13 #####

# computing the one factor beta (CAPM)
df.regression.results2 = rollRegres::roll_regres(data = df.FF,
                                                formula = "COP.weekly_m_RF ~ Mkt.RF",
                                                width = 104)$coef %>% as_tibble

# add the date column to the new data frame
df.regression.results2$date = df.FF$date

# filter the data for the timeframe in the question (Ex. 12)
df.regression.results2 = df.regression.results2 %>% 
  filter(between(date, as.Date("2017-01-01"),as.Date("2021-12-31")))

# write the data frame to the output directory
write.csv(df.regression.results2, file = "~/WU/CorporateFinance/CorporateFinance/HW3/OutputFiles/Ex13_regression_results.csv")

plot(df.regression.results$Mkt.RF, x=df.regression.results$date, type = "l", ylim=c(0, 2.1))
lines(df.regression.results2$Mkt.RF, x=df.regression.results2$date, col="red")

##### EX 15 #####

# tickers: EOG, APA, MRO, MUR, PXD

TICKERS = c('COP', 'EOG', 'APA', 'MRO', 'MUR', 'PXD')


get_market_cap = function(TICKER, date){
  
  SMFIN_ID = entities %>% filter(ticker==TICKER) %>% select(simfin_id) %>% as.numeric()
  # get price time series for the ticker - to compute market cap 
  df.prices = simfinapi::sfa_get_prices(TICKER, SMFIN_ID)
  
  # compute market cap for given TICKER and date
  market_cap = df.prices$adj_close[df.prices$date %in% date] *
    df.prices$common_shares_outstanding[df.prices$date %in% date]
  
  return(market_cap)
}

get_assetbeta = function(TICKER, year=2021, quarter="q3", lookback_weeks=104, debt_beta=0.1){
  
  # get the simfin id
  SMFIN_ID = entities %>% filter(ticker==TICKER) %>% select(simfin_id) %>% as.numeric()
  
  # get balance sheet data to compute total debt
  df.bs.quarterly = simfinapi::sfa_get_statement(
    ticker = TICKER,
    simfin_id = SMFIN_ID,
    statement = "bs",
    period = quarter,
    fyear = year,
  )
  
  # compute the total debt (current_portion_of_long_term_debt is 0 for all quarters)
  book_value_of_debt =  df.bs.quarterly$long_term_debt + df.bs.quarterly$short_term_debt + 
                ifelse(is.na(df.bs.quarterly$current_portion_of_long_term_debt),
                       0, df.bs.quarterly$current_portion_of_long_term_debt)
  
  # get price time series for the ticker - to compute market cap 
  df.prices = simfinapi::sfa_get_prices(TICKER, SMFIN_ID)
  
  # compute the market cap for TICKER and date
  market_cap = get_market_cap(TICKER, df.bs.quarterly$report_date)
  
  # --- computing the equity beta ---
  
  # getting the price data for the stock
  df.TICKER = tq_get(TICKER, get='stock.prices',
                  from="2020-01-01", to="2021-12-31")
  
  # getting the price data for the benchmark
  df.SPY = tq_get("SPY", get='stock.prices',
                     from="2020-01-01", to="2021-12-31")
  
  # saving the data in the inputfile dir
  write.csv(df.TICKER,file = paste0(c("~/WU/CorporateFinance/CorporateFinance/HW3/InputFiles/",TICKER,"_daily_prices.csv"), collapse = ""))
  write.csv(df.SPY,file = "~/WU/CorporateFinance/CorporateFinance/HW3/InputFiles/SPY_daily_prices.csv")
  
  # compute the returns (weekly) for the stock
  df.returns = df.TICKER %>% tq_transmute(select="adjusted",
                                          mutate_fun=periodReturn, 
                                          period="weekly", 
                                          type="arithmetic")
  
  # compute the returns (weekly) for the benchmark
  df.SPY.weekly = df.SPY %>% tq_transmute(select="adjusted",
                                          mutate_fun=periodReturn, 
                                          period="weekly", 
                                          type="arithmetic")
  
  equity_beta = cov(df.returns$weekly.returns, df.SPY.weekly$weekly.returns) /
    var(df.SPY.weekly$weekly.returns)
  
  # --- computing the asset (unlevered) beta ---
  
  asset_beta = market_cap * equity_beta / (market_cap + book_value_of_debt) +
         book_value_of_debt * debt_beta / (market_cap + book_value_of_debt)
  
  attr(asset_beta, "label") = NULL

  return(asset_beta)
  
}

# create the debt beta table - the source s&p500 capital-iq -
df.debt_beta = tibble(company=TICKERS, debt_beta=c(0.05, 0.05, 0.17, 0.1, 0.17, 0.1))

# create empty data frame for results
df.results_EX15 = tibble(company=TICKERS, asset_beta=NA)


# compute all asset betas
asset_beta = sapply(TICKERS, function(TICKER){
  get_assetbeta(TICKER, debt_beta=df.debt_beta$debt_beta[df.debt_beta$company == TICKER])})


df.results_EX15$asset_beta = asset_beta
write.csv(df.results_EX15,file = "~/WU/CorporateFinance/CorporateFinance/HW3/OutputFiles/EX15_asset_betas.csv")



#### EX 14 ####

df.results_EX14 = df.results_EX15 %>% summarise(peer_asset_beta=mean(asset_beta))
write.csv(df.results_EX14,file = "~/WU/CorporateFinance/CorporateFinance/HW3/OutputFiles/EX14_peer_asset_betas.csv")




