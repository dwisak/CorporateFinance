library(simfinapi)
library(dplyr)
library(ggplot2)
library(reshape2)
library(Rcpp)
library(magrittr)
library(plotly)
library(tidyquant)

rm(list = ls())
main_wd <- getwd()
WD <- main_wd

# source the API KEY https://simfin.com/

source(paste(main_wd, "/HW1/ConstantVariables.R", sep = ""))

# Own API Key from 'https://simfin.com/login'
API_KEY <- "fMCZMXOSNa1WMr9lGTlOKCUXiJjNm7rT"
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
write.csv(df.COP,file = "COP_daily_prices.csv")

# computing weekly returns for COP
df.COP.weekly = df.COP %>% tq_transmute(select="adjusted",
                                        mutate_fun=periodReturn, 
                                        period="weekly", 
                                        type="arithmetic")

##### EX 12 #####


# loading the Fama-French data
df.FF = read.csv(paste(WD, "/InputFiles/F-F_Research_Data_Factors_weekly.csv", sep = ""),
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

# rolling weekly regression (linear model over a rolling window of 104 weeks)
df.regression.results = rollRegres::roll_regres(data = df.FF,
                        formula = "COP.weekly_m_RF ~ Mkt.RF + SMB + HML",
                        width = 104)$coef %>% as_tibble

# add the date column to the new data frame
df.regression.results$date = df.FF$date

# filter the data for the timeframe in the question (Ex. 12)
df.regression.results = df.regression.results %>% 
  filter(between(date, as.Date("2017-01-01"),as.Date("2021-12-31")))

# write the data frame to the output directory
write.csv(df.regression.results,file = "Ex12_regression_results.csv")

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
write.csv(df.regression.results2, file = "Ex13_regression_results.csv")

plot(df.regression.results$Mkt.RF, x=df.regression.results$date, type = "l", ylim=c(0, 2.1))
lines(df.regression.results2$Mkt.RF, x=df.regression.results2$date, col="red")

##### EX 15 #####

# Peer group: Firms 1-6
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

get_book_value_of_debt = function(TICKER, year=2021, quarter="q3"){
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
  
  book_value_of_debt
}

get_report_date = function(TICKER, year=2021, quarter="q3"){
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
  
  df.bs.quarterly$report_date
  
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
  write.csv(df.TICKER,file = paste0(c(TICKER,"_daily_prices.csv"), collapse = ""))
  write.csv(df.SPY,file = "SPY_daily_prices.csv")
  
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
write.csv(df.results_EX15,file = "EX15_asset_betas.csv")



#### EX 14 ####

df.results_EX14 = df.results_EX15 %>% summarise(peer_asset_beta=mean(asset_beta))
write.csv(df.results_EX14,file = "EX14_peer_asset_betas.csv")


#### Ex 16 ####

#Hint: you have to relever the equity beta according to the actual leverage of 
#your firm. Take the debt beta according to the youngest debt rating

# Asset beta from exercise 15 for COP
COP_beta <- df.results_EX15$asset_beta[df.results_EX15$company == "COP"]

# Expected market return of 8%
mr <- 0.08

# 10 Year US-Treasury Yield (https://fred.stlouisfed.org/series/DGS10/)
US_10year_yield <- read.csv(paste(WD, "/InputFiles/DGS10.csv", sep = ""))

# Risk free rate (Taken on 2021-12-31 to be consistent with exercise)
rf <- as.numeric(US_10year_yield$DGS10[US_10year_yield$DATE == "2021-12-31"])/100

# book value of debt
book_value_of_debt = get_book_value_of_debt("COP")

# market cap
market_cap = get_market_cap("COP", date = get_report_date("COP"))

# most recent debt beta computed in ex 15
debt_beta <- df.debt_beta$debt_beta[df.debt_beta$company == "COP"]

# unlevered (asset) beta from ex. 15
asset_beta['COP']

# levered (equity) beta = beta_u + D/E (beta_u-beta_d)
equity_beta = asset_beta['COP'] + book_value_of_debt/market_cap * (asset_beta['COP'] - debt_beta)


COP_exp_return <- rf + equity_beta * (mr - rf)


#### 17 ####




#### 17 ####


#expected dividend per share / (cost of equity - dividend growth rate )
df.prices = simfinapi::sfa_get_prices(TICKER, SMFIN_ID)

dat<-data.frame(date=df.prices$date , dividneds= df.prices$dividend ) %>% na.omit() %>% filter(date>as.Date("2017-01-01")) %>% head(-1)
plot(xts(dat$dividneds, order.by = dat$date), main="Historical Dividends per Share")


########possible options to estimate expected dividends
#Dividend policy (when available)
#Historical dividend growth rate
#Management's profit forecasts
#Recent dividend payout ratios
#Current economic conditions


#quartely divindeds are paid for COP
#SEC filing:
#An increase in the company's quarterly ordinary dividend from 43 cents per share to 46 cents per share, representing a 
#~7% increase and a current dividend yield of 3%. The dividend is payable on Dec. 1, 2021, to stockholders of record at the close of business on Oct. 28, 2021.
#from 2017 to 2021 the average annual increase in dividends was 9 - 10 % p.a. 
#Since in the past increase in dividends was approximately linear, we deem the constant dividend growth model to be an appropriate tool.

dividends_2022<-rep(0.46,4)
dividends_2023<-rep(0.50,4)
dividends_2024<-rep(0.54,4)


Div1 <- c(sum(dividends_2022), sum(dividends_2023), sum(dividends_2024))
g1 <- mean(diff(Div1)/Div1[-1])

### P_0:                                 
sum(dividends_2022/(1+COP_exp_return_eq)^(seq(0.25, 1 ,by=0.25)))+ sum(dividends_2023/(1+COP_exp_return_eq)^(seq(1.25, 2 ,by=0.25)))+sum(dividends_2024/(1+COP_exp_return_eq)^(seq(2.25, 3 ,by=0.25))) +  
  sum(dividends_2024/ (COP_exp_return_eq -g1)/(1+COP_exp_return_eq)^3)


#### 19 ####
mr #expexted market return
tau<- 0.21 #tax rate
COP_exp_return_dbt <-rf  + debt_beta *(mr -rf ) #capm with debt beta
market_cap_all<-sapply( TICKERS, get_market_cap ,date = as.Date("2021-09-30")) 
debt_all <- mean(sapply( TICKERS,  get_book_value_of_debt )) 

#we are supposed to calculate the arithmetic mean of the debt to value ratio of our 6 firms for this task
equity_ratio<- mean(debt_all/(market_cap_all+debt_all))
debt_ratio<- 1- equity_ratio

wacc <- ((COP_exp_return_eq) * equity_ratio + (COP_exp_return_dbt)* debt_ratio * (1-tau))
wacc


### 18 ###
#https://www.macrotrends.net/stocks/charts/COP/conocophillips/free-cash-flow
#https://static.conocophillips.com/files/resources/transaction-announcement-and-market-update.pdf
#https://www.macrotrends.net/stocks/charts/COP/conocophillips/free-cash-flow
#$10 B increase in FCF June 2021 however now is Jan 2022
#quartely report doesnt contain any forcasting data
#final press release 6 Dec 2021 contained information about the investment plan of ConocoP
#key infos:
#planned companywide 2022 capital expenditures of~$7.2 billion
#Expected 2022 annual average production of ~1.8MMBOED, representing low single-digit percentage underlying growth versus pro forma 2021;
#Expected 2022 return of capital to shareholders of ~$7 billion
#https://www.marketscreener.com/quote/stock/CONOCOPHILLIPS-13929/financials/

#see slides
#from 2018 to 2023
FCF<- c( 6184000000	,4468000000	,87000000,	10738000000	,12332000000	,9602000000)
FCF_in_Mio<-FCF/1000000
names(FCF_in_Mio)<- 2018:2023
barplot(FCF_in_Mio, ylab="FCF in Mio")

FCF_in_Mio_2021<-FCF_in_Mio[4]
FCF_in_Mio_2022<-FCF_in_Mio[5]
FCF_in_Mio_2023<-FCF_in_Mio[6]
FCF_in_Mio_Inf<-(FCF_in_Mio_2023)/(wacc-rf)

V0 <- (FCF_in_Mio_2022)/(1+wacc) + (FCF_in_Mio_2023)/(1+wacc)^2 +FCF_in_Mio_Inf 
shares_outsanding<- df.prices %>% select(common_shares_outstanding) %>% tail(1)
V0/(shares_outsanding/1000000)



