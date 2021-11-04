library(simfinapi)
library(dplyr)

WD = getwd()

# source the API KEY https://simfin.com/
source("ConstantVariables.R")

simfinapi::sfa_set_api_key(api_key = API_KEY)
simfinapi::sfa_set_cache_dir(path=paste0(WD,"/responses"), create = TRUE)

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
        statement = "bs",
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




# create data frames for yearly and quarterly statements
df.bs.yearly = get_fin_data(statement='bs', years=2015:2020,
                            quarters=c("q1", "q2", "q3", "q4"),
                            ticker=TICKER, simfin_id=SMFIN_ID,
                            quarterly=FALSE)

df.bs.quarterly = get_fin_data(statement='bs', years=2015:2020,
                            quarters=c("q1", "q2", "q3", "q4"),
                            ticker=TICKER, simfin_id=SMFIN_ID,
                            quarterly=TRUE)

df.cf.yearly = get_fin_data(statement='cf', years=2015:2020,
                            quarters=c("q1", "q2", "q3", "q4"),
                            ticker=TICKER, simfin_id=SMFIN_ID,
                            quarterly=FALSE)

df.cf.quarterly = get_fin_data(statement='cf', years=2015:2020,
                            quarters=c("q1", "q2", "q3", "q4"),
                            ticker=TICKER, simfin_id=SMFIN_ID,
                            quarterly=TRUE)

df.cf.yearly = get_fin_data(statement='pl', years=2015:2020,
                            quarters=c("q1", "q2", "q3", "q4"),
                            ticker=TICKER, simfin_id=SMFIN_ID,
                            quarterly=FALSE)

df.cf.quarterly = get_fin_data(statement='pl', years=2015:2020,
                               quarters=c("q1", "q2", "q3", "q4"),
                               ticker=TICKER, simfin_id=SMFIN_ID,
                               quarterly=TRUE)



#### Ex 1 ####
# Compute the Book and Market leverage on a yearly and quarterly basis for your firm for the last 6 years back from 12/31/2020.

# set all NAs to zero - this makes it easier to work out the numbers - doubble check with Yahoo Finance
df.bs.yearly[is.na(df.bs.yearly)] = 0

# book leverage = total debt (long term + short term debt) / (total assets - total debt)
book_leverage.yearly = (df.bs.yearly$long_term_debt + df.bs.yearly$short_term_debt) / (df.bs.yearly$total_assets -
     (df.bs.yearly$long_term_debt + df.bs.yearly$short_term_debt)) 

attributes(book_leverage.yearly)$label = 'book_leverage yearly'

book_leverage.quarterly = (df.bs.quarterly$long_term_debt + df.bs.quarterly$short_term_debt) / (df.bs.quarterly$total_assets -
                                                                                               (df.bs.quarterly$long_term_debt + df.bs.quarterly$short_term_debt)) 

attributes(book_leverage.quarterly)$label = 'book_leverage quarterly'


# get price time series for the ticker - to compute market cap 
df.prices = simfinapi::sfa_get_prices(TICKER, SMFIN_ID)

# get the correct dates for the report filing - s.t. we can filter the price df
price_dates = c("2015-12-31","2016-12-30", "2017-12-29",
                "2018-12-31", "2019-12-31", "2020-12-31")

# convert to string in order to apply %in% filter
df.prices$date = df.prices$date %>% as.character()

# compute the market cap yearly
market_cap.yearly = df.prices %>% filter(df.prices$date %in% price_dates) %>% 
  mutate(market_cap=common_shares_outstanding* adj_close) %>%
  select(market_cap) %>% c

# get the correct dates for the report filing - s.t. we can filter the price df
price_dates = c("2015-03-31", "2015-06-30", "2015-09-30", "2015-12-31",
                "2016-03-31", "2016-06-30", "2016-09-30", "2016-12-30",
                "2017-03-31", "2017-06-30", "2017-09-29", "2017-12-29",
                "2018-03-29", "2018-06-29", "2018-09-28", "2018-12-31",
                "2019-03-29", "2019-06-28", "2019-09-30", "2019-12-31", 
                "2020-03-31", "2020-06-30", "2020-09-30", "2020-12-31")

# compute the market cap quarterly
market_cap.quarterly = df.prices %>% filter(df.prices$date %in% price_dates) %>% 
  mutate(market_cap=common_shares_outstanding* adj_close) %>%
  select(market_cap) %>% c

# compute the market leverage - yearly
market_leverage.yearly = (df.bs.yearly$long_term_debt + df.bs.yearly$short_term_debt)/
  market_cap.yearly$market_cap

attributes(market_leverage.yearly)$label = "market leverage yearly"

# compute the market leverage - quarterly
market_leverage.quarterly = (df.bs.quarterly$long_term_debt + df.bs.quarterly$short_term_debt)/
  market_cap.quarterly$market_cap

attributes(market_leverage.quarterly)$label = "market leverage quarterly"



