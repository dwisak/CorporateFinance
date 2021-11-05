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

df.pl.yearly = get_fin_data(statement='pl', years=2015:2020,
                            quarters=c("q1", "q2", "q3", "q4"),
                            ticker=TICKER, simfin_id=SMFIN_ID,
                            quarterly=FALSE)

df.pl.quarterly = get_fin_data(statement='pl', years=2015:2020,
                               quarters=c("q1", "q2", "q3", "q4"),
                               ticker=TICKER, simfin_id=SMFIN_ID,
                               quarterly=TRUE)

df.pl.yearly = get_fin_data(statement='pl', years=2015:2020,
                            quarters=c("q1", "q2", "q3", "q4"),
                            ticker=TICKER, simfin_id=SMFIN_ID,
                            quarterly=FALSE)

df.pl.quarterly = get_fin_data(statement='pl', years=2015:2020,
                               quarters=c("q1", "q2", "q3", "q4"),
                               ticker=TICKER, simfin_id=SMFIN_ID,
                               quarterly=TRUE)


df.derived.yearly = get_fin_data(statement='derived', years=2015:2020,
                            quarters=c("q1", "q2", "q3", "q4"),
                            ticker=TICKER, simfin_id=SMFIN_ID,
                            quarterly=FALSE)

df.derived.quarterly = get_fin_data(statement='derived', years=2015:2020,
                               quarters=c("q1", "q2", "q3", "q4"),
                               ticker=TICKER, simfin_id=SMFIN_ID,
                               quarterly=TRUE)
#### EX 1 ####

# empty data frames for the results
df.results.yearly = data.frame(time=2015:2020)
df.results.quarterly = data.frame(time=sapply(2015:2020, FUN = function(year) paste0(year, quarters)) %>% c)


# set all NAs to zero - this makes it easier to work out the numbers - doubble check with Yahoo Finance
df.bs.yearly[is.na(df.bs.yearly)] = 0
df.bs.quarterly[is.na(df.bs.quarterly)] = 0
df.cf.yearly[is.na(df.cf.yearly)] = 0
df.cf.quarterly[is.na(df.cf.quarterly)] = 0

# ---- BOOK LEVERAGE ----
# book leverage = total debt (long term + short term debt) / (total assets - total debt)

# --- book_leverage: yearly ---
book_leverage.yearly = (df.bs.yearly$long_term_debt + df.bs.yearly$short_term_debt) / (df.bs.yearly$total_assets -
     (df.bs.yearly$long_term_debt + df.bs.yearly$short_term_debt)) 

# change label
attributes(book_leverage.yearly)$label = 'book_leverage yearly'

# add to results data frame
df.results.yearly$book_leverage = book_leverage.yearly

# --- book_leverage: quarterly ---
book_leverage.quarterly = (df.bs.quarterly$long_term_debt + df.bs.quarterly$short_term_debt) / (df.bs.quarterly$total_assets -
                                                                                               (df.bs.quarterly$long_term_debt + df.bs.quarterly$short_term_debt)) 
# change label
attributes(book_leverage.quarterly)$label = 'book_leverage quarterly'

# add to results data frame
df.results.quarterly$book_leverage = book_leverage.quarterly


# ---- MARKET LEVERAGE ----

# get price time series for the ticker - to compute market cap 
df.prices = simfinapi::sfa_get_prices(TICKER, SMFIN_ID)


# --- market_leverage: yearly ---
# get the correct dates for the report filing - s.t. we can filter the price data frame
price_dates = c("2015-12-31","2016-12-30", "2017-12-29",
                "2018-12-31", "2019-12-31", "2020-12-31")

# convert to string in order to apply %in% filter
df.prices$date = df.prices$date %>% as.character()

# compute the market cap yearly
market_cap.yearly = df.prices %>% filter(df.prices$date %in% price_dates) %>% 
  mutate(market_cap=common_shares_outstanding* adj_close) %>%
  select(market_cap) %>% c

# compute the market leverage - yearly
market_leverage.yearly = (df.bs.yearly$long_term_debt + df.bs.yearly$short_term_debt)/
  market_cap.yearly$market_cap

# change label
attributes(market_leverage.yearly)$label = "market leverage yearly"

# add to results data frame
df.results.yearly$market_leverage = market_leverage.yearly

# --- market_leverage: quarterly ---
# get the correct dates for the report filing - s.t. we can filter the price data frame
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

# compute the market leverage - quarterly
market_leverage.quarterly = (df.bs.quarterly$long_term_debt + df.bs.quarterly$short_term_debt)/
  market_cap.quarterly$market_cap

# change label
attributes(market_leverage.quarterly)$label = "market leverage quarterly"

# add to results data frame
df.results.quarterly$market_leverage = market_leverage.quarterly


#### EX 2 ####

#Gross margin can also be called gross profit margin, which is gross profit divided by net sales.
df.results.yearly$gross_profit_margin = df.pl.yearly$gross_profit / df.pl.yearly$revenue
df.results.quarterly$gross_profit_margin = df.pl.quarterly$gross_profit / df.pl.quarterly$revenue

# Net profit margin = net incom / revenue
df.results.yearly$net_profit_margin = df.pl.yearly$net_income /df.pl.yearly$revenue
df.results.quarterly$net_profit_margin = df.pl.quarterly$net_income /df.pl.quarterly$revenue



# NAs in preferred dividends https://www.investopedia.com/terms/p/price-earningsratio.asp; 
# https://www.investopedia.com/terms/e/eps.asp >> so we exclude them 
df.pl.yearly$preferred_dividends
df.pl.quarterly$preferred_dividends

# we compute market cap / net incume 1_{net_income > 0}
df.results.yearly$price_to_earnings_ratio = ifelse(market_cap.yearly$market_cap / df.pl.yearly$net_income > 0, market_cap.yearly$market_cap / df.pl.yearly$net_income, 0) 
df.results.quarterly$price_to_earnings_ratio = ifelse(market_cap.quarterly$market_cap / df.pl.quarterly$net_income > 0, market_cap.quarterly$market_cap / df.pl.quarterly$net_income, 0) 

# market to book ratio = market cap / net book value 
# https://corporatefinanceinstitute.com/resources/knowledge/valuation/market-to-book-ratio-price-book/
# where net book value = total assets - total debt
# could also just compute market_leverage / book_leverage 

# -- market_to_book: yearly --
book_equity.yearly = (df.bs.yearly$total_assets -
    (df.bs.yearly$long_term_debt + df.bs.yearly$short_term_debt))

df.results.yearly$market_to_book_ratio = market_cap.yearly$market_cap / book_equity.yearly

# -- market_to_book: quarterly --
book_equity.quarterly = (df.bs.quarterly$total_assets -
                        (df.bs.quarterly$long_term_debt + df.bs.quarterly$short_term_debt))

df.results.quarterly$market_to_book_ratio = market_cap.quarterly$market_cap / book_equity.quarterly

# -- interest coverage rate: yearly --
# https://www.investopedia.com/terms/i/interestcoverageratio.asp

# net interest expense = interest expense

ebit.yearly = df.pl.yearly$pretax_income_loss + df.pl.yearly$interest_expense_net

df.results.yearly$interest_coverage = ebit.yearly / df.pl.yearly$interest_expense_net

# -- interest coverage rate: quarterly --
ebit.quarterly = df.pl.quarterly$pretax_income_loss + df.pl.quarterly$interest_expense_net

df.results.quarterly$interest_coverage = ebit.quarterly / df.pl.quarterly$interest_expense_net


# -- return on equity: yearly --
df.results.yearly$return_on_equity = df.pl.yearly$net_income / df.bs.yearly$total_equity

# -- return on equity: quarterly --
df.results.quarterly$return_on_equity = df.pl.quarterly$net_income / df.bs.quarterly$total_equity

# -- return on assets: yearly --
df.results.yearly$return_on_assets = df.pl.yearly$net_income / df.bs.yearly$total_assets

# -- return on assets: quarterly --
df.results.quarterly$return_on_assets = df.pl.quarterly$net_income / df.bs.quarterly$total_assets

# --- cash from operating activities ---
# should be this: df.cf.yearly$net_cash_from_operating_activities
# https://www.investopedia.com/terms/c/cashflowfinvestingactivities.asp

# -- cash from operating activities: yearly --
df.results.yearly$cash_from_operating_activities = df.cf.yearly$net_income_starting_line + df.cf.yearly$other_adjustments + df.cf.yearly$depreciation_amortization +
  df.cf.yearly$non_cash_items + df.cf.yearly$stock_based_compensation + df.cf.yearly$deferred_income_taxes +
  df.cf.yearly$other_non_cash_adjustments + df.cf.yearly$change_in_working_capital + df.cf.yearly$change_in_accounts_receivable +
  df.cf.yearly$change_in_inventories + df.cf.yearly$change_in_accounts_payable + df.cf.yearly$change_in_other

# -- cash from operating activities: quarterly --
df.results.quarterly$cash_from_operating_activities = df.cf.quarterly$net_income_starting_line + df.cf.quarterly$other_adjustments + df.cf.quarterly$depreciation_amortization +
  df.cf.quarterly$non_cash_items + df.cf.quarterly$stock_based_compensation + df.cf.quarterly$deferred_income_taxes +
  df.cf.quarterly$other_non_cash_adjustments + df.cf.quarterly$change_in_working_capital + df.cf.quarterly$change_in_accounts_receivable +
  df.cf.quarterly$change_in_inventories + df.cf.quarterly$change_in_accounts_payable + df.cf.quarterly$change_in_other

# --- cash from financing activities ---
# should be this: df.cf.yearly$net_cash_from_financing_activities
# https://www.investopedia.com/terms/c/cashflowfromfinancing.asp

# -- cash from financing activities: yearly --
df.results.yearly$cash_from_financing_activities = df.cf.yearly$dividends_paid + df.cf.yearly$other_financing_activities + df.cf.yearly$repayments_of_long_term_debt + 
  df.cf.yearly$cash_from_repurchase_of_equity + df.cf.yearly$cash_from_repayment_of_debt

# -- cash from financing activities: quarterly --
df.results.quarterly$cash_from_financing_activities = df.cf.quarterly$dividends_paid + df.cf.quarterly$other_financing_activities + df.cf.quarterly$repayments_of_long_term_debt + 
  df.cf.quarterly$cash_from_repurchase_of_equity + df.cf.quarterly$cash_from_repayment_of_debt

# --- cash from financing activities ---
# should bet this: df.cf.yearly$net_cash_from_investing_activities
# https://www.investopedia.com/terms/c/cashflowfinvestingactivities.asp

# -- cash from investing activities: yearly --
df.results.yearly$cash_from_investing_activities = df.cf.yearly$change_in_fixed_assets_intangibles + df.cf.yearly$disposition_of_fixed_assets_intangibles +df.cf.yearly$disposition_of_fixed_assets +
  df.cf.yearly$disposition_of_intangible_assets + df.cf.yearly$acquisition_of_fixed_assets_intangibles + df.cf.yearly$purchase_of_fixed_assets +
  df.cf.yearly$acquisition_of_intangible_assets + df.cf.yearly$other_change_in_fixed_assets_intangibles + df.cf.yearly$net_change_in_long_term_investment +
  df.cf.yearly$decrease_in_long_term_investment + df.cf.yearly$increase_in_long_term_investment + df.cf.yearly$net_cash_from_acquisitions_divestitures +
  df.cf.yearly$net_cash_from_divestitures + df.cf.yearly$cash_for_acquisition_of_subsidiaries + df.cf.yearly$cash_for_joint_ventures +
  df.cf.yearly$net_cash_from_other_acquisitions + df.cf.yearly$other_investing_activities + df.cf.yearly$net_cash_from_discontinued_operations_investing

# -- cash from investing activities: quarterly --
df.results.quarterly$cash_from_investing_activities = df.cf.quarterly$change_in_fixed_assets_intangibles + df.cf.quarterly$disposition_of_fixed_assets_intangibles +df.cf.quarterly$disposition_of_fixed_assets +
  df.cf.quarterly$disposition_of_intangible_assets + df.cf.quarterly$acquisition_of_fixed_assets_intangibles + df.cf.quarterly$purchase_of_fixed_assets +
  df.cf.quarterly$acquisition_of_intangible_assets + df.cf.quarterly$other_change_in_fixed_assets_intangibles + df.cf.quarterly$net_change_in_long_term_investment +
  df.cf.quarterly$decrease_in_long_term_investment + df.cf.quarterly$increase_in_long_term_investment + df.cf.quarterly$net_cash_from_acquisitions_divestitures +
  df.cf.quarterly$net_cash_from_divestitures + df.cf.quarterly$cash_for_acquisition_of_subsidiaries + df.cf.quarterly$cash_for_joint_ventures +
  df.cf.quarterly$net_cash_from_other_acquisitions + df.cf.quarterly$other_investing_activities + df.cf.quarterly$net_cash_from_discontinued_operations_investing
