library(simfinapi)
library(dplyr)
library(ggplot2)
library(reshape)
library(reshape2)
library(Rcpp)
library(magrittr)
library(plotly)
library(viridis)
library(tidyquant)

WD = getwd()

# source the API KEY https://simfin.com/

source("ConstantVariables.R")

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
quarters <- c("q1", "q2", "q3", "q4")
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

## Plots the Leverage Ratios

df <- data.frame(quarter = df.results.quarterly$time,
                 book = df.results.quarterly$book_leverage,
                 market = df.results.quarterly$market_leverage)
df <- df %>% melt(id.var = 'quarter')
ggplot(df, aes(x = quarter, y = value, group = variable, colour = variable)) + 
  geom_line() + 
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
  ggtitle("Leverage Ratios") +
  ylab("leverage")
  

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
  df.cf.yearly$other_non_cash_adjustments + df.cf.yearly$change_in_working_capital + df.cf.yearly$net_cash_from_discontinued_operations_operating + df.cf.yearly$change_in_other

# -- cash from operating activities: quarterly --
df.results.quarterly$cash_from_operating_activities = df.cf.quarterly$net_income_starting_line + df.cf.quarterly$other_adjustments + df.cf.quarterly$depreciation_amortization +
  df.cf.quarterly$non_cash_items + df.cf.quarterly$stock_based_compensation + df.cf.quarterly$deferred_income_taxes +
  df.cf.quarterly$other_non_cash_adjustments + df.cf.quarterly$change_in_working_capital + df.cf.quarterly$net_cash_from_discontinued_operations_operating + 
  df.cf.quarterly$change_in_other

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


# Plotting the ratios

library(patchwork)
library(scales)

df <- data.frame(quarter = df.results.quarterly$time,
                 net_profit = df.results.quarterly$net_profit_margin,
                 roe = df.results.quarterly$return_on_equity,
                 roa = df.results.quarterly$return_on_assets)
df <- df %>% melt(id.var = 'quarter')
fig1 <- ggplot(df, aes(x = quarter, y = value, group = variable, colour = variable)) + 
    geom_line() + 
    scale_color_viridis(discrete = TRUE, option = "plasma", begin = 0, end = 0.6) +
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
    ggtitle("Financial Ratios") +
    ylab("level")

df2 <- data.frame(quarter = df.results.quarterly$time,
                 operations = df.results.quarterly$cash_from_operating_activities,
                 financing = df.results.quarterly$cash_from_financing_activities,
                 investing = df.results.quarterly$cash_from_investing_activities)
df2 <- df2 %>% melt(id.var = 'quarter')
fig2 <- ggplot(df2, aes(x = quarter, y = value, group = variable, colour = variable)) + 
    geom_line() + 
    scale_color_viridis(discrete = TRUE, option = "D", begin = 0.4, end = 0.8) +
    scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
    ggtitle("Cash from diferent activities") +
    ylab("Cash")

fig1 + fig2 + plot_layout(ncol = 1, nrow = 2)


#### EX 3 ####

# load in the dividend dates 
# https://www.streetinsider.com/dividend_history.php?q=cop

df.dividends = read.csv('dividend_dates.csv')

# formatting the columns
df.dividends$Ex.Div..Date = df.dividends$Ex.Div..Date %>% as.Date(format="%m/%d/%y")
df.dividends$Decl..Date = df.dividends$Decl..Date %>% as.Date(format="%m/%d/%y")
df.dividends$Rec..Date = df.dividends$Rec..Date %>% as.Date(format="%m/%d/%y")
df.dividends$Pay..Date = df.dividends$Pay..Date %>% as.Date(format="%m/%d/%y")
df.dividends$Amount = sub("$", "", df.dividends$Amount, fixed = TRUE) %>% as.numeric
df.dividends$Yield = sub("%", "", df.dividends$Yield, fixed = TRUE) %>% as.numeric


#### EX 4 ####

# The S&P500 Energy is the most suitable market proxy for a US-based firm in the broadest sense.
# However, as part of this case study we try to evaluate a potential investment in
# the sector of US publicly traded companies that deal in oil/gas. Hence, the most suitable
# market proxy would be an US index that tracks the oil/gas sector in the USA. 
# The Dow Jones U.S. Oil & Gas Index (DJUSEN) seems like a suitable choice. The index is
# designed to measure the stock performance of U.S. companies in the oil and gas sector, 
# complete information was nos available online so we decided to stick with the S&P500_Energy
# S&P Energy Sector is Part of the S&P Index, composed of 21 energy stocks, including COP.
# ConocoPhillips (COP).
# https://www.spglobal.com/spdji/en/indices/equity/sp-500-energy-sector/#overview


df.SP500 = tq_get("^GSPC", get='stock.prices',
                from="2016-01-01", to="2020-12-31")

df.COP = tq_get("COP", get='stock.prices',
                from="2016-01-01", to="2020-12-31")

df.SP500E = read.csv("sp500e.csv")
Sys.setlocale("LC_TIME", "C")
colnames(df.SP500E)[1:2] = c("date","price")
df.SP500E$date = df.SP500E$date %>% as.Date(format="%b %d, %Y")
df.SP500_E = df.SP500E %>% filter(between(date, as.Date("2016-01-01"),as.Date("2020-12-31")))

price_SP <- df.SP500[c("date", "adjusted")]
price_COP <- df.COP[c("date", "adjusted")]
price_SP_E <- df.SP500_E[c("date", "price")]

joined_df <- left_join(price_COP, price_SP, by=c("date"))
full_df <- left_join(joined_df, price_SP_E, by=c("date"))
colnames(full_df) <- c("date", "COP", "SP500", "SP500_Energy")

norm_df <- as.data.frame(cbind(full_df[,1], apply(full_df[,2:4], 2, function(x) x/x[1])))
colnames(norm_df) <- c("date", "COP", "SP500", "SP500_Energy")


# Plot for comparison of the Market index

df <- norm_df %>% melt(id.var = 'date')
ggplot(df, aes(x = date, y = value, group = variable, colour = variable)) + 
  geom_line() + 
  #scale_color_viridis(discrete = TRUE, option = "D", begin = 0.4, end = 0.8) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  ggtitle("Market Index Comparision") +
  ylab("Return")

#### EX 5 ###


df.COP = tq_get("COP", get='stock.prices',
                from="2016-01-01", to="2020-12-31")

df.COP.daily = df.COP %>% tq_transmute(select=adjusted,
                                       mutate_fun=periodReturn, 
                                       period="daily", 
                                       type="arithmetic") 

df.COP.weekly = df.COP %>% tq_transmute(select=adjusted,
                                        mutate_fun=periodReturn, 
                                        period="weekly", 
                                        type="arithmetic") 
  
df.COP.monthly = df.COP %>% tq_transmute(select=adjusted,
                                         mutate_fun=periodReturn, 
                                         period="monthly", 
                                         type="arithmetic") 
  
returns_daily_ar_annualized = df.COP.daily$daily.returns %>% Return.annualized(scale = 252, geometric = FALSE)
returns_weekly_ar_annualized = df.COP.weekly$weekly.returns %>% Return.annualized(scale = 52, geometric = FALSE)
returns_monthly_ar_annualized = df.COP.monthly$monthly.returns %>% Return.annualized(scale = 12, geometric = FALSE)

returns_daily_compounded_annualized = df.COP.daily$daily.returns %>% Return.annualized(scale = 252, geometric = TRUE)
returns_weekly_compounded_annualized = df.COP.weekly$weekly.returns %>% Return.annualized(scale = 52, geometric = TRUE)
returns_monthly_compounded_annualized = df.COP.monthly$monthly.returns %>% Return.annualized(scale = 12, geometric = TRUE)






# src: https://www.investing.com/indices/s-p-500-energy-historical-data
df.SP500E = read.csv("InputFiles/sp500e.csv")
# set right language for R
Sys.setlocale("LC_TIME", "C")

colnames(df.SP500E)[1:2] = c("date","price")
df.SP500E$date = df.SP500E$date %>% as.Date(format="%b %d, %Y")


df.SP500E.small = df.SP500E %>% filter(between(date, as.Date("2016-01-01"),as.Date("2020-12-31")))

df.SP500E.daily = df.SP500E.small %>% tq_transmute(select="price",
                                       mutate_fun=periodReturn, 
                                       period="daily", 
                                       type="arithmetic") 

df.SP500E.weekly = df.SP500E.small %>% tq_transmute(select="price",
                                        mutate_fun=periodReturn, 
                                        period="weekly", 
                                        type="arithmetic") 

df.SP500E.monthly = df.SP500E.small %>% tq_transmute(select="price",
                                         mutate_fun=periodReturn, 
                                         period="monthly", 
                                         type="arithmetic") 

market_daily_ar_annualized = df.SP500E.daily$daily.returns %>% Return.annualized(scale = 252, geometric = FALSE)
market_weekly_ar_annualized = df.SP500E.weekly$weekly.returns %>% Return.annualized(scale = 52, geometric = FALSE)
market_monthly_ar_annualized = df.SP500E.monthly$monthly.returns %>% Return.annualized(scale = 12, geometric = FALSE)

market_daily_compounded_annualized = df.SP500E.daily$daily.returns %>% Return.annualized(scale = 252, geometric = TRUE)
market_weekly_compounded_annualized = df.SP500E.weekly$weekly.returns %>% Return.annualized(scale = 52, geometric = TRUE)
market_monthly_compounded_annualized = df.SP500E.monthly$monthly.returns %>% Return.annualized(scale = 12, geometric = TRUE)


# barplot for annualized returns x=day,week,month ; y=return ; cat=cop,market
df.annualized.returns.geom = data.frame(frequency=c("daily","weekly","monthly",
                                              "daily","weekly","monthly"),
                                        compounded_return=c(returns_daily_compounded_annualized,
                                                          returns_weekly_compounded_annualized,
                                                          returns_monthly_compounded_annualized,
                                                          market_daily_compounded_annualized,
                                                          market_weekly_compounded_annualized,
                                                          market_monthly_compounded_annualized),
                                  ticker=c("COP","COP","COP","SP500E","SP500E","SP500E"))

df.annualized.returns.ar = data.frame(frequency=c("daily","weekly","monthly",
                                               "daily","weekly","monthly"),
                                   arithmetic_avg_return=c(returns_daily_ar_annualized,
                                                           returns_weekly_ar_annualized,
                                                           returns_monthly_ar_annualized,
                                                           market_daily_ar_annualized,
                                                           market_weekly_ar_annualized,
                                                           market_monthly_ar_annualized),
                                   ticker=c("COP","COP","COP","SP500E","SP500E","SP500E"))

g1 = ggplot(df.annualized.returns.ar, aes(x=frequency, y=arithmetic_avg_return, fill=ticker)) +
  geom_bar(stat="identity", position=position_dodge()) +
  ylab("arithmetic avg. annualized return")+
  scale_fill_brewer(palette="Paired")
  
g2 = ggplot(df.annualized.returns.geom, aes(x=frequency, y=compounded_return, fill=ticker)) +
geom_bar(stat="identity", position=position_dodge()) +
  ylab("compounded annualized return")+
  scale_fill_brewer(palette="Paired")

ggpubr::ggarrange(g1,g2, ncol = 2)

#### EX 6 ####

# --- daily return volatiltiy ---
sd(df.COP.daily$daily.returns) * sqrt(252)
sd(df.SP500E.daily$daily.returns) * sqrt(252)

# --- weekly return volatiltiy ---
sd(df.COP.weekly$weekly.returns) * sqrt(52)
sd(df.SP500E.weekly$weekly.returns) * sqrt(52)

# --- monthly return volatiltiy ---
sd(df.COP.monthly$monthly.returns) * sqrt(12)
sd(df.SP500E.monthly$monthly.returns) * sqrt(12)




#### EX 7 ####

# --- SECOND APPROACH: rolling window of 104

vola_cop.weekly = runSD(df.COP.weekly$weekly.returns, n=104) * sqrt(52)

vola_sp500e.weekly = runSD(df.SP500E.weekly$weekly.returns, n=104)  * sqrt(52)

vola_cop.weekly %>% is.na %>% sum # 103
df.plot.vola = data_frame(date=df.COP.weekly$date[-(1:103)],
                          cop=vola_cop.weekly[-(1:103)],
                          sp500e=vola_sp500e.weekly[-(1:103)])

ggplot(df.plot.vola %>% melt(id='date', variable.name="Ticker"),
       aes(x=date, y=value, colour=Ticker)) +
  geom_line(size=1) +
  ggtitle("Rolling Volatility (n=104)")+
  ylab("volatility (annualized)") 



#### EX 8 ####

# get the big data frame for COP
df.COP.big = tq_get("COP", get='stock.prices',
                    from="2006-01-01", to="2020-12-31")

# compute the weekly returns for the big data frame
df.COP.big.weekly = df.COP.big %>% tq_transmute(select=adjusted,
                                        mutate_fun=periodReturn, 
                                        period="weekly", 
                                        type="arithmetic") 
# filter the right data
df.SP500E.big = df.SP500E %>% filter(between(date, as.Date("2006-01-01"),as.Date("2020-12-31")))

df.SP500E.weekly =  df.SP500E.big %>% tq_transmute(select="price",
                                                     mutate_fun=periodReturn, 
                                                     period="weekly", 
                                                     type="arithmetic") 


# --- FIRST APPROACH: compute one correlation for each year ---

#(6,7) (8,9) (10, 11) ... (18, 19) (20_start, 20_end)
df.iteration = data.frame(start=c(paste0("20",ifelse(seq(6, 18, by=2)<10,"0",""), seq(6, 18, by=2),"-01-01"), "2020-01-01"),
                          end=c(paste0("20",ifelse(seq(7, 19, by=2)<10,"0",""),seq(7, 19, by=2),"-12-31"), "2020-12-31"))

# create empty seq for correlation values
cor_seq = c()

# compute the correlation for 2 years like mentioned above
for(row in 1:nrow(df.iteration)){
  start_date = df.iteration[row, "start"]
  end_date = df.iteration[row, "end"]
  df.sp_tmp = df.SP500E.weekly %>% filter(between(date, as.Date(start_date),as.Date(end_date)))
  df.cop_tmp = df.COP.big.weekly %>% filter(between(date, as.Date(start_date),as.Date(end_date)))
  cor_seq = c(cor_seq, cor(df.sp_tmp$weekly.returns, df.cop_tmp$weekly.returns))
}

# plot the correlation in a barplot
barplot(cor_seq, names.arg = sapply(1:8, FUN = function(x)  paste0("(",df.iteration[x,],")", collapse = " to\n ")), las=2)


# --- SECOND APPROACH: compute rolling correlation ---

# rolling correlation for a window of 104 weeks
rolling_cor = runCor(df.SP500E.weekly$weekly.returns, df.COP.big.weekly$weekly.returns, n=104)


write.csv(rolling_cor, file = "corr.csv")

ggplot(data.frame(date=df.SP500E.weekly$date[-(1:103)], corr=rolling_cor[-(1:103)]),
       aes(x=date,y=corr)) +
  geom_line(size=1, color='#1e90ff')+
  ylab("correlation")+
  ggtitle("Rolling Correlation: COP - SP500E") 


# plot the time series of the rolling correlation

df <- data.frame(date = df.SP500E.weekly$date, corr = rolling_cor)
df <- na.omit(df)
ggplot(df, aes(x = date, y = corr, group = 1)) + 
  geom_line(color="steelblue", size=1) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  ggtitle("Rolling Correlation") +
  ylab("correlation")

