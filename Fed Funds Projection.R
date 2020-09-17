#---------------------------------------------------------- Fed Funds Target Forecast ---------------------------------------------------------------------#
# Uses the Bullard Methodology to calculate the what the Fed Funds rate should be.
# Based on calculation Fed path can be inferred.
# Model uses economic data reflecting the state of the economy to determine if policy should be accomodative or restrictive.
#----------------------------------------------------------------------------------------------------------------------------------------------------------#

if (!require("fredr")) {install.packages("fredr"); library('fredr')}
if (!require("mFilter")) {install.packages("mFilter"); library('mFilter')}
if (!require("lubridate")) {install.packages("lubridate"); library('lubridate')}
if (!require("xts")) {install.packages("xts"); library('xts')}
if (!require("dplyr")) {install.packages("dplyr"); library('dplyr')}

# FRED API Key HERE

# Dates
medate <-floor_date(Sys.Date(),"month") - 1
qe.lagged <-as.Date(cut(as.Date(cut(Sys.Date(), "quarter")), "quarter")) - 1
start.date = as.Date("2003-01-01")

# Manual Inputs
smoothing.factor <- .85
r.smoothing.factor <- (1-smoothing.factor)
inflation.target <-.02
unemployment.coefficient <-.1
inflation.coefficient <-1.5

# One Quarter Lag of Target Fed Funds Rate. it-1
fed.funds.target <-fredr(series_id = 'DFEDTARL',observation_start = qe.lagged ,observation_end =qe.lagged ,frequency = 'q')

# real interest rate rt*
pce <-fredr(series_id = 'PCETRIM12M159SFRBDAL', observation_start = start.date, observation_end = medate,frequency = 'm')
cmt.1yr <-fredr(series_id = 'GS1', observation_start = start.date, observation_end = medate,frequency = 'm')
rt.star <-hpfilter((cmt.1yr$value - pce$value),freq = 600,type = 'lambda', drift = FALSE)
rt.star <-tail(rt.star$trend,1)

# Unemplyoment Gap: CUrrent UNrate less CBO natural rate of unemployment
natural.unemployment <-fredr(series_id='NROU', observation_start=start.date,frequency='q')
unemployment.rate <-fredr(series_id='UNRATE', observation_start=start.date, observation_end = medate,frequency='m')
natural.unemployment <-natural.unemployment[natural.unemployment$date %in% unemployment.rate$date, ]
ut.gap <-tail(unemployment.rate$value,1) - tail(natural.unemployment$value,1)

# Inflation Gap: 5y BEI (adjusted by 30bps to bring in line with PCE) less Fed Inflation Target
five.year.treasury <-fredr(series_id='DGS5',frequency = 'm', observation_start = start.date,observation_end = medate)
five.year.tip <-fredr(series_id='DFII5', frequency = 'm', observation_start = start.date,observation_end = medate)
inflation.gap <-((tail(five.year.treasury$value,1) -  tail(five.year.tip$value,1)) - .3) - inflation.target

forecast.fed.funds <-smoothing.factor * fed.funds.target$value + (1-smoothing.factor)*(rt.star + inflation.target + inflation.coefficient*inflation.gap + unemployment.coefficient * ut.gap)

print(round(forecast.fed.funds,1)) # Low-End Target
print(round((forecast.fed.funds + 0.25) * 2) / 2 - 0.25) # High-End Target
