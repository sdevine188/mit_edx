library(dplyr)
library(stringr)
library(readr)
library(lubridate)
library(ggplot2)
library(reshape2)

# unit 1 - stock prices
setwd("c:/users/stephen/desktop/r/mit_edx/unit1")
list.files()

# read in data
ibm <- read.csv("IBMStock.csv", stringsAsFactors = FALSE)
ge <- read.csv("GEStock.csv", stringsAsFactors = FALSE)
cc <- read.csv("CocaColaStock.csv", stringsAsFactors = FALSE)
pg <- read.csv("ProcterGambleStock.csv", stringsAsFactors = FALSE)
bo <- read.csv("BoeingStock.csv", stringsAsFactors = FALSE)

# convert dates to dates
ibm$Date <- mdy(ibm$Date)
ge$Date <- mdy(ge$Date)
cc$Date <- mdy(cc$Date)
pg$Date <- mdy(pg$Date)
bo$Date <- mdy(bo$Date)

dim(ibm)

# need to handle issue with 1900 vs 2000s for two digit year
# years range from 1970 to 2009
century <- function(x, cutoff=1970){
        two_digit_year <- year(x) %% 100
        year(x) <- ifelse(two_digit_year < cutoff %% 100, 2000 + two_digit_year, 1900 + two_digit_year)
        x
}

ibm$Date <- century(ibm$Date)
ge$Date <- century(ge$Date)
cc$Date <- century(cc$Date)
pg$Date <- century(pg$Date)
bo$Date <- century(bo$Date)

min(year(ibm$Date))
min(year(ge$Date))
min(year(cc$Date))
min(year(pg$Date))
min(year(bo$Date))

max(year(ibm$Date))
max(year(ge$Date))
max(year(cc$Date))
max(year(pg$Date))
max(year(bo$Date))

mean(ibm$StockPrice)
min(ge$StockPrice)
max(cc$StockPrice)
median(bo$StockPrice)
sd(pg$StockPrice)

cc_plot <- ggplot(data = cc, aes(x = Date, y = StockPrice)) + geom_line()
cc_pg <- cbind(cc, pg)
names(cc_pg)[2] <- "cc_stock_price"
names(cc_pg)[4] <- "pg_stock_price"
cc_pg_long <- melt(cc_pg, id.vars = "Date")
ggplot(data = cc_pg_long, aes(x = Date, y = value, color = variable)) + geom_line()

ggplot(data = filter(cc, year(Date) > 1994, year(Date) < 2006), aes(x = Date, y = StockPrice, color = "cc")) + geom_line() +
        geom_line(aes(y = ibm$StockPrice, color = "ibm")) + 
        geom_line(aes(y = ge$StockPrice, color = "ge")) + 
        geom_line(aes(y = pg$StockPrice, color = "pg")) + 
        geom_line(aes(y = bo$StockPrice, color = "bo"))

# create one master data of all stocks
stocks <- cbind(cc, pg, ibm, ge, bo)
names(stocks) <- c("date", "cc_price", "date", "pg_price", "date", "ibm_price", "date", 
                   "ge_price", "date", "bo_price")
stocks_long <- melt(stocks, id.vars = "date", 
                    measure.vars = c("cc_price", "pg_price", "ibm_price",
                        "ge_price","bo_price"), variable.name = "stock", value.name = "price")
stocks_long %>% filter(year(date) > 1994, year(date) < 2006) %>% 
        ggplot(aes(x = date, y = price, group = stock, color = stock)) + geom_line()

stocks_long %>% filter(year(date) == 1997, month(date) < 12, month(date) > 8) %>% 
        ggplot(aes(x = date, y = price, group = stock, color = stock)) + geom_line()

stocks_long %>% filter(year(date) > 2003, year(date) < 2006) %>% 
        ggplot(aes(x = date, y = price, group = stock, color = stock)) + geom_line()

# average ibm stock price by month
monthly_avg <- stocks_long %>% mutate(month = month(date)) %>% group_by(month) %>% filter(stock == "ibm_price") %>% 
        summarize(monthly_avg_price_ibm = mean(price))
overall_avg <- stocks_long %>% mutate(month = month(date)) %>% filter(stock == "ibm_price") %>% 
        summarize(overall_avg_price_ibm = mean(price))
monthly_avg[which(monthly_avg$monthly_avg_price_ibm > 
                                                overall_avg$overall_avg_price_ibm), ]

stocks_long %>% filter(stock %in% c("ge_price", "cc_price")) %>% 
        mutate(month = month(date)) %>% group_by(stock, month) %>% 
        summarize(monthly_avg = mean(price)) %>% group_by(stock) %>% 
        filter(monthly_avg == max(monthly_avg))

stocks_long %>% filter(month(date) %in% c(12, 1)) %>% mutate(year = year(date)) %>% 
        mutate(month = month(date)) %>%
        group_by(stock, year, month) %>% summarize(monthly_avg = mean(price)) %>%
        group_by(stock, year) %>% summarize(monthly_diff = monthly_avg[1] - monthly_avg[2]) %>%
        group_by(stock) %>%
        summarize(avg_monthly_diff = mean(monthly_diff))
