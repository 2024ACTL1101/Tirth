---
title: "ACTL1101 Assignment Part A"
author: "Tirth Thakker"
output:
  pdf_document: default
  html_document:
    df_print: paged
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, tidy.opts=list(width.cutoff=90),tidy=TRUE)
```

## Algorithmic Trading Strategy

## Introduction

In this assignment, you will develop an algorithmic trading strategy by incorporating financial metrics to evaluate its profitability. This exercise simulates a real-world scenario where you, as part of a financial technology team, need to present an improved version of a trading algorithm that not only executes trades but also calculates and reports on the financial performance of those trades.

## Background

Following a successful presentation to the Board of Directors, you have been tasked by the Trading Strategies Team to modify your trading algorithm. This modification should include tracking the costs and proceeds of trades to facilitate a deeper evaluation of the algorithm’s profitability, including calculating the Return on Investment (ROI).

After meeting with the Trading Strategies Team, you were asked to include costs, proceeds, and return on investments metrics to assess the profitability of your trading algorithm.

## Objectives

1. **Load and Prepare Data:** Open and run the starter code to create a DataFrame with stock closing data.

2. **Implement Trading Algorithm:** Create a simple trading algorithm based on daily price changes.

3. **Customize Trading Period:** Choose your entry and exit dates.

4. **Report Financial Performance:** Analyze and report the total profit or loss (P/L) and the ROI of the trading strategy.

5. **Implement a Trading Strategy:** Implement a trading strategy and analyze the total updated P/L and ROI. 

6. **Discussion:** Summarise your finding.


## Instructions

### Step 1: Data Loading

Start by running the provided code cells in the "Data Loading" section to generate a DataFrame containing AMD stock closing data. This will serve as the basis for your trading decisions. First, create a data frame named `amd_df` with the given closing prices and corresponding dates. 

```{r load-data}

# Load data from CSV file
amd_df <- read.csv("AMD.csv")

# Convert the date column to Date type and Adjusted Close as numeric
amd_df$date <- as.Date(amd_df$Date)
amd_df$close <- as.numeric(amd_df$Adj.Close)

amd_df <- amd_df[, c("date", "close")]
```


##Plotting the Data
Plot the closing prices over time to visualize the price movement.
```{r plot}
plot(amd_df$date, amd_df$close,'l')
```


## Step 2: Trading Algorithm
Implement the trading algorithm as per the instructions. You should initialize necessary variables, and loop through the dataframe to execute trades based on the set conditions.

- Initialize Columns: Start by ensuring dataframe has columns 'trade_type', 'costs_proceeds' and 'accumulated_shares'.
- Change the algorithm by modifying the loop to include the cost and proceeds metrics for buys of 100 shares. Make sure that the algorithm checks the following conditions and executes the strategy for each one:
  - If the previous price = 0, set 'trade_type' to 'buy', and set the 'costs_proceeds' column to the current share price multiplied by a `share_size` value of 100. Make sure to take the negative value of the expression so that the cost reflects money leaving an account. Finally, make sure to add the bought shares to an `accumulated_shares` variable.
  - Otherwise, if the price of the current day is less than that of the previous day, set the 'trade_type' to 'buy'. Set the 'costs_proceeds' to the current share price multiplied by a `share_size` value of 100.
  - You will not modify the algorithm for instances where the current day’s price is greater than the previous day’s price or when it is equal to the previous day’s price.
  - If this is the last day of trading, set the 'trade_type' to 'sell'. In this case, also set the 'costs_proceeds' column to the total number in the `accumulated_shares` variable multiplied by the price of the last day.



```{r trading}

# Initialize columns for trade type, cost/proceeds, and accumulated shares in amd_df
amd_df$trade_type <- NA
amd_df$costs_proceeds <- NA  # Corrected column name
amd_df$accumulated_shares <- 0  # Initialize if needed for tracking

##Step 2 has been turned into the function Run_Trades, this is to allow for the 
##same code to be used again in step 3 when the trading period is being modified
previous_price <- 0
share_size <- 100
accumulated_shares <- 0
Run_Trades <- function(){
  # Initialize variables for trading logic

  
  for (i in 1:nrow(amd_df)) {

    #Assigning whether to buy, sell or do nothing
    if (previous_price > amd_df$close[i] || previous_price == 0) {amd_df$trade_type[i] <- "buy"}
    #ensuring that columns do not have any N/A values 
    else {amd_df$trade_type[i] <- ""; amd_df$costs_proceeds[i] <- 0}
    #changing last row to a 'sell' row 
    if(i == nrow(amd_df)){amd_df$trade_type[i] <- "sell"}
    #checking whether to buy or sell, and updating columns accordingly.  
    if (amd_df$trade_type[i] == "buy") 
    {amd_df$costs_proceeds[i] <- amd_df$close[i]*-100; accumulated_shares <-
       accumulated_shares + 100}
    else if (amd_df$trade_type[i] == "sell")
    {amd_df$costs_proceeds[i] <- accumulated_shares*amd_df$close[i]}
    amd_df$accumulated_shares[i] <- accumulated_shares
    #resetting previous price for next iteration 
    previous_price <- amd_df$close[i]
    
  }
  return(amd_df)
}
```


## Step 3: Customize Trading Period
- Define a trading period you wanted in the past five years 
```{r period}

start_date = as.Date('2021-01-01')
end_date = as.Date('2021-12-31')
#creating the data frame for the modified period
amd_df = subset(amd_df, amd_df$date >= start_date & amd_df$date <= end_date)
#Running the trades for the new period 
amd_df <- Run_Trades()


```


## Step 4: Run Your Algorithm and Analyze Results
After running your algorithm, check if the trades were executed as expected. Calculate the total profit or loss and ROI from the trades.

- Total Profit/Loss Calculation: Calculate the total profit or loss from your trades. This should be the sum of all entries in the 'costs_proceeds' column of your dataframe. This column records the financial impact of each trade, reflecting money spent on buys as negative values and money gained from sells as positive values.
- Invested Capital: Calculate the total capital invested. This is equal to the sum of the 'costs_proceeds' values for all 'buy' transactions. Since these entries are negative (representing money spent), you should take the negative sum of these values to reflect the total amount invested.
- ROI Formula: $$\text{ROI} = \left( \frac{\text{Total Profit or Loss}}{\text{Total Capital Invested}} \right) \times 100$$

```{r}
profit <- round(sum(amd_df$costs_proceeds),2)
#cost for this algorithm is simply profit minus the last entry in the costs_proceeds column
total_cost <- profit - amd_df$costs_proceeds[nrow(amd_df)]
ROI <- round((profit/-total_cost)*100,2)
#paste statements have been used to allow for dynamic outputs 
paste('The profit earned by this strategy was $', as.character(profit), sep ='' )
paste('The total investment in buying the shares was $',round(-total_cost,2), sep = '')
paste('The ROI from this strategy was ', as.character(ROI),'%', sep = '')

```

## Step 5: Profit-Taking Strategy or Stop-Loss Mechanisum (Choose 1)
- Option 1: Implement a profit-taking strategy that you sell half of your holdings if the price has increased by a certain percentage (e.g., 20%) from the average purchase price.
- Option 2: Implement a stop-loss mechanism in the trading strategy that you sell half of your holdings if the stock falls by a certain percentage (e.g., 20%) from the average purchase price. You don't need to buy 100 stocks on the days that the stop-loss mechanism is triggered.


```{r option}

```


## Step 6: Summarize Your Findings
- Did your P/L and ROI improve over your chosen period?
- Relate your results to a relevant market event and explain why these outcomes may have occurred.


```{r}
As a result of using the profit taking strategy where we sold shares after a 15% increase on the average price the profit declined from  538,758.93 dollars to 169189.15 dollars, while ROI dropped from 43.25% to 15.23%. 
The main reason for this drop in ROI is that while costs dropped to 1,111,161 from 1,245,601 dollars, when using the strategy, profits dropped by a higher percentage, decreasing ROI. 

The primary reason for this decrease in profit is a market event that occurred near the end of our chosen trading period. On 8th November 2021, AMD announced that they had acquired Meta Platforms Inc as a customer for data center chips. This news led to increased consumer confidence, and AMD shares, which closed at 136.64 on the previous Friday, rose in price and closed at 150.16. AMD shares continued strong for the rest of the year, closing at 143.90 on 31/12/21.

However, when using the profit-taking strategy, we had sold a majority of our holdings previously at cheaper prices (as targets had already been hit), and hence we could not take full advantage of this sudden increase in price (only 68 shares were owned on 8th November)

In the previous, more basic strategy, since all shares were sold at the end of the year, the shares were sold at a relatively higher price leading to higher profits and ROI. 

It is important to note, that while the profit-taking strategy earned lower profits and had a lower ROI, it was also significantly less risky. Had the price reduced instead of going up the profit taking strategy would have minimized losses.

Alternatively, choosing a higher mark-up percentage, may have also improved the ROI of the profit-taking strategy in this case, given that AMD finished the year so strongly. 
```







