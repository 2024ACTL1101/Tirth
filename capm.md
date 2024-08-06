

# CAPM Analysis

## Introduction

In this assignment, you will explore the foundational concepts of the Capital Asset Pricing Model (CAPM) using historical data for AMD and the S&P 500 index. This exercise is designed to provide a hands-on approach to understanding how these models are used in financial analysis to assess investment risks and returns.

## Background

The CAPM provides a framework to understand the relationship between systematic risk and expected return, especially for stocks. This model is critical for determining the theoretically appropriate required rate of return of an asset, assisting in decisions about adding assets to a diversified portfolio.

## Objectives

1. **Load and Prepare Data:** Import and prepare historical price data for AMD and the S&P 500 to ensure it is ready for detailed analysis.
2. **CAPM Implementation:** Focus will be placed on applying the CAPM to examine the relationship between AMD's stock performance and the overall market as represented by the S&P 500.
3. **Beta Estimation and Analysis:** Calculate the beta of AMD, which measures its volatility relative to the market, providing insights into its systematic risk.
4. **Results Interpretation:** Analyze the outcomes of the CAPM application, discussing the implications of AMD's beta in terms of investment risk and potential returns.

## Instructions

### Step 1: Data Loading

- We are using the `quantmod` package to directly load financial data from Yahoo Finance without the need to manually download and read from a CSV file.
- `quantmod` stands for "Quantitative Financial Modelling Framework". It was developed to aid the quantitative trader in the development, testing, and deployment of statistically based trading models.
- Make sure to install the `quantmod` package by running `install.packages("quantmod")` in the R console before proceeding.

```r
# Set start and end dates
start_date <- as.Date("2019-05-20")
end_date <- as.Date("2024-05-20")

# Load data for AMD, S&P 500, and the 1-month T-Bill (DTB4WK)
amd_data <- getSymbols("AMD", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
gspc_data <- getSymbols("^GSPC", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
rf_data <- getSymbols("DTB4WK", src = "FRED", from = start_date, to = end_date, auto.assign = FALSE)

# Convert Adjusted Closing Prices and DTB4WK to data frames
amd_df <- data.frame(Date = index(amd_data), AMD = as.numeric(Cl(amd_data)))
gspc_df <- data.frame(Date = index(gspc_data), GSPC = as.numeric(Cl(gspc_data)))
rf_df <- data.frame(Date = index(rf_data), RF = as.numeric(rf_data[,1]))  # Accessing the first column of rf_data

# Merge the AMD, GSPC, and RF data frames on the Date column
df <- merge(amd_df, gspc_df, by = "Date")
df <- merge(df, rf_df, by = "Date")
```

#### Data Processing 
```r
colSums(is.na(df))
# Fill N/A RF data
df <- df %>%
  fill(RF, .direction = "down") 
```

### Step 2: CAPM Analysis

The Capital Asset Pricing Model (CAPM) is a financial model that describes the relationship between systematic risk and expected return for assets, particularly stocks. It is widely used to determine a theoretically appropriate required rate of return of an asset, to make decisions about adding assets to a well-diversified portfolio.

#### The CAPM Formula
The formula for CAPM is given by:

$$
E(R_i) = R_f + \beta_i (E(R_m) - R_f)
$$

Where:

- $E(R_i)$ is the expected return on the capital asset,
- $R_f$ is the risk-free rate,
- $\beta_i$ is the beta of the security, which represents the systematic risk of the security,
- $E(R_m)$ is the expected return of the market.



#### CAPM Model Daily Estimation

- **Calculate Returns**: First, we calculate the daily returns for AMD and the S&P 500 from their adjusted closing prices. This should be done by dividing the difference in prices between two consecutive days by the price at the beginning of the period.
  
$$
\text{Daily Return} = \frac{\text{Today's Price} - \text{Previous Trading Day's Price}}{\text{Previous Trading Day's Price}}
$$

```r
#adding colums for amd and gspc returns 
df <- df %>%
  mutate(amd_returns = NA)
df <- df %>%
  mutate(gspc_returns = NA)
#Calculating daily returns 
for (i in 2:nrow(df)) { 
  df$amd_returns[i] <- ((df$AMD[i] - df$AMD[i-1])/df$AMD[i-1])
  df$gspc_returns[i] <- ((df$GSPC[i] - df$GSPC[i-1])/df$GSPC[i-1])
}
```

- **Calculate Risk-Free Rate**: Calculate the daily risk-free rate by conversion of annual risk-free Rate. This conversion accounts for the compounding effect over the days of the year and is calculated using the formula:
  
$$
\text{Daily Risk-Free Rate} = \left(1 + \frac{\text{Annual Rate}}{100}\right)^{\frac{1}{360}} - 1
$$

```r
#converting annual rates in the data frame to daily rates 
for (j in 1:nrow(df)){ 
  df$RF[j] <- (1+ df$RF[j]/100)^(1/360) -1
}
```


- **Calculate Excess Returns**: Compute the excess returns for AMD and the S&P 500 by subtracting the daily risk-free rate from their respective returns.

```r
#adding columns for excess amd and gspc returns
df <- df %>% 
  mutate(amd_excess_returns = NA)
df <- df %>% 
  mutate(gspc_excess_returns = NA)
#calculating excess returns for each day 
for (m in 2:nrow(df)){
  df$amd_excess_returns[m] <- df$amd_returns[m] - df$RF[m]
  df$gspc_excess_returns[m] <- df$gspc_returns[m] - df$RF[m]
}
```


- **Perform Regression Analysis**: Using linear regression, we estimate the beta (\(\beta\)) of AMD relative to the S&P 500. Here, the dependent variable is the excess return of AMD, and the independent variable is the excess return of the S&P 500. Beta measures the sensitivity of the stock's returns to fluctuations in the market.

```r
#removing na values (first row)
df = na.omit(df)
#performing linear regression 
capm_model <- lm(amd_excess_returns ~ gspc_excess_returns, data = df)
#displaying results 
summary <- summary(capm_model)
print(summary)
```


#### Interpretation

What is your \(\beta\)? Is AMD more volatile or less volatile than the market?

**Answer:**
Also note that since we have a low p value for the F statistic, and an $R^2$ of 0.4022, we can say that the model is a good fit. 

The beta value can be seen in the above plot to be 1.57 to 2 d.p. This suggests that AMD is more volatile than the market, as a 1% change in the expected adjusted return of the market leads to a 1.57% change in the return for AMD.
This suggests that there is a higher risk to investing in AMD, in the case that the market crashes, as AMD stock will fall further, although it does promise higher returns in case the market does well. 


#### Plotting the CAPM Line
Plot the scatter plot of AMD vs. S&P 500 excess returns and add the CAPM regression line.

```r
#defining plot as a ggplot object
plot <- ggplot(df, aes(x = gspc_excess_returns, y = amd_excess_returns))+
  #adding points
  geom_point()+
  #adding regression line
  geom_smooth(method = "lm", col="green")+
  labs(title = "AMD vs S&P 500 excess returns", x = "Excess S&P 500 Returns", y = "Excess AMD returns")+
  theme(plot.title = element_text(hjust=0.5))
print(plot)
```

### Step 3: Predictions Interval
Suppose the current risk-free rate is 5.0%, and the annual expected return for the S&P 500 is 13.3%. Determine a 90% prediction interval for AMD's annual expected return.



**Answer:**

```r
#fill the code
n <- length(df$gspc_excess_returns)
gspc_expected<- 13.3/100
rf<- 5
#exctracting beta 
beta <- summary$coefficients[2,1]

#using simple return average method to convert annual expected return to daily
#expected return, using 252 market days 
daily_gspc <- gspc_expected/252
#changing the risk free rate to a daily rate 
daily_rf <- (1+ rf/100)^(1/360) -1
#calculating excess gspc returns 
daily_excess_gspc <- daily_gspc - daily_rf
#making daily prediction
daily_amd <- beta*daily_excess_gspc + daily_rf
#converting to annual prediction
predicted_amd <- daily_amd*252
gspc_mean <- mean(df$gspc_excess_returns)
#calculating standard error of the model
daily_se <- sqrt(sum(residuals(summary)^2/(n-2)))
SSX <- sum((df$gspc_excess_returns - gspc_mean)^2)
#calculating standard error for the forecast (daily)
daily_sf <- daily_se*sqrt(1 + 1/n + (daily_excess_gspc - gspc_mean)^2/SSX)
#approximating annual standard forcast error
annual_sf <- sqrt(252)*daily_sf
tval <- qt(0.95, df = n-2)
annual_upper <- predicted_amd + annual_sf*tval
annual_lower <- predicted_amd - annual_sf*tval 



paste('The annual expected return: ', round(predicted_amd,4)*100,'%', sep = '')
paste('The 90% Confidence interval is between ', round(annual_lower,4)
      *100,'% and ', round(annual_upper,4)*100,'%', sep = '')
```
