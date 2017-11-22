download_data = function(url) {
  raw = read.table(url, header=TRUE, sep=",")
  raw = raw[, c(1, 7)] # 1 is Date column, 7 is Adj.Close column
  raw = raw[nrow(raw):1, ] # Sort oldest to newest
  return(raw)
}

daily_returns = function(data) {
  close_values = data[, 2] # 2 is Adj.Close column
  num = length(close_values) - 1
  returns = numeric(num)
  for (i in 1:num) {
    returns[i] = (close_values[i + 1] - close_values[i]) / close_values[i] * 100
  }
  return(returns)
}

print("===== Download S&P 500 and its 10 component stocks =====")
stocks = c("^GSPC", "ACN", "ATVI", "ADBE", "GOOG", "AAPL", "T", "CTXS", "KO", "EXPE", "FB")

count = 0

for (stock in stocks) {
  #####################################################################################
  # Step 1
  # Download data for last 1 year for the S&P500 and any 10 of its component stocks
  #####################################################################################
  # First date = 2013-07-30, Last date = 2016-07-30

  url = paste("http://chart.finance.yahoo.com/table.csv?s=", stock, "&a=9&b=8&c=2015&d=9&e=7&f=2016&g=d&ignore=.csv", sep="")
  data = download_data(url)
  #####################################################################################
  # Step 2
  # Calculate daily returns of the S&P500 index and the downloaded stocks over the
  # period under study.
  #####################################################################################
  print(paste("===== Daily Returns of ", stock, " =====", sep=""))
  returns = daily_returns(data)
  print(returns)

  #####################################################################################
  # Step 3
  # For each of the selected stocks and the index perform a Student’s T test, 
  # calculate the p-value and t-value 
  # and test the Null Hypothesis that the mean daily stock return is zero
  #####################################################################################
  print("Null Hypothesis: the mean daily stock return is zero")
  # 3.1 Calculate t-value (Student's T test)
  # Ref https://en.wikipedia.org/wiki/Student%27s_t-test#One-sample_t-test
  mu0 = 0
  xbar = mean(returns)
  s = sd(returns)
  n = length(returns)
  tval = (xbar - mu0) / (s / sqrt(n))
  print("===== t-value =====")
  print(tval)

  # 3.2 Calculate p-value from a t-distribution
  pval = 2 * pt(-abs(tval), df = n - 1, lower = FALSE)
  print("===== p-value =====")
  print(pval)

  # 3.3 Test the Null Hypothesis the mean daily stock return is zero
  print("Null Hypothesis H0: mu = 0")
  print("Alternate Hypothesis H1: mu != 0")
  print("Testing H0")
  alpha = 0.05
  if (pval <= alpha) {
    print("Reject H0")
  } else {
    print("Accept H0")
    count = count + 1
  }
}

print("A number of stocks that the null hypothesis accepted")
print(count)