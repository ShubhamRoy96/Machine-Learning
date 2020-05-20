dataset = read.csv('Ads_CTR_Optimisation.csv')
NumRows = nrow(dataset)
NumCol = ncol(dataset)
NtimeSelected = integer(NumCol)
SumofRewards = integer(NumCol)
ad_selected = integer(0)
total_reward = 0

for (row in 1:NumRows) {
  ad = 0
  max_UB = 0
  
  for (lCol in 1:NumCol) {
    if (NtimeSelected[lCol] > 0) {
      avg_reward = SumofRewards[lCol] / NtimeSelected[lCol]
      Delta_i = sqrt((3 / 2) * (log(row) / NtimeSelected[lCol]))
      upper_bound = avg_reward + Delta_i
    }
    else
    {
      upper_bound = 10e400
    }
    
    if (upper_bound > max_UB)
    {
      max_UB = upper_bound
      ad = lCol
    }
  }
  
  ad_selected = append(ad_selected, ad)
  NtimeSelected[ad] = NtimeSelected[ad] + 1
  reward = dataset[row, ad]
  SumofRewards[ad] = SumofRewards[ad] + reward
  total_reward = total_reward + reward
}

hist(ad_selected,
     col = "blue",
     main = "Histogram of Selected ADs",
     xlab = "AD",
     ylab = "No of times selected")
