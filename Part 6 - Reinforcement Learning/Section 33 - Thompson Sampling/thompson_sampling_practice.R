dataset = read.csv('Ads_CTR_Optimisation.csv')
NumRows = nrow(dataset)
NumCol = ncol(dataset)
No_of_times_reward_0 = integer(NumCol)
No_of_times_reward_1 = integer(NumCol)
ad_selected = integer(0)
total_reward = 0
for (i in 1:NumRows) {
  ad = 0
  max_random = 0
  for (colm in 1:NumCol) {
    random_beta = rbeta(n = 1,
                        shape1 = No_of_times_reward_1[colm] + 1,
                        shape2 = No_of_times_reward_0[colm] + 1
                        )
    if (random_beta > max_random) {
      max_random = random_beta
      ad = colm
    }
  }
  ad_selected = append(ad_selected, ad)
  reward = dataset[i, ad]
  if (reward == 1)
    No_of_times_reward_1[ad] = No_of_times_reward_1[ad] + 1
  else
    No_of_times_reward_0[ad] = No_of_times_reward_0[ad] + 1
  total_reward = total_reward + reward
}
hist(ad_selected,
     col = "blue",
     main = "Histogram of Selected ADs",
     xlab = "AD",
     ylab = "No of times selected")