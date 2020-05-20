dataset = read.csv('Ads_CTR_Optimisation.csv')

N = 10000
reward = 0
totalReward = 0
ad_selected = integer(0)
for (n in 1:N) {
  ad = sample(1:10, size = 1)
  ad_selected = append(ad_selected, ad)
  reward = dataset[n,ad]
  totalReward = totalReward + reward
}

hist(ad_selected,
     col = "blue",
     main = "Histogram of Ad selected",
     xlab = "Ads",
     ylab = "Number of times selected")