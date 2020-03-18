dataset = read.csv('Corona_18Mar2020.csv')
Corona_India = dataset %>% select(Name.of.State...UT,Total.Confirmed.cases..Indian.National.,Total.Confirmed.cases...Foreign.National..,Cured.Discharged,Death)
ggplot()+
  geom_point(aes(Corona_India$Name.of.State...UT, Corona_India$Death),color='red')+
  geom_point(aes(Corona_India$Name.of.State...UT, Corona_India$Cured.Discharged),color='blue')+
  geom_point(aes(Corona_India$Name.of.State...UT, Corona_India$Total.Confirmed.cases..Indian.National.),color='black')+
  geom_point(aes(Corona_India$Name.of.State...UT, Corona_India$Total.Confirmed.cases...Foreign.National..),color='pink')