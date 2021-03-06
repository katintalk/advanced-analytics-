mdf<-read.csv("housedata_2.csv", header=T)
View(mdf)
set.seed(123)
pairs(mdf[1:4])

mlmresults<-lm(price~sqft_living+condition+grade, mdf)
mlmresults
summary(mlmresults)

mlmresults1<-lm(price~sqft_living+grade, mdf)
mlmresults1
summary(mlmresults1)

mlmresults2<-lm(price~sqft_living+condition+grade, mdf)
mlmresults2
summary(mlmresults2)

mlmresults3<-lm(price~condition, mdf)
mlmresults3
summary(mlmresults3)

mlmresults4<-lm(price~sqft_living, mdf)
mlmresults4
summary(mlmresults4)


reduced<-lm(price~sqft_living, mdf)
partial<-lm(price~sqft_living+condition, mdf)
partial1<-lm(price~sqft_living+grade, mdf)
full<-lm(price~sqft_living+condition+grade, mdf)
anova(reduced, partial,partial1, full)

#first equation doesn't fit by anova
predict(reduced, data.frame(sqft_living=2160), interval = 'confidence')
predict(partial, data.frame(sqft_living=2160, condition=4), interval = 'confidence')
predict(full, data.frame(sqft_living=2160, condition=4, grade=6), interval = 'confidence')
predict(partial1, data.frame(sqft_living=2160, grade=6), interval = 'confidence')

