#sample data
#Button_color  Visitors  Clicks  Rate
#Green          8238     486      5.9%
#orange         7893     734      9.3%

a <- matrix(c(8238, 486, 0.059, 7893, 734, 0.093),
            2,3, byrow=T, dimnames=list(c("Green","Orange")))

#alpha level 95%
#check the statistical significance for click rate
#using Pearson's Chi-squared test
y <- matrix(c(a[1,2], a[2,2], (a[1,1]-a[1,2]), (a[2,1]-a[2,2])),2,2, byrow=T,dimnames=list(c("Click","nonClick"))) 
chisq.test(x=y)$p.value*100
#4.15193e-14
#p value is really small, so we are 95% confidence that
#there is dependency between color of the button and click rate


#check confidence interval
se <- c()
ci <- matrix(NA, 2,2)
for(i in 1:2) {
  se[i] <- sqrt( a[i,3] * (1-a[i,3]) / a[i,1] )
}
for(i in 1:2) {
  ci[i,] = c((a[i,3] - 1.64*se[i]) * 100, (a[i,3] + 1.64*se[i]) * 100)
}
#> confidence interval in 95%
#      min      max
#  g   5.474251 6.325749
#  o   8.763873 9.836127
#orange color button has the higher click rate 