Answer the following based on telecom data
# "Churn" refers to customers who have switched network operator. 
#All other variable describe customers and their usage of various services. 

#1. Does income affect churn?
#2. is there a gender preference for tollfree usage?
#3. Do married people with lower than average income churn more?
#4. Does long distance calling usage differ across regions?
#5. What are major factors which determine a customer category?(custcat)
#6. Does any of the following service show a relationship to churn?
#  Tollfree  equip  callcard  wireless

tel <- read.csv("Telecom_data.csv", header = T)
View(tel)
# Answer - 1
t.test(tel$income~tel$churn)
# Yes income affect churn.
# Low income people tend to churn more.

# Answer - 2
chisq.test(tel$gender,tel$tollfree)
# No, there is no gender preference. It is independent.
# Male(tollfree):Female(toll free) = Male( No tollfree):Female( No toll free) 

# Answer - 3 

tel1 <- tel[tel$marital==1, ]
View(tel1)

tel1$lowincome = ifelse( tel1$income < mean(tel1$income),1,0) 

chisq.test(tel1$churn,tel1$lowincome)


# Answer - 4
av <- aov(tel$longten~tel$region)
 summary(av)
# long distance calling is not different across regions.

# Answer - 5
chisq.test(tel$custcat,tel$region)

chisq.test(tel$custcat,tel$marital)
# Marital affects

chisq.test(tel$custcat,tel$ed)
# ed affects

chisq.test(tel$custcat,tel$retire)
# retire affects

chisq.test(tel$custcat,tel$gender)

chisq.test(tel$custcat,tel$tollfree)
# toll free affects

chisq.test(tel$custcat,tel$equip)
# equip affects

chisq.test(tel$custcat,tel$callcard)
# Callcard affects

chisq.test(tel$custcat,tel$wireless)
# wireless affects

chisq.test(tel$custcat,tel$multline)
chisq.test(tel$custcat,tel$voice)
chisq.test(tel$custcat,tel$pager)
chisq.test(tel$custcat,tel$internet)
chisq.test(tel$custcat,tel$callid)
chisq.test(tel$custcat,tel$callwait)
chisq.test(tel$custcat,tel$forward)
chisq.test(tel$custcat,tel$confer)
chisq.test(tel$custcat,tel$ebill)
chisq.test(tel$custcat,tel$churn)
# all of the above affects customer category.

# Except gender and region all affects the customer category.
 # Aliter

for(i in 1:30) {
  if(length(unique(tel[,i]))< 10) {
    pv = chisq.test(tel[,i],tel$custcat)$p.value
  }else {
    pv = summary(aov(tel[,15]~tel$custcat))[[1]][["pr(>F)"]][1]
  }

  if(pv< .05) {
  print(names(tel)[i])
  print(pv)
  
     }
}


  



# Answer 6
chisq.test(tel$churn, tel$tollfree)
# It is independent

chisq.test(tel$churn, tel$equip)
# There is a relationship

chisq.test(tel$churn, tel$callcard)
# There is a relationship

chisq.test(tel$churn, tel$wireless)
# There is a relationship

# Aliter

vars = c("tollfree","equip","callcard","wireless")
for(var in vars) {
  if(chisq.test(tel[,var],tel$churn)$p.value < .05) {
    print(var)
  print(table(tel[,var], churn = tel$churn))
    }
}

# Anova, Manova, Ancova, Mancova