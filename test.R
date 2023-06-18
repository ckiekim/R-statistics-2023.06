ad = read.csv('data/age.data.csv')
head(ad)
str(ad)
tail(ad)
ad$scale <- factor(ad$scale)

boxplot(age~scale, data=ad, col=rainbow(3))

# 등분산 검증
install.packages('lawstat')
library(lawstat)
levene.test(ad$age, ad$scale)

# Anova
ow = lm(age~scale, data=ad)
anova(ow)


### 적합도
# 멘델의 법칙
x <- c(315, 101, 108, 32)
chisq.test(x, p=c(9, 3, 3, 1)/16)

### 동질성 검정
sns.c <- read.csv("data/snsbyage.csv", 
                  header=T, stringsAsFactors=FALSE)
str(sns.c)
sns.c <- transform(sns.c, age.c = 
                       factor(age, levels=c(1, 2, 3), 
                              labels=c("20대", "30대", "40대")))

sns.c <- transform(sns.c, service.c = 
                       factor(service, levels=c("F", "T", "K", "C", "E"), 
                              ordered=TRUE))
c.tab <- table(sns.c$age.c, sns.c$service.c)
c.tab
ct.info = chisq.test(c.tab)
ct.info
ct.info$expected
ct.info$observed

# 독립성 검정
data("UCBAdmissions")
UCBAdmissions
ucba.tab = apply(UCBAdmissions, c(1,2), sum)
ucba.tab
round(prop.table(ucba.tab, margin=2) * 100, 1)
chisq.test(ucba.tab)
