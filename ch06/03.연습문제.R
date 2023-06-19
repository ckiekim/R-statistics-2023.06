##### 1-sample T test
# 1. 모집단의 평균이 1000 인가?
battery = c(980, 1008, 968, 1032, 1012, 996, 1021, 1002, 996, 1017)
# 정규성 검정
shapiro.test(battery)   # p-value = 0.9382
qqnorm(battery)
qqline(battery)
t.test(battery, mu=1000)    # p-value = 0.611
# 귀무가설 채택

# 2. 수학 평균이 55점보다 올랐나?
math = c(58, 49, 39, 99, 32, 88, 62, 30, 55, 65, 44, 55, 57, 53, 88, 42, 39)
shapiro.test(math)          # p-value = 0.1058
t.test(math, mu=55, alternative='greater')      # p-value = 0.4046
# 귀무가설 채택, 변화없다.

# 3. 모집단의 평균: 8.1
alcohol = c(15.50, 11.21, 12.67, 8.87, 12.15, 9.88, 2.06, 14.50, 0, 4.97)
shapiro.test(alcohol)       # p-value = 0.3863
t.test(alcohol, mu=8.1)     # p-value = 0.5301
# 귀무가설 채택