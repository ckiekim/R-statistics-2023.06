##### 2-sample T test
# 1. mtcars 데이터셋에서 자동차 기어 종류(am: 오토/수동)에 따른 mpg의 차이가 
#    통계적으로 유의한지 t-test를 통해 확인해 보시오.
head(mtcars)
str(mtcars)
table(mtcars$am)        # 0(수동) - 19, 1(자동) - 13
shapiro.test(mtcars$mpg[mtcars$am == 0])    # p-value = 0.8987
shapiro.test(mtcars$mpg[mtcars$am == 1])    # p-value = 0.5363
t.test(mpg ~ am, data=mtcars)   # p-value = 0.001374
# 귀무가설 기각

# 2. MASS 패키지에 내장된 Cars93 데이터프레임에 대해서 생산국(Origin)이
#    USA vs. non-USA 2개의 group 에 대해서 차 가격(Price)의 평균이 차이가 있는지를
#    검정해보시오.
library(MASS)
head(Cars93)
str(Cars93)
table(Cars93$Origin)
t.test(Price ~ Origin, data=Cars93)     # p-value = 0.3428
# 귀무가설 채택

# 3. ggplot2 패키지에 내장된 mpg 데이터셋
# 3-1) subcompact 자동차와 midsize 자동차의 고속도로 연비
library(ggplot2)
head(mpg)
table(mpg$class)
subcompact = mpg[mpg$class == 'subcompact',]
midsize = mpg[mpg$class == 'midsize',]
t.test(subcompact$hwy, midsize$hwy)     # p-value = 0.3846
# 귀무가설 채택

# 3-2) 일반 휘발유(r)와 고급 휘발유(p)의 도시 연비
table(mpg$fl)
premium = mpg[mpg$fl == 'p',]
regular = mpg[mpg$fl == 'r',]
t.test(premium$cty, regular$cty)        # p-value = 0.2283
# 귀무가설 채택

# 3-3) subcompact 자동차의 전륜구동(f)이냐 후륜구동(r)이냐에 따른 도시 연비
table(subcompact$drv)
shapiro.test(subcompact$cty[subcompact$drv == 'f'])     # p-value = 0.09599
shapiro.test(subcompact$cty[subcompact$drv == 'r'])     # p-value = 0.1048
t.test(subcompact$cty[subcompact$drv == 'f'], subcompact$cty[subcompact$drv == 'r'])
# p-value = 1.759e-06, 귀무가설 기각


##### Paired T test
# 1. 당뇨병 치료제
before = c(51.4,52.0,45.5,54.5,52.3,50.9,52.7,50.3,53.8,53.1)
after = c(50.1,51.5,45.9,53.1,51.8,50.3,52.0,49.9,52.5,53.0)
t.test(before, after, paired=T, alternative='greater')  # p-value = 0.003105
# 귀무가설 기각

# 2. 신발 밑창
mat.A = c(13.2,8.2,10.9,14.3,10.7,6.6,9.5,10.8,8.8,13.3)
mat.B = c(14.0,8.8,11.2,14.2,11.8,6.4,9.8,11.3,9.3,13.6)
t.test(mat.A, mat.B, paired=T)      # p-value = 0.008539
# 귀무가설 기각

