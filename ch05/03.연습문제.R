##### 모비율 추정
# 1. 대중교통을 이용하여 등교하는 학생의 비율 p에 대한 신뢰도 95%의 신뢰 구간
# phat - 1.96 * sqrt(phat * (1-phat) / n) < p < phat + 1.96 * sqrt(phat * (1-phat) / n)
# 0.5 - 0.98 / sqrt(n) < p < 0.5 + 0.98 / sqrt(n)
phat = 0.5
n = 100
ll = phat - 1.96 * sqrt(phat * (1-phat) / n)    # ll = phat - 0.98 / sqrt(n)
ul = phat + 1.96 * sqrt(phat * (1-phat) / n)    # ul = phat + 0.98 / sqrt(n)
c(ll, ul)       # 0.402 0.598

# 2. 전체 고객의 새로운 메뉴에 대한 선호도를 p라 할 때, 
#    모비율 p에 대한 신뢰도 95%의 신뢰구간
phat = 0.8
n = 100
ll = phat - 1.96 * sqrt(phat * (1-phat) / n)
ul = phat + 1.96 * sqrt(phat * (1-phat) / n)
c(ll, ul)       # 0.7216 0.8784

# 3. 흡연율(모비율)에 대한 90% 신뢰구간
phat = 0.43
n = 1000
ll = phat - 1.645 * sqrt(phat * (1-phat) / n)
ul = phat + 1.645 * sqrt(phat * (1-phat) / n)
c(ll, ul)       # 0.4042464 0.4557536
