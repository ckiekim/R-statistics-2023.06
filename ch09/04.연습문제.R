# Q-1
x = c(150, 160, 170, 180, 190)
y = c(176, 179, 182, 181, 185)
q1 = lm(y ~ x)
q1$coefficients
height = q1$coefficients[2] * 165 + q1$coefficients[1]
height

# Q-2
x = c(100, 200, 300, 400, 500)
y = c(30, 70, 85, 140, 197)
q2 = lm(y ~ x)
q2$coefficients
card = q2$coefficients[2] * 250 + q2$coefficients[1]
card

# Q-3
head(mtcars)
q3 = lm(hp ~ disp, data=mtcars)
q3$coefficients
# hp = 0.4376 * disp + 45.7345

plot(hp ~ disp, data=mtcars)
abline(q3$coefficients)
