library ("lmtest")

data = swiss
help(swiss)

# Высчитывание среднего значения переменных Agriculture, Fertility, Education
data["Education1"] = (sum(data$Education)/47)
print((sum(data$Education)/47))
data["Fertility1"] = mean(data$Fertility)
print(data$Fertility1)
data["Agriculture1"] = mean(data$Agriculture)
print(data$Agriculture1)

# Высчитывание дисперсии переменных Agriculture, Fertility, Education
data["Education2"] = var(data$Education)
data["Fertility2"] = var(data$Fertility)
data["Agriculture2"] = var(data$Agriculture)

# Высчитывание СКО переменных Agriculture, Fertility, Education
data["Education3"] = sqrt(var(data$Education))
data["Fertility2"] = sqrt(var(data$Fertility))
data["Agriculture2"] = sqrt(var(data$Agriculture))

# Постороение модели вида y = a + bx, где y - Education, x - Agriculture
model1 = lm(Education~Agriculture, data)
model1
summary(model1)

# Команда summary(model) позволяет нам увидеть коэффициенты a, b, для того
# чтобы составить модель вида y = a + bx. Здесь a = 24.69 (Intercept),
# b = -0.27(Agriculture). y = 24.69 - 0.27x

plot(swiss) + abline(a = 24.69, b = -0.27, col = "red")

# Оценка модели y = a + bx, где y - Education, x - Agriculture по коэффициенту детерминации:
# Так как мы описываем Education, всего одним параметром (Agriculture), то коэффициент детерминации
# очевидно вряд ли может быть высоким. В нашем случае R^2 = около 40 %. Если брать в расчет, что Education
# описывается лишь одним параметром, то модель не такая уж и плохая.

# Существует ли взаимосвзяь между объясняемой переменной(Education) и объясняющей(Agriculture)?
# Определенно есть, ведь по вероятности, а именно по большому кол-ву звездочек у регрессора, мы
# можем сказать, что уровень образования довольно сильно зависит от образа жизни(Agriculture).

#------------------------------------------------------------------------------------------------------------#

# Постороение модели вида y = a + bx, где y - Education, x - Fertility
model2 = lm(Education~Fertility, data)
model2
summary(model2)

# Команда summary(model2) позволяет нам увидеть коэффициенты a, b, для того
# чтобы составить модель вида y = a + bx. Здесь a = 46.82 (Intercept),
# b = -0.51(Fertility). y = 46.82 - 0.51x

plot(swiss) + abline(a = 46.82, b = -0.51, col = "green")

# Оценка модели y = a + bx, где y - Education, x - Fertility по коэффициенту детерминации:
# Так как мы описываем Education, всего одним параметром (Fertility), то коэффициент детерминации
# очевидно вряд ли может быть высоким. В нашем случае R^2 = около 43 - 44 %. Если брать в расчет, что Education
# описывается лишь одним параметром, то модель не такая уж и плохая.

# Существует ли взаимосвзяь между объясняемой переменной(Education) и объясняющей(Fertility)?
# Определенно есть, ведь по вероятности, а именно по большому кол-ву звездочек у регрессора, мы
# можем сказать, что уровень образования довольно сильно зависит от коэффициента рождаемости(Fertility).
