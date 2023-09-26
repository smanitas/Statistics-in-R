# 1. К частям таблицы можно обращаться так же, как и к матрицам. HairEyeColor - таблица с данными, встроенными в R. Ваша задача в переменную red_men сохранить долю рыжеволосых (Red) от общего числа голубоглазых мужчин.

red_men <- prop.table(HairEyeColor[,, "Male"], margin = 2)["Red", "Blue"]

# 2. Постройте столбчатую диаграмму распределения цвета глаз по цвету волос только у женщин из таблицы HairEyeColor. По оси X должен идти цвет волос, цвет столбиков должен отражать цвет глаз. По оси Y - количество наблюдений.

library("ggplot2")
mydata <- as.data.frame(HairEyeColor)
obj <- ggplot(data = subset(mydata, Sex=="Female"), aes(x = Hair, y = Freq, fill = Eye)) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_manual(values=c("Brown", "Blue", "Darkgrey", "Darkgreen"))

# 3. При помощи критерия Хи - квадрат проверьте гипотезу о взаимосвязи качества огранки бриллианта (сut) и его цвета (color) в таблице Diamonds. В переменную main_stat сохраните значение статистики критерия Хи - квадрат. Обратите внимание, main_stat должен быть вектором из одного элемента, а не списком (листом).

chisq_result <- chisq.test(diamonds$cut, diamonds$color)
main_stat <- chisq_result$statistic
main_stat <- c(main_stat) # преобразуем вектор в список

# 4. При помощи критерия Хи - квадрат проверьте гипотезу о взаимосвязи цены (price) и каратов (carat) бриллиантов. Сохраните в переменную main_stat значение критерия  Хи - квадрат.
#Пример перевода количественной шкалы в номинативную:
  #> x <- (1, 2, 3, 5, 6, 7) # mean(x) = 4
  #> factor_x <- (0, 0, 0, 1, 1, 1)

library(ggplot2)

# создаем переменные factor_price и factor_carat
diamonds$factor_price <- ifelse(diamonds$price >= mean(diamonds$price), 1, 0)
diamonds$factor_carat <- ifelse(diamonds$carat >= mean(diamonds$carat), 1, 0)

# проверяем таблицу с новыми переменными
head(diamonds)

# критерий Хи-квадрат для связи цены и каратов
table_price_carat <- table(diamonds$factor_price, diamonds$factor_carat)
main_stat <- chisq.test(table_price_carat)$statistic
main_stat

# 5. При помощи точного критерия Фишера проверьте гипотезу о взаимосвязи типа коробки передач (am) и типа двигателя (vs) в данных mtcars. Результат выполнения критерия сохраните в переменную.Получившийся p - уровень значимости сохраните в переменную fisher_test.

data(mtcars)
fisher_test <- fisher.test(mtcars$am, mtcars$vs)$p.value
