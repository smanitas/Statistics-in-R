# 1. Напишите функцию, которая на вход получает dataframe с двумя количественными переменными, а возвращает стандартизованные коэффициенты для регрессионной модели, в которой первая переменная датафрейма выступает в качестве зависимой, а вторая в качестве независимой.

beta.coef <- function (df){
  df <- scale (df,center= TRUE, scale=TRUE)
  df <- as.data.frame(df) 
  fit <- lm(df[,1]~df[,2],data=df)$coefficients 
  return(fit)
}

# 2. Напишите функцию normality.test, которая получает на вход dataframe с количественными переменными, проверяет распределения каждой переменной на нормальность с помощью функции shapiro.test. Функция должна возвращать вектор с значениями p - value, полученного в результате проверки на нормальность каждой переменной. Названия элементов вектора должны совпадать с названиями переменных. 

normality.test <- function(data) {
  p_values <- sapply(data, function(x) shapiro.test(x)$p.value)
  names(p_values) <- names(data)
  return(p_values)
}

# 3. Напишите функцию resid.norm, которая тестирует распределение остатков от модели на нормальность при помощи функции shapiro.test и создает гистограмму при помощи функции ggplot() с красной заливкой "red", если распределение остатков значимо отличается от нормального (p < 0.05), и с зелёной заливкой "green" - если распределение остатков значимо не отличается от нормального. На вход функция получает регрессионную модель. Функция возвращает переменную, в которой сохранен график ggplot.

library(ggplot2)

resid.norm <- function(model) {
  res <- residuals(model)
  norm_test <- shapiro.test(res)
  
  if (norm_test$p.value < 0.05) {
    col <- "red"
  } else {
    col <- "green"
  }
  
  ggplot(data.frame(res), aes(x=res)) +
    geom_histogram(aes(y=..density..), fill=col, alpha=0.5) +
    stat_function(fun=dnorm, args=list(mean=mean(res), sd=sd(res)), color="black", size=1) +
    ggtitle(paste0("Normality test: p-value = ", round(norm_test$p.value, 4)))
}

# 4. Напишите функцию high.corr, которая принимает на вход датасет с произвольным числом количественных переменных и возвращает вектор с именами двух переменных с максимальным абсолютным значением коэффициента корреляции 

high.corr <- function(x){    
  cr <- cor(x)    
  diag(cr) <- 0    
  return(rownames(which(abs(cr)==max(abs(cr)),arr.ind=T)))}