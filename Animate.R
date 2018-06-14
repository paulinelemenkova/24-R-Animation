# "Скрипичный график" - график распределения значений данных (значения среднего, станд. отклонение).
# ЧАСТЬ-1. готовим датафрейм. 
	# шаг-1. вчитываем таблицу с данными. делаем из нее исходный датафрейм. чистим датафрейм от NA
MDepths <- read.csv("Depths.csv", header=TRUE, sep = ",")
Ml <- na.omit(MDepths) 
row.has.na <- apply(Ml, 1, function(x){any(is.na(x))}) 
sum(row.has.na) 
head(Ml)

MDTt = melt(setDT(Ml), measure = patterns("^profile"), value.name = c("depth"))
head(MDTt)


# ЧАСТЬ-2. делаем анимацию : как изменяются глубины по профилям 1:25.
library(ggplot2)
library(gganimate)
library(gapminder)
theme_set(theme_bw())  # pre-set the bw theme.

Observations<- MDTt$observ
Depth<- MDTt$depth
Profiles<- MDTt$variable

# 1 вариант - одноцветный, с одним методом

g <- ggplot(MDTt, aes(x = Observations, y = Depth, frame = Profiles)) +
  geom_point() +
  geom_smooth(aes(group = Profiles), method = "loess", show.legend = TRUE)
gganimate(g, interval=1.0)

# 2 вариант - четырехцветный, с 4 методами

g <- ggplot(MDTt, aes(x = Observations, y = Depth, frame = Profiles, color = "Observation points")) +
	geom_point() +
	geom_smooth(aes(group = Profiles), method = "loess", show.legend = TRUE) +
	geom_smooth(aes(group = Profiles, colour = "Loess method"), method = loess, se = TRUE, span = .4, size=.2, linetype = "solid", show.legend =  TRUE) +
	geom_smooth(aes(group = Profiles, colour = "Glm method"), method = glm, se = TRUE, span = .4, size=.2, linetype = "dotted", show.legend = TRUE) +
	geom_smooth(aes(group = Profiles, colour = "Lm method"), method = lm, se = TRUE, size=.2, linetype = "solid", show.legend = TRUE) +
	scale_color_manual(name = "Legend:", values = c("Observation points" = "seagreen", "Loess method" = "red", "Glm method" = "orange", "Lm method" = "blue", "Quantiles" = "purple"))
    
gganimate(g, interval=1.0)



