knitr::opts_chunk$set(echo = TRUE, warning=FALSE, message=FALSE)
library(tidyverse)
library(pander)
library(DT)
### p.value.string v2
# by Reid Ginoza
p.value.string = function(p.value){
  p.value <- round(p.value, digits=4)
  if (p.value == 0) {
    return("p < 0.0001")
  } else {
    return(paste0("p = ", format(p.value, scientific = F)))
  }
}

# enter data
x <- c(33.6,	26.6,	23.3,	28.1,	43.1,	25.6,	31,	35.3,	30.5,	0,	37.6,	38,	27.1,	30.1,	25.8,	30,	45.8,	29.6,	43.3,	34.6,	39.3,	35.4,	39.8,	29,	36.6,	31.1,	39.4,	23.2,	22.2,	34.1,	36,	31.6,	24.8,	19.9,	27.6,	24,	33.2,	32.9,	38.2,	37.1,	34,	40.2,	22.7,	45.4,	27.4,	42,	29.7,	28,	39.1,	0,	19.4,	24.2,	24.4,	33.7,	34.7,	23,	37.7,	46.8,	40.5,	41.5,	0,	32.9,	25,	25.4,	32.8,	29,	32.5,	42.7,	19.6,	28.9,	32.9,	28.6,	43.4,	35.1,	32,	24.7,	32.6,	37.7,	43.2,	25,	22.4,	0,	29.3,	24.6,	48.8,	32.4,	36.6,	38.5,	37.1,	26.5,	19.1,	32,	46.7,	23.8,	24.7,	33.9,	31.6,	20.4,	28.7,	49.7)

y <- c(148,	85,	183,	89,	137,	116,	78,	115,	197,	125,	110,	168,	139,	189,	166,	100,	118,	107,	103,	115,	126,	99,	196,	119,	143,	125,	147,	97,	145,	117,	109,	158,	88,	92,	122,	103,	138,	102,	90,	111,	180,	133,	106,	171,	159,	180,	146,	71,	103,	105,	103,	101,	88,	176,	150,	73,	187,	100,	146,	105,	84,	133,	44,	141,	114,	99,	109,	109,	95,	146,	100,	139,	126,	129,	79,	0,	62,	95,	131,	112,	113,	74,	83,	101,	137,	110,	106,	100,	136,	107,	80,	123,	81,	134,	142,	144,	92,	71,	93,	122)

# put together as tibble
one <- tibble(x, y)
datatable(one)

# comment
means <- summarize(one, mean_x = mean(x, na.rm=TRUE), sd_x = sd(x, na.rm=TRUE), mean_y = mean(y, na.rm=TRUE), sd_y = sd(y, na.rm=TRUE))

one_model <- lm(y ~ x, data = one)
one_summary <- summary(one_model)
one_coef <- coefficients(one_model)
one_ci <- as_tibble(confint(one_model, level=0.95))


one_t <- as_tibble(one_summary[[4]])

# find predicted values
one$pred  <- predict(one_model)  

#total SS
totalSS = sum((y - means$mean_y)^2)

#regression SS
regSS = sum((one$pred- means$mean_y)^2)

#rsquared
rsquared = regSS / totalSS

# correlation - Pearson
one_corr <- cor(one$x, one$y, method="pearson")



one <- one %>% mutate(pred = predict(one_model))
one %>% ggplot(aes(x = x, y = y)) +
  geom_line(aes(y = pred), color = "red", size=1) +
  geom_point() +
  geom_text(aes(x = 46, y = 150, label = "Regression Line"), color="red", show.legend = TRUE) +
  scale_x_continuous(lim = c(0, 50), breaks = seq(min(0), max(200), by = 5), minor_breaks = seq(min(0), max(50), by = 15)) +
  xlab("BMI (kg/m²)") +
  ylab("Glucose Level (mg/dL)") +
  theme_minimal()









