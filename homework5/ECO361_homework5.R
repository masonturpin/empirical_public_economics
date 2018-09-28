eight <- c(169, 165, 134, 123, 113, 111)
six <- c(141, 131, 121, 112, 104, 104)
year <- c(1929, 1930, 1931, 1932, 1933, 1934)
data <- data.frame(year, six, eight)
library(ggplot2)
library(ggthemes)
trendsFigure <- ggplot(data.mason, aes(x = year, y = banks, linetype = district)) + geom_point() + geom_line(aes(y = six), lty = 2) +
  geom_point(aes(y = eight)) + geom_line(aes(y = eight)) +
  ggtitle("Trends in Bank Failures in the Sixth and Eighth Federal Reserve Districts") +
  ylab("Number of banks in business") + xlab("Year") + theme_tufte()
trendsFigure
library(stargazer)
stargazer(data, type = "html", title = "Number of Banks Operating", digit.separator = "", 
          out = "bankfailures.html", covariate.labels = c("Year", "Sixth District", 
                                                          "Eighth District"), 
          rownames = FALSE, summary = FALSE)
#estimating equation 5.3
year1 <- c(1929, 1930, 1931, 1932, 1933, 1934, 1929, 1930, 1931, 1932, 1933, 1934)
banks <- c(169, 165, 134, 123, 113, 111, 141, 131, 121, 112, 104, 104)
banksap <- c(169, 165, 132, 123, 113, 111, 141, 135, 121, 112, 104, 104)
treat <- c(0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1)
post <- c(0, 0, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1)
district <- c("Eighth District", "Eighth District", "Eighth District", "Eighth District", "Eighth District", "Eighth District",
          "Sixth District", "Sixth District", "Sixth District", "Sixth District", "Sixth District", "Sixth District")
#using my data
data.mason <- data.frame(year1, banks, treat, post, district, stringsAsFactors = TRUE)
equation.mason <- lm(banks ~ treat + post + treat*post, data.mason)
summary(equation.mason)
#using data closer to ap
data.ap <- data.frame(year1, banksap, treat, post)
equation.ap <- lm(banksap ~ treat + post + treat*post, data.ap)
summary(equation.ap)
#making a regression table
stargazer(equation.mason, equation.ap, type = "html", out = "regressionoutput.html", 
          title = "Equation 5.3 Estimates",
          covariate.labels = c("Treatment", "Post", "Treatment * Post"),
          dep.var.labels = c("My Estimate", "AP Estimate"),
          dep.var.caption = "Number of Banks Operating")

ggplot(data.mason, aes(x = year1, y = banks, linetype = district)) + 
  geom_point(stat = "identity") +
  geom_line() +
  labs(x="Year", y="Number") +
  scale_linetype_discrete(name="District") +
  theme_tufte()
