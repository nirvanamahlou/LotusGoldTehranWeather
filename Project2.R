
req <- c("dplyr", "tidyr","ggplot2", "stats", "tseries")
inst <- req[!req %in% installed.packages()[, "Package"]]
if (length(inst)) install.packages(inst)
invisible(lapply(req, require, character.only = TRUE))

weather <- read.csv("Tehran (Mehrabad Airport).csv", stringsAsFactors = FALSE)

names(weather) <- trimws(tolower(names(weather)))

weather$tmax <- as.numeric(weather$tmax)
weather$tmin <- as.numeric(weather$tmin)
weather$tm   <- as.numeric(weather$tm)
weather$rain <- as.numeric(weather$rain)

W <- weather[complete.cases(weather[, c("tmax","tmin","tm","rain")]), ]
W

cor_tm_rain <- cor(W$tm, W$rain)
cor_tm_rain


summary(lm(rain ~ tm, data = W))
mod1 <- lm(rain ~ tm,            data = W); summary(mod1)


ggplot(W, aes(x = tm, y = rain)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Rain vs. Mean Temperature in Tehran (Bahman 1401)",
       x = "Mean Temperature (°C)", y = "Rainfall (mm)") +
  theme_minimal()

