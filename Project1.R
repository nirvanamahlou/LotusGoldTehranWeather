pk <- c("lubridate","dplyr","ggplot2","xts","tseries","seastests","aTSA",
        "forecast","PerformanceAnalytics","Kendall")
ni <- pk[!pk %in% installed.packages()[,"Package"]]
if(length(ni)) install.packages(ni)
invisible(lapply(pk, require, character.only = TRUE))


f  <- "E:/Courses/CS/Lotus Gold Com.ETF.csv"
raw <- read.delim(f, header = TRUE, sep = ",", stringsAsFactors = FALSE,
                  check.names = FALSE, fileEncoding = "UTF-8-BOM",
                  strip.white = TRUE)

dt  <- raw %>% mutate(d = ymd(DTYYYYMMDD), p = as.numeric(CLOSE)) %>%
  filter(!is.na(d) & !is.na(p)) %>% arrange(d)

e   <- max(dt$d); s <- e %m-% months(11)
d12 <- dt %>% filter(d >= s & d <= e)

mo  <- d12 %>% mutate(m = floor_date(d, "month")) %>%
  group_by(m) %>% summarise(p = mean(p), .groups = "drop")

tsd <- ts(mo$p, start = c(year(mo$m[1]), month(mo$m[1])), frequency = 12)

ggplot(mo, aes(m, p)) +
  geom_line(color = "steelblue", linewidth = 1.2) +
  geom_point(color = "dodgerblue4", size = 2) +
  scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m") +
  labs(title = "Lotus Gold ETF – Monthly", x = "Month", y = "Price") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"))

print(adf.test(tsd))
print(pp.test(tsd))
print(kpss.test(tsd))
print(MannKendall(tsd))
mf <- factor(cycle(tsd)); print(kruskal.test(as.numeric(tsd) ~ mf))
print(isSeasonal(tsd, test = "kw"))

acf_vals <- acf(tsd, lag.max = 10, plot = FALSE)$acf[-1]
print(setNames(round(acf_vals, 3), paste0("lag", 1:10)))
acf(tsd, main = "ACF")
pacf_vals <- pacf(tsd, lag.max = 10, plot = FALSE)$acf
print(setNames(round(pacf_vals, 3), paste0("lag", 1:10)))
pacf(tsd, main = "PACF")
last_month <- max(mo$m)

target_end <- ymd("2025-04-20")


h <- interval(last_month, target_end) %/% months(1) + 1

fc <- forecast(fit, h = h)

fit <- auto.arima(tsd, seasonal = TRUE, stepwise = FALSE, approximation = FALSE)


autoplot(tsd, colour = "red", size = 1) +
  autolayer(fc, series = "Forecast", colour = "steelblue", size = 1.2, alpha = .6) +
  labs(title = "Lotus Gold ETF – Forecast",
       x = "Month", y = "Price") +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"),
        legend.title = element_blank(),
        panel.grid.minor = element_blank())


lr  <- diff(log(d12$p))
sdm <- sd(lr) * sqrt(22)
sh  <- SharpeRatio.annualized(xts(lr, order.by = d12$d[-1]))
cat(round(sdm, 4), round(as.numeric(sh), 4), "\n")

print(sh)


