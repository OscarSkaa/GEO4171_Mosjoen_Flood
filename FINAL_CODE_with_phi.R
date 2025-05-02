# -*- coding: UTF-8 -*-
####################################################################
# GEO4171 - Flood frequency analysis at TollC%ga (Station 161.7.0)
####################################################################

# --- Input Data File ---
csv_file <- "161_7_0_Vannforing_dogn_v1.csv"

# --- 1. Load Required Packages ---
pkgs <- c("nsRFA", "plotrix", "lubridate", "evir", "readr", "ggplot2")
new <- pkgs[!(pkgs %in% installed.packages()[,"Package"])]
if(length(new)) install.packages(new)
invisible(lapply(pkgs, library, character.only = TRUE))

# --- 2. Load Course Macros ---
source("GEO4171_macro.R")  # Must be in working directory

# --- 3. Load and Clean Daily Discharge Data ---
raw <- read_csv2(csv_file, skip = 2,
                 col_names = c("Date", "Q", "Corr", "Qual"),
                 locale = locale(decimal_mark = ",", encoding = "latin1"))
raw$Date <- as.Date(raw$Date)
raw$Q[raw$Q < 0] <- NA

# --- 3b. Remove years with too many missing daily values (>5%) ---
raw$Year <- lubridate::year(raw$Date)
na_stats <- aggregate(Q ~ Year, data = raw, function(x) mean(is.na(x)))
bad_years <- na_stats$Year[na_stats$Q > 0.05]
if (length(bad_years) > 0) {
  cat("Removing years with >5% missing data:", bad_years, "\n")
  raw <- raw[!raw$Year %in% bad_years, ]
}

# Final cleanup
raw$DateInt <- as.integer(format(raw$Date, "%Y%m%d"))
daily_mat <- as.matrix(raw[, c("DateInt", "Q")])

# --- 4. Extract Annual Maximum Series (AMS) ---
ams <- get_ams(daily_mat)

# --- 4b. Attach correction and control status to AMS data ---
raw$Corr <- as.numeric(raw$Corr)
raw$Qual <- as.numeric(raw$Qual)
correction <- as.matrix(raw[, c("DateInt", "Corr")])
quality    <- as.matrix(raw[, c("DateInt", "Qual")])

amsdata <- list()
amsdata$amsdates <- ams$amsdates
amsdata$correction <- correction[match(ams$amsdates, correction[,1]), 2]
amsdata$quality    <- quality[match(ams$amsdates, quality[,1]), 2]

cat("\nAMS correction flags:\n")
print(cbind(amsdata$amsdates, amsdata$correction))
cat("\nAMS control status (quality flags):\n")
print(cbind(amsdata$amsdates, amsdata$quality))

valid_idx <- which(amsdata$correction %in% c(0, 3) & amsdata$quality %in% c(2, 3))
if (length(valid_idx) < length(amsdata$amsdates)) {
  cat("\n⚠️ Excluding", length(amsdata$amsdates) - length(valid_idx), "AMS points due to unacceptable correction/control flags.\n")
  excluded_dates <- amsdata$amsdates[-valid_idx]
  excluded_q <- ams$ams[-valid_idx]
  print(data.frame(Date = as.Date(as.character(excluded_dates), "%Y%m%d"), 
                   Q = excluded_q,
                   Correction = amsdata$correction[-valid_idx],
                   ControlStatus = amsdata$quality[-valid_idx]))
}
ams <- list(
  amsdates = ams$amsdates[valid_idx],
  ams = ams$ams[valid_idx]
)

cat("Years used in AMS:\n")
print(lubridate::year(as.Date(as.character(ams$amsdates), "%Y%m%d")))

# --- 5. Plot Time Series with AMS Peaks ---
png("timeseries_AMS.png", width = 1800, height = 600, res = 220)
ggplot(raw, aes(Date, Q)) +
  geom_line(color = "grey50") +
  geom_point(data = data.frame(date = as.Date(as.character(ams$amsdates), "%Y%m%d"),
                               Q = ams$ams),
             aes(date, Q), color = "red", size = 2) +
  labs(title = "Tollaaga | Daily Discharge and AMS Peaks",
       x = "Year",
       y = expression("Discharge (m"^3*"/s)"))
dev.off()

# --- 6. Plot Flood Rose ---
png("flood_rose.png", 1200, 1200, res = 220)
plot_season_floods(daily_mat)
title("Tollaaga | Flood Rose (All Years)", cex.main = 1.4)
dev.off()

# --- 7. Fit Bayesian GEV Distribution ---
my_prior <- function(par) dnorm(par[3], 0, 0.2)
fit <- BayesianMCMC(xcont = ams$ams, dist = "GEV",
                    apriori = my_prior, confint = c(0.025, 0.975),
                    varparameters0 = c(NA, NA, 0.01))

# --- 8. GEV Parameter Table with 95% CI ---
param_ci <- t(get_parameter_ci(fit, q = c(0.025, 0.975)))
param_tbl <- data.frame(Parameter = c("mu", "sigma", "xi"),
                        Mode = round(fit$parametersML, 3),
                        CI_low = round(param_ci[,1], 3),
                        CI_high = round(param_ci[,2], 3))
cat("\nGEV parameters with 95% CI:\n")
print(param_tbl, row.names = FALSE)

# --- 9. GEV Diagnostic Plots ---
png("GEV_diagnostics.png", 1800, 1200, res = 220)
par(mfrow = c(2, 2), mar = c(4, 4, 3, 2))
gev.diag.bayes(fit)
dev.off()
par(mfrow = c(1, 1))

# --- 10. Return-Level Plot with Confidence Bands and AMS Points ---
png("return_level.png", 1600, 1200, res = 220)
plot.new()
Rx <- fit$returnperiods
Qy <- fit$quantilesML
plot(Rx, Qy, type = "l", log = "x",
     xlim = c(1, 1200),
     ylim = range(fit$intervals, ams$ams),
     xlab = "Return period (years)",
     ylab = expression("Discharge (m"^3*"/s)"),
     main = "Return-Level Plot with 95% Credible Band")
lines(Rx, fit$intervals[1,], lty = 2)
lines(Rx, fit$intervals[2,], lty = 2)
n <- length(ams$ams)
p <- (1:n) / (n + 1)
T_emp <- 1 / (1 - p)
points(T_emp, sort(ams$ams), pch = 1)
axis(1, at = c(1, 2, 5, 10, 20, 50, 100, 200, 500, 1000),
     labels = c(1, 2, 5, 10, 20, 50, 100, 200, 500, 1000))
dev.off()

# --- 11. Calculate Design Floods for TEK-17 Classes ---
R <- c(20, 200, 1000)
Qb <- splinefun(Rx, Qy)(R)
Ql <- splinefun(Rx, fit$intervals[1,])(R)
Qh <- splinefun(Rx, fit$intervals[2,])(R)
design_tbl <- data.frame(
  Safety = c("F1_small", "F2_medium", "F3_large"),
  Return = R,
  Q_low = round(Ql, 1),
  Q_best = round(Qb, 1),
  Q_high = round(Qh, 1)
)
cat("\nDesign floods (95% CI):\n")
print(design_tbl, row.names = FALSE)

# --- 12. Adjust Design Floods to Target Site (MosjC8en) ---

# 12.1 Median Discharges
QM_d <- 72.7
QM_t <- 949.0
SF <- QM_t / QM_d
design_tbl$Q_target_daily <- round(SF * design_tbl$Q_best, 1)

# --- 12b. Flood dominance check and phi computation ---
target_csv <- "151_28_0_Vannforing_dogn_v1_mosjoen.csv"
mos <- read_csv2(target_csv, skip = 1,
                 locale = locale(decimal_mark = ",", encoding = "latin1"),
                 col_names = c("Date", "Q", "Corr", "Qual"))
mos$Date <- as.Date(lubridate::ymd_hms(mos$Date))

mos$Q <- as.numeric(gsub(",", ".", as.character(mos$Q)))
mos$Year <- lubridate::year(mos$Date)
mos$Month <- lubridate::month(mos$Date)

monthly_avg <- aggregate(Q ~ Year + Month, data = mos, mean, na.rm = TRUE)
W_Nov <- mean(monthly_avg$Q[monthly_avg$Month == 11], na.rm = TRUE)
P_Jun <- mean(monthly_avg$Q[monthly_avg$Month == 6], na.rm = TRUE)
p_val <- 1 / (1 + exp(-(24.08 - 3.66 * exp(W_Nov / 100) - 4.28 * log(P_Jun))))
cat("\nFlood Type Assessment:\n")
cat(sprintf("Mean November discharge: %.2f\n", W_Nov))
cat(sprintf("Mean June discharge: %.2f\n", P_Jun))
cat(sprintf("Calculated p: %.4f → %s dominated\n",
            p_val, ifelse(p_val > 0.5, "RAIN", "SNOWMELT")))

# --- 12c. Compute phi for snowmelt (adjust if needed) ---
phi_snowmelt <- function(QN, A, ASE) {
  t <- -1.38 + 0.979 * log10(QN) - 0.668 * log10(A) - 0.858 * sqrt(ASE)
  return(1 + exp(t))
}
QN_demo <- 48.9
A_demo <- 4119
ASE_demo <- 0.20
phi <- phi_snowmelt(QN_demo, A_demo, ASE_demo)
cat(sprintf("Hourly peak factor (phi): %.3f\n", phi))

# --- 12.4 Final peak flood values ---
design_tbl$Q_target_peak <- round(phi * design_tbl$Q_target_daily, 1)
cat("\nAdjusted design floods for MosjC8en (95% CI on donor values only):\n")
print(design_tbl[, c("Safety", "Return", "Q_target_daily", "Q_target_peak")], row.names = FALSE)
