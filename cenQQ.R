#  cenQQ 1.1
#  Draw a QQ plot of censored data.  Lognormal is the default
#  Possible distributions are lognormal (lnorm), normal (norm) and gamma (gamma)
# v 1.0  5/09/2019
# v 1.1  8/27/2020  now runs even if all data are uncensored

cenQQ11 <- function(y.var, cen.var, dist = "lnorm", Yname = yname)  {
  yname <- deparse(substitute(y.var))
  cen.logical <- as.logical(cen.var)
  
  if (sum(as.integer(cen.var)) > 0)    # not all data are detects
  {var.choose <- distChooseCensored(y.var, cen.logical)
  norm.text <- paste("Shapiro-Francia W =", signif(var.choose$test.results$norm$statistic, 3) )
  lnorm.text <- paste("Shapiro-Francia W =", signif(var.choose$test.results$lnorm$statistic, 3) )
  gamma.text <- paste("Shapiro-Francia W =", signif(var.choose$test.results$gamma$statistic, 3) )
  
  if (dist == "norm") {
    qqPlotCensored(y.var, cen.logical, pch = 19, add.line = TRUE, line.col = "red", xlab = "Normal Quantiles", ylab = Yname, main = "Normal Q-Q Plot")
    mtext(norm.text)
    #  legend("bottomright", legend = norm.text)
  }
  
  if (dist == "lnorm")  {
    ylabel <- paste ("ln (", Yname, ")", sep = "")
    qqPlotCensored(y.var, cen.logical, pch = 19, add.line = TRUE, line.col = "red", distribution = "lnorm", xlab = "Normal Quantiles", ylab = ylabel, main = "Lognormal Q-Q Plot")
    mtext(lnorm.text)
    #  legend("bottomright", legend = lnorm.text)
  }
  
  if (dist == "gamma")  {
    qqPlotCensored(y.var, cen.logical, pch = 19, add.line = TRUE, line.col = "red", distribution = "gamma", estimate.params = TRUE, ylab = Yname, main = "Gamma Q-Q Plot")
    mtext(gamma.text)
    #   legend("bottomright", legend = gamma.text)
  }
  }      
  
  else    # all data are detects
  {var.choose <- distChoose(y.var, method = "sf", alpha = 0.05, choices = c("norm", "gamma", "lnorm"))
  norm.text <- paste("Shapiro-Francia W =", signif(var.choose$test.results$norm$statistic, 3) )
  lnorm.text <- paste("Shapiro-Francia W =", signif(var.choose$test.results$lnorm$statistic, 3) )
  gamma.text <- paste("Shapiro-Francia W =", signif(var.choose$test.results$gamma$statistic, 3) )
  
  if (dist == "norm") {
    EnvStats::qqPlot(y.var, pch = 19, add.line = TRUE, line.col = "red", xlab = "Normal Quantiles", ylab = Yname, main = "Normal Q-Q Plot")
    mtext(norm.text)
  }
  
  if (dist == "lnorm")  {
    ylabel <- paste ("ln (", Yname, ")", sep = "")
    EnvStats::qqPlot(y.var, pch = 19, add.line = TRUE, line.col = "red", distribution = "lnorm", xlab = "Normal Quantiles", ylab = ylabel, main = "Lognormal Q-Q Plot")
    mtext(lnorm.text)
  }
  
  if (dist == "gamma")  {
    EnvStats::qqPlot(y.var, pch = 19, add.line = TRUE, line.col = "red", distribution = "gamma", estimate.params = TRUE, ylab = Yname, main = "Gamma Q-Q Plot")
    mtext(gamma.text)
  } }
  
}
