#' Moderator plot
#'
#' Create moderator plot for two contrasts (i.e, PH vs. HW, PH vs. ctrl, and HW vs. ctrl)
#' @param dv the dependent variable
#' @param x the moderator
#' @param xlab the x-axis label
#' @param ylab the y-axis lable
#' @param ref the reference group
#' @param foc the focal group
#' @param mod the model
#' @export
#'
modPlot <- function(mod, x, dv, xlab, ylab, ref, foc){
  moderator <- seq(min(x, na.rm = T), max(x, na.rm = T), by = .05)
  params <- coef(mod)
  # M <- mean(eat_data$w1eatsym)
  focal <- params[[1]] + params[[2]]*M + params[[3]] + (params[[4]] + params[[5]])*moderator
  reference <- params[[1]] + params[[2]]*M + params[[4]]*moderator
  plot(dv ~  x, type = "n", xlab = xlab, ylab = ylab, ylim = c(min(focal, reference), max(focal, reference)))
  lines(x = moderator, y = focal, col = "black", lty = 1)
  lines(x = moderator, y = reference, col = "black", lty = 2)
  legend("topright", legend=c(foc, ref),
         col=c("black", "black"), lty=1:2, cex=0.8)
}
