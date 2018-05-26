###                    IRF.PLOT.SVAREST             ###
###  Plot impulse response functions and their CIs  ###
###       To be used only with svarest objects      ###

require("vars")

# This function plot ALL shocks from a SVAR model. It cannot be used if one
# is just interested in some of the shocks.

# Note: titles may be changed in order to fit individual plots' size, e.g., removing "shock from"

# Arguments to be used:
# model: svarest object;
# n.ahead: time ahead after the shocks;

# Default values are:
# – bootstrap = TRUE, bootstraping method to get the confidence intervals;
# – cumul = TRUE, cumulative impulse response function;
# – ci = 0.95, confidence interval is 0.95;
# – zero = FALSE; if TRUE, force abline(0) to be displayed;
# – linte.type = 3, dotted line type (see "lty" in par{graphics} for more info)

irf.plot.svarest <- function(model, n.ahead, bootstrap = TRUE, cumul = TRUE, ci = 0.95, zero = FALSE, line.type = 3){
  # Computing the impulse response function
  IRF <- irf(model, n.ahead = n.ahead, boot = bootstrap, cumulative = cumul, ci = ci)
  # Preparing the plots
  names <- colnames(model$var$y)
  M <- length(names)
  # Plotting the IRFs
  par(mfrow=c(M,M),oma = c(2,2,1,1), mar =c(2,2,2,2), mgp = c(2,1,0), xpd = FALSE)
  for (i in 1:M){
    for (j in 1:M){
      if (zero == FALSE) {
        ymax <- max(IRF$irf[[i]][,j], IRF$Lower[[i]][,j], IRF$Upper[[i]][,j])
        ymin <- min(IRF$irf[[i]][,j], IRF$Lower[[i]][,j], IRF$Upper[[i]][,j])
        plot(x = seq(along.with = IRF$irf[[i]][,j]), y = IRF$irf[[i]][,j], type = "n", ylim = c(ymin, ymax), xlab = "",
             ylab= "", main = paste0("Shock from ", names[i], " to ", names[j]))
        polygon(c(x = seq(along.with = IRF$irf[[i]][,j]), rev(seq(along.with = IRF$irf[[i]][,j]))), c(IRF$Lower[[i]][,j], rev(IRF$Upper[[i]][,j])),
                col = "gray90", border = NA)
        lines(x = c(0,seq(along.with = IRF$irf[[i]][,j])), y = rep(0, 1 + length(IRF$irf[[i]][,j])), col = "black")
        lines(x = seq(along.with = IRF$irf[[i]][,j]), y = IRF$irf[[i]][,j], col = "blue")
        lines(x = seq(along.with = IRF$irf[[i]][,j]), y = IRF$Lower[[i]][,j], col = "red", lty = line.type)
        lines(x = seq(along.with = IRF$irf[[i]][,j]), y = IRF$Upper[[i]][,j], col = "red", lty = line.type)
      }
      else {
        ymax <- max(IRF$irf[[i]][,j], IRF$Lower[[i]][,j], IRF$Upper[[i]][,j], 0)
        ymin <- min(IRF$irf[[i]][,j], IRF$Lower[[i]][,j], IRF$Upper[[i]][,j], 0)
        plot(x = seq(along.with = IRF$irf[[i]][,j]), y = IRF$irf[[i]][,j], type = "n", ylim = c(ymin, ymax), xlab = "",
             ylab= "", main = paste0("Shock from ", names[i], " to ", names[j]))
        polygon(c(seq(along.with = IRF$irf[[i]][,j]), rev(seq(along.with = IRF$irf[[i]][,j]))), c(IRF$Lower[[i]][,j], rev(IRF$Upper[[i]][,j])),
                col = "gray90", border = NA)
        lines(x = c(0,seq(along.with = IRF$irf[[i]][,j])), y = rep(0, 1 + length(IRF$irf[[i]][,j])), col = "black")
        lines(x = seq(along.with = IRF$irf[[i]][,j]), y = IRF$irf[[i]][,j], col = "blue")
        lines(x = seq(along.with = IRF$irf[[i]][,j]), y = IRF$Lower[[i]][,j], col = "red", lty = line.type)
        lines(x = seq(along.with = IRF$irf[[i]][,j]), y = IRF$Upper[[i]][,j], col = "red", lty = line.type)
      }
    }
  }
  par(mfrow=c(1,1))
}
