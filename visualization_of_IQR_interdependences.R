# This file runs the simulation for Interquartile Ranges
#   # This is based on Julian's plot for beta

# TODO: rewrite more generically, modification of plot based on argument instead of many else ifs 

# load packages

if (!'pacman' %in% installed.packages()[,'Package']) install.packages('pacman', repos='http://cran.r-project.org')
pacman::p_load(tidyverse,xtable,stabledist,plotly)

# functions

iqr <- function(var) {
    # Function to IQR as a function of beta
    # Accepts argument:
    #   var (character string): variable to be investigated  
    # No return value. Saves plot.

    # Standard parameter values
    g <- 1
    d <- 0
    a <- 1.1
    b <- 0.5

    # prepare plot
    IQR_3 <- matrix(NA, nrow = 201, ncol = 30)
    xcoord <- seq(1:30)/100
    iseq <- seq(1:201)
    zlabel <- "log(IQR)"
    xlabel <- "quantile"
    theta <- 55

    for(j in 1:30){
      print(j)
      Top <- (100 - j) / 100
      Bottom <- j / 100

      if (var == "beta") {
          bs <- (iseq - 101) / 100
          ycoord <- bs
          ylabel <- "beta"
          for (i in iseq){
            IQR_3[i, j] <- qstable(Top, a, bs[[i]], g, d) - qstable(Bottom, a, bs[[i]], g, d)
          }
      } else if (var == "alpha") {
          as <- iseq / 101
          ycoord <- as
          ylabel <- "alpha"
          for (i in iseq){
            IQR_3[i, j] <- qstable(Top, as[[i]], b, g, d) - qstable(Bottom, as[[i]], b, g, d)
          }
      } else if (var == "gamma") {
          gs <- iseq / 20
          ycoord <- gs
          ylabel <- "gamma"
          for (i in iseq){
            IQR_3[i, j] <- qstable(Top, a, b, gs[[i]], d) - qstable(Bottom, a, b, gs[[i]], d)
          }
      } else if (var == "beta_relativeIQR") {
          bs <- (iseq - 101) / 100
          ycoord <- bs
          ylabel <- "beta"
          zlabel <- "IQR(beta)/IQR(0)"
          theta <- 235
          for (i in iseq){
            IQR_3[i, j] <- (qstable(Top, a, bs[[i]], g, d) - qstable(Bottom, a, bs[[i]], g, d))/((qstable(Top, a, 0, g, d) - qstable(Bottom, a, 0, g, d)))
          }
      } else if (var == "alpha_beta") {
          bs <- (iseq - 101) / 100
          a_current <- j / 15
          Top <- 0.95
          Bottom <- 0.05
          ycoord <- bs
          xcoord <- seq(1:30)/15
          ylabel <- "beta"
          xlabel <- "alpha"
          #theta <- 235
          for (i in iseq){
            IQR_3[i, j] <- qstable(Top, a_current, bs[[i]], g, d) - qstable(Bottom, a_current, bs[[i]], g, d)
          }
      } else {
          # Unknown variable var supplied
          quit(-1)
      }
    }

    if (var != "beta_relativeIQR") {
      IQR_3 <- log(IQR_3)
    }
    
    pdf(paste(var, 'iqr2.pdf', sep="_"), height = 6, width = 8)
    persp(x=xcoord, y=ycoord, z=t(IQR_3), main="", zlab = zlabel, xlab=xlabel, ylab=ylabel, theta = theta, phi = 30, col = "blue", shade = 0.5, ticktype="detailed")
    
    dev.off()

}


# main entry point

iqr("alpha_beta")
iqr("beta_relativeIQR")
iqr("alpha")
iqr("beta")
iqr("gamma")
