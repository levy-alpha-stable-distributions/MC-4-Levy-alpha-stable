# Script to perform test of mean convergence speed for Levy alpha-stable, especially with low tail indices alpha.

# TODO: compute rate of convergence
# TODO: generalize if possible (more parameter values?)
# TODO: work with MC ensemble mean and standard error instead of single estimates

# load packages
if (!'pacman' %in% installed.packages()[,'Package']) install.packages('pacman', repos='http://cran.r-project.org')
pacman::p_load(StableEstim,parallel) 

# fix random seed
set.seed(3)

# prepare variables
x_sample_sizes <- 10^seq(3, 8, by = 0.05)
x_expectation <- 10^seq(2, 9, by = 0.05)
mean_estimates <- c()

# Levy alpha-stable parameter values
alpha = 1.1
beta = 1

# MC simulation for mean
mean_estimates <- mclapply(as.list(x_sample_sizes),function(x){mean(rstable(x, alpha = alpha, beta = beta))}, mc.cores = 4)   
mean_estimates <- unlist(mean_estimates)

# compute theoretical mean from 
mean_expectation <- x_expectation*0+0-1*1*tan(pi*1.1/2)


# plot
pdf("stable_mean_convergence.pdf", height=5, width=7) 
plot(x_sample_sizes, mean_estimates, col = 'red', log="xy", xlab="Sample size", ylab="Mean estimate")
lines(x_expectation, mean_expectation, col="black")
m<-nls(log(mean_estimates)~log(mean_expectation[[1]])-a/log(x_sample_sizes))
lines(x_sample_sizes, exp(predict(m)), lty=1, col="red", lwd=3)
#abline(lm(log(mean_estimates) ~ log(x_sample_sizes)), col = 'red')
dev.off()
