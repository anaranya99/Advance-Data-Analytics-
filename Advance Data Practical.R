n=100 ##No. of simulation
p=0.5
miss_prob=0.2  #Probability of misingness###
set.seed(1231)
##Generated random samples from a Bernoulli distribution
samples=rbinom(n,1,p)
##Generated missing data according to the missing completely at random##
missing_indices_mcar=sample(1:n, round(n * miss_prob), replace = FALSE)
samples_mcar <- samples
samples_mcar[missing_indices_mcar] <- NA
#Generate missing data according to the Missing At Random#
missing_indices_mar=sample(1:n, round(n * miss_prob), replace = FALSE)
missing_vals_mar=rbinom(length(missing_indices_mar), size = 1, prob = p)
samples_mar=samples
samples_mar[missing_indices_mar]=missing_vals_mar
# Calculate the mean using complete case analysis
mean_mcar=mean(samples_mcar, na.rm = TRUE)
mean_mar=mean(samples_mar, na.rm = TRUE)
# Display the results
cat("True Mean: ", p, "\n")
cat("Estimated Mean (MCAR): ", mean_mcar, "\n")
cat("Estimated Mean (MAR): ", mean_mar, "\n")
