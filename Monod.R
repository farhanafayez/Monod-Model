# ______For reactor 1________

# Reading the file as a dataframe, including the headers
DF <- read.table('Monod.txt', header = TRUE)
par(mfrow = c(1,2))

# Transformed the original model into a model that is linear with respect to some new parameters
# Making the equation linear, by multiplying with the denominator
# Had some issues with decimals in the dataset, changed them as '.' instead of ','
x1 <- DF$y1
x2 <- (DF$S1)*(DF$y1)
y <-  DF$S1

# We have no intercept so using -1
model <- lm(y~x1+x2-1)

b_1 <- coefficients(model)[1]
b_2 <- coefficients(model)[2]

theta_1 <- 1 / b_2
theta_2 <- b_1 / (1 / b_2)

# make sure the length  of seqc and y_model and match when deciding the sequence
seqc <- seq(0, 165, 0.5)
y_model <- (theta_1*seqc)/(theta_2+seqc)
with(DF, plot(S1,y1))
lines(seqc,y_model,col='hotpink',lwd=2)

# ______For reactor 2________

# Reading the file as a dataframe, including the headers
DF <- read.table('Monod.txt', header = TRUE)
par(mfrow = c(1,2))


# Making the equation linear, by multiplying with the denominator
# Had some issues with decimals, changes them as '.' instead of ','
x1_2 <- DF$y2
x2_2 <- (DF$S2)*(DF$y2)
y_2 <-  DF$S2

# We have no intercept so using -1
model <- lm(y~x1+x2-1)

b_1_2 <- coefficients(model)[1]
b_2_2 <- coefficients(model)[2]

theta_1_2 <- 1 / b_2_2
theta_2_2 <- b_1_2 / (1 / b_2_2)

# make sure the length  of seqc and y_model and match when deciding the sequence
seqc_2 <- seq.int(0, 165, .5)
y_model_2 <- (theta_1_2*seqc_2)/(theta_2_2+seqc_2)
with(DF, plot(S2,y2))
lines(seqc_2,y_model_2,col='hotpink',lwd=2)

# The goodness of fit can have a hypothesis that:
# H0: the model fits 
# H1: the model does not fit
# It compares the observed values to the expected fitted values.
# Inorder to obtain this we would have to conduct the chi square goodness of fit
# But by observing the graph we could see that reactor 2 has the best fit model, as it shows lesser residual error.
