beta = 0.5
b = 2
sigma = 2
a = 1.1

#Estimation functions
#Estimation using the conditional expectation of Y|X
f_condexp = function(x){beta*x}

#TODO: Put your function in here.  You can reference a,b,sigma, and it will just pull them from
# the outside namespace
f_yours = function(x){beta*x+0.5*sigma^2*a}

#Define the loss function, where z = y - yhat
loss = function(z){b*(exp(a*z)-a*z-1)}

#Simulation to see how you do
reps = 1000
#Just generate the X variables normally.  You don't really care
x = rnorm(reps,0,1)
#Generate the Y variables from our normal model
y = rnorm(reps,x*beta,sigma)
#Compute the losses
condexp_loss = sapply(y-f_condexp(x),loss)
your_loss = sapply(y-f_yours(x),loss)
print(paste("Average loss of the conditional expectation:", round(mean(condexp_loss),3)))
print(paste("Average loss of your method:", round(mean(your_loss),3)))

