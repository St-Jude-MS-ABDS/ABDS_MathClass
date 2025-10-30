#########################################
# Error Metrics


################################
# check for valid input

input.ok=function(theta,x)                              # theta to check
{
  if ((length(theta)!=1)||(!is.numeric(theta)))         # ensure theta is a numeric of length 1
      stop("theta must be a numeric with length 1.")    # stop if theta is not ok
  
  if (!is.numeric(x))                                   # ensure x is numeric
    stop("x must be numeric.")                          # stop if x is not numeric
}


################################
# mean absolute error

mean.absolute.error=function(theta,x)                 # theta: candidate summary value, x: numeric data vector
{
  input.ok(theta,x)                                   # check for valid theta
  res=mean(abs(x-theta),na.rm=T)                      # formula for mean absolute error
  return(res)                                         # return result
}

###################################
# root mean squared error

root.mean.squared.error=function(theta,x)             # theta: candidate summary value, x: numeric data vector
{
  input.ok(theta,x)                                   # check for valid theta   
  na=is.na(x)                                         # check for missing x
  x=x[!na]                                            # remove missing x
  n=length(x)                                         # determin sample size 
  res=sqrt(sum((x-theta)^2)/(n-1))                    # formula for root.mean.squared.error
  return(res)                                         # return result
}

#################################
# median absolute error

median.absolute.error=function(theta,x)               # theta: candidate summary value, x: numeric data vector
{
  input.ok(theta,x)                                   # check for valid input
  res=median(abs(x-theta),na.rm=T)                    # formula for median absolute error
  return(res)                                         # return result
}

################################
# root median squared error

root.median.squared.error=function(theta,x)           # theta: candidate summary value, x: numeric data vector
{
  input.ok(theta,x)                                   # check for valid input
  res=sqrt(median((x-theta)^2,na.rm=T))               # formula for root median squared error
  return(res)                                         # return result
}

################################
# maximum absolute error

max.absolute.error=function(theta,x)                  # theta: candidate summary value; x: numeric data vector
{
  input.ok(theta,x)                                   # check for valid input
  res=max(abs(x-theta),na.rm=T)                          # formula for max absolute error
  return(res)                                         # return result
}

#################################
# root trimmed squared error

root.trim50.squared.error=function(theta,x)         # theta: candidate summary value; x: numeric data vector

{
  input.ok(theta,x)                                  # check for valid input
  na=is.na(x)                                        # identify missing data in x
  x=x[!na]                                           # remove missing data from x
  d=sort((x-theta)^2)                                # compute and sort ll squared errors
  n=length(d)                                        # determine sample size
  k=ceiling(n/2)                                     # determine number of squared errors to include in sum
  res=sqrt(sum(d[1:k])/(k-1))                        # formula for root trimmed squared error
  return(res)                                        # return result
}

##################################
# plot error criteria as a function of theta for a particular set of data

plot.crit=function(x,cf,
                   theta.rng=range(x,na.rm=T),...)
  
{
  # check for valid theta.rng
  if ((length(theta.rng)!=2)||
      (!is.numeric(theta.rng))||
      (any(is.na(theta.rng))))
      stop("theta must be a numeric vector length 2 without any NAs.")
  
  # define sequence of 10,000 theta values for plotting
  theta=seq(from=theta.rng[1],  # start at theta.rng[1]
            to=theta.rng[2],    # end at theta.rng[2]
            length=10000)       # length 10000
  
  # compute criteria value at each theta value
  crit.value=rep(NA,length(theta))  # initialize vector for crit value           
  for (i in 1:length(theta))        # compute criteria value for each theta value
    crit.value[i]=cf(theta[i],x)
  
  # generate the plot
  plot(theta,crit.value,...)
  
  # for convenience, find the minimum among the plotted values
  best.index=which.min(crit.value)           # index of minimum crit.value
  best.theta=theta[best.index]               # theta for that index
  best.crit=crit.value[best.index]           # crit.value for that index  
  
  sc=deparse(sys.call())                     # get call string to annotate result (so criteria function name is known later)

  # package results in a list
  res=list(theta.crit=cbind(theta=theta,crit=crit.value), # table of theta and criteria values
           call.str=sc,                                   # system call string
           cf=cf,                                         # criteria function 
           best.theta=best.theta,                         # best theta among plotted values
           best.crit=best.crit)                           # minimum criteria among plotted values
  
  class(res)="plot.crit"
  
  return(res)                                       # return result
  
}

################################
# print function for plot.crit result
print.plot.crit=function(pc.res)
  
{
  res=list(best.theta=pc.res$best.theta,
           best.criteria=pc.res$best.crit,
           call.string=pc.res$call.str)
  print(res)
}