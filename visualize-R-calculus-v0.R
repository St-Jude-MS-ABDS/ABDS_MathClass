###################################
# Visulaize R calculus
# A library of R functions for students to visualize calculus concepts

##########################
# Newton's method:
# Use a series of iterative local linear approximations of 
# a function f to solve the equation f(x)=0

newton=function(f,             # the function to be solve f(x)=0
                x0,            # an initial guess at x=0
                h=0.001,       # h in the limit definition of the derivative
                max.its=100,   # maximum number of iterations (to ensure computations will stop if solution doesn't exist)
                eps=1e-6,      # desired accuracy for x in solving f(x)=0
                delta=1e-6,    # desired accuracy for y in solving y=f(x)=0
                ...)           # additional arguments to the function f
{
  y0=f(x0,...); y1=f(x0+h,...)    # compute y0=f(x) and y1=f(x+h)
  m=(y1-y0)/h                     # compute the slope of the line in the limit definition
  dy=(y1-y0)                      # compute the change in y
  x=x0-y0/m                       # the approximate solution for x based on Newton's linear approximation 
  dx=(x-x0)                       # difference between approximate solution (x) and initial guess (x0) for x
  k=0                             # initialize the number of iterations performed
  it.seq=cbind(k=k,x=x0,y=y0)     # initialize a matrix with the iteration sequence
  
  # perform the following calculations until the accuracy criteria are met or maximum interations are performed
  while(((abs(y0)>delta)||(abs(dx)>eps))&&(k<max.its)) 
  {
    x0=x                           # use previous solution (x) as new guess (x0)
    y0=f(x0,...);  y1=f(x0+h,...)  # compute y0=f(x0) and y1=f(x+h) in limit definition of derivative
    m=(y1-y0)/h                    # compute slope for linear approximation
    dy=(y1-y0)                     # compute change in f(x) between previous solution and new solution
    x=x0-y0/m                      # compute new approximate solution for x from linear approximation
    dx=(x-x0)                      # difference between previous approximate solution and new approximate solution
    k=k+1                          # increment the iteration number
    it.seq=rbind(it.seq,           # update the iteration sequence
                 c(k=k,x=x0,y=y0))
  }                               # end loop of iterative linear approximations
  y=f(x,...)                      # compute the final y
  res=list(x=x,y=y,it.seq=it.seq) # package the results
  return(res)                     # return the results 
}

################################
# Visualize limits for x approaching a finite value

limit.plots=function(f,                   # a function
                     a,                   # a scalar
                     eps0=1,              # an initial epsilon
                     k=4,                 # number of plots to generate 
                     ylab="f(x)",         # label for y-axis
                     xlab="x",            # label for x-axis
                     ylim=NULL,           # y limits for plots
                     line.clr="darkblue", # color for curve
                     box.clr="gainsboro", # color for zoom-in box
                     ...)                 # additional arguments for the function f
{
  # Check for valid inputs
  if (!is.function(f)) stop("f must be a function")
  if ((length(a)!=1)&(!is.numeric(a))) stop("a must be a numeric with length 1.")
  if ((length(eps0)!=1)&(!is.numeric(eps0))) stop("eps0 must be a numeric of length 1.")
  
  # generate the series of zoom-in plots
  eps=eps0        # initialize eps as user-input eps0
  ylim0=ylim      # capture input ylim
  for (i in 1:k)                               # repeat this operation k times
  {
    x=seq(from=a-eps,to=a+eps,length=10001)      # generate x sequence for plotting
    y=rep(NA,10001)                              # initialize values of y
    for (j in 1:10001)                           # compute y for each value of x
    {
      y.temp=try(f(x[j],...),silent=T)             # try to compute y for each value of x
      if (class(y.temp)!="try-error")              # if successful, then assign jth entry of y
        y[j]=y.temp
    }
    ylim=range(y,na.rm=T)                        # compute range of y
    if (!is.null(ylim0)) ylim=ylim0              # if specified, set user-specified y-limits
    if (is.null(ylim)) ylim=range(y,na.rm=T)     # set y-limits if user didn't specify
    
    na=(is.na(x)|is.na(y))                       # find NAs in x or y
    L=approx(x[!na],y[!na],xout=a)$y             # interpolote to approximate the limit L
    
    plot(range(x,na.rm=T),ylim,                  # generate plot region
         type="n",las=1,                         # no points or lines, horizontal labels on axes
         xlab=xlab,ylab=ylab)                    # x and y axis labels
    
    eps=eps/2                                    # cut epsilon in half for zoom-in box
    x.box=(x>a-eps)&(x<a+eps)                    # find x-values in the zoom-in box
    y.box=range(y[x.box],na.rm=T)                # find y-values for zoom-in box 
    rect(a-eps,y.box[1],a+eps,y.box[2],          # draw the zoom-in box
         col=box.clr,border=NA)                  # with user-specified color and no border
    lines(x,y,lwd=3,col=line.clr)                # draw f(x)                             
    points(a,L,cex=3,col=line.clr)               # draw a circle point around (a,L)
  }                                           # end loop
}                                             # end function
