shortest.half=function(x)
{
  x=x[!is.na(x)]                  # remove missing values
  x=sort(x)                       # sort x
  n=length(x)                     # get number of observations
  h=ceiling(n/2)                  # get number of observations in half-width
  nhw=n-h+1                       # number of half-widths
  hw=cbind(a=x[1:nhw],            # left-endpoints of half-widths
           b=x[h+(0:(nhw-1))])    # right-endpoints of half-widths
  dw=hw[,2]-hw[,1]                # size of half-widths
  minhw.index=which.min(dw)       # index of shortest half
  
  # package and return results
  res=list(sorted.x=x,                              # sorted data
           shortest.half=hw[minhw.index,],          # endpoints of shortest half
           sh.mean=mean(x[minhw.index+(0:(h-1))]))  # mean of data in shortest half
  
  class(res)="shortest.half"      # set a class to define print and plot functions
  return(res)                     # return the result
}

# print only the summary stats, not the entire sorted x
print.shortest.half=function(sh.res)
{
  print(list(shortest.half=sh.res$shortest.half,
             sh.mean=sh.res$sh.mean))
}