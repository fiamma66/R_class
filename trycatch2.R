tryCatch(
  {
    x="1"
    z=sqrt(x)
  },
  
  
  warning = function(msg){
    message(paste("[Warning]",msg,"\n"))
    return(NULL)
  },

  error = function(msg){
    message(paste("[Critical Error"),msg,"\n")
    return(NA)
  }
  
)
ifelse(exists("z"),z,"z does not exist!")
a=x
a





