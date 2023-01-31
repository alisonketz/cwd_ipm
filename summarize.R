summarize <- function(x){
  y=rbind(x[[1]],x[[2]],x[[3]])
  z=data.frame(cbind(apply(y,2,mean,na.rm=TRUE),apply(y,2,sd,na.rm=TRUE),t(apply(y,2,quantile,c(.5,.025,.975),na.rm=TRUE))))
  names(z)=c("mean","sd","median","lower","upper")
  return(z)
}