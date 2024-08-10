
library(data.table)
library(ggplot2)

N=10000
a <- 0

betas <- seq(0,1,.1)
ssize <- c(50,100,250,1000)

power_mat <- matrix(nrow=length(betas),ncol=length(ssize))
colnames(power_mat) <- paste0("n",ssize)

for(n in ssize){
  
  set.seed(n)
  x <- runif(n)
  
  size_power <- list()
  for(b in betas){
    pval <- replicate(N,expr={
      e <- rnorm(n)
      y <- a+b*x+e
      reg <- lm(y~x)
      pval <- summary(reg)$coef["x",4]
      pval})
    
    size_power[[paste(b)]] <- length(which(pval<.05))/N
  }
  
  power_mat[,paste0("n",n)] <- unlist(size_power)
  
}



power_dt <- data.table(b=as.factor(betas),power_mat)
power_dt$b <- factor(power_dt$b,levels=betas)
power_lg <- melt(power_dt,id.vars = "b")

ggplot(power_lg,aes(x=b,y=value,color=variable,linetype=variable,group=variable))+
  geom_line()+
  geom_point()+
  theme_classic()+
  labs(x="parameter",y="power")+
  theme(legend.title = element_blank())


