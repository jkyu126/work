# Former calculation file: E:\99_Jinkyu\MktImpact\03_analysis.sas => "main" file
# Specification: 
# (1) group Q/V by 0.1% (1,000 bins)
# (2) 

library(ggplot2)
library(dplyr)
library(gridExtra)
library(ggpubr)
mydat = read.csv("q_v.csv")

# Estimate regression model

myres = summary(lm(log_shortfall_std ~ log_X_V_round, data=mydat))$coefficients %>%
  as.data.frame()
alpha = myres[1, "Estimate"]
beta = myres[2, "Estimate"]

plt1 = mydat %>% 
  ggplot(aes(log_X_V_round, log_shortfall_std)) +
  geom_point() +
  geom_smooth(method=lm, color="red", se=T, fill="#69b3a2") +
  labs(title = expression(log(Delta/sigma)== 0.963 + '0.420' ~'*'~ log(Q/V))) + 
  xlab(expression(log(Q/V))) +
  ylab(expression(log(Delta/sigma))) + 
  theme_bw() + 
  theme(plot.title = element_text(size=11, face="bold", hjust=0.5))

plt2 = mydat %>%
  ggplot(aes(X_V_round, shortfall_std)) +
  geom_point() +
  geom_smooth(method=loess, color="red", se=T, fill="#69b3a2") +
  labs(title = expression(Delta/sigma ~"~"~ Q/V)) + 
  xlab(expression(Q/V)) +
  ylab(expression(Delta/sigma)) + 
  theme_bw() + 
  theme(plot.title = element_text(size=11, face="bold", hjust=0.5))
  
g = ggarrange(plt1, plt2, ncol=2, nrow=1)
ggsave(file="figure.png", g, dpi=300, width=270, height=110, units="mm")


