
# Tinker

library(tidyverse)
library(broom)

mtcars %>%
  split(.$cyl) %>% # from base R
  map(~ lm(mpg ~ wt, data = .)) %>%
  map(summary) %>%
  map_dbl("r.squared")

lm.1=lm(mpg ~ wt,data=mtcars)

lm.1
par(mfrow=c(2,3)) 
plot(lm.1)
par(mfrow=c(1,1)) 
#resid vs fitt
tidy(lm.1)
glance(lm.1)
augment(lm.1)

###
My.Mod=lm.1


plot(My.Mod,which=1)
D1  = augment(My.Mod) %>% ggplot(aes(x=.fitted,y=.std.resid)) +
  geom_point()+
geom_smooth(se=FALSE,colour="red",size=.25)+
  geom_hline(yintercept=0,linetype=3)+
  labs(title="Residuls vs Fitted",subtitle=My.Mod$call)
D1


#################
plot(My.Mod,which=2)
geom_qq()
?geom_qq

D2  = augment(My.Mod) %>% ggplot(aes(x=.se.fit,y=6)) +
                                 geom_point()
D2  = augment(My.Mod) %>% ggplot(aes(sample=.fitted)) +
  stat_qq()
  
D2

################

plot(My.Mod,which=3)
