
# Tinker

library(tidyverse)
library(broom)

lm.1 <- lm(mpg ~ wt,data=mtcars)

lm.1
par(mfrow=c(2,3)) 
plot(lm.1) 
par(mfrow=c(1,1)) 
#resid vs fitt
tidy(lm.1)
glance(lm.1)
augment(lm.1)

############
My.Mod <- lm.1
Tidy.Mod <- augment(My.Mod)

class(My.Mod)
attributes(My.Mod)

################
# Res v Fitt
plot(My.Mod,which=1)

D1  <- augment(My.Mod) %>% 
  ggplot(aes(x=.fitted,y=.std.resid)) +
  geom_point() +
  geom_smooth(se=FALSE,colour="red",size=.25) +
  geom_hline(yintercept=0,linetype=3) +
  labs(title="Residuls vs Fitted",subtitle=My.Mod$call)

D1


#################
# Normal QQ

plot(My.Mod,which=2)
geom_qq()
?geom_qq

D2  <- augment(My.Mod) %>% 
  ggplot(aes(sample=.fitted)) +
  stat_qq() +
  labs(title="Normal QQ",subtitle=My.Mod$call)

D2 

D2 + geom_abline(intercept = 10, slope = 2,colour="red")
D2 + geom_abline(intercept = -5.344 , slope = 37.285,colour="red")

D2 + geom_smooth( se = FALSE)

################
#Scale Location
head(Tidy.Mod)
summary(sqrt(Tidy.Mod$.std.resid))

plot(My.Mod,which=3)

D3  <- augment(My.Mod) %>% 
  ggplot(aes(x=.fitted, y=sqrt(.std.resid))) +
  geom_point()+
  labs(title="Scale Location",subtitle=My.Mod$call)


D3

## text?
D3 + geom_text(aes(label=.rownames),
               nudge_x = 1,nudge_y = .1, colour="blue")

D3 + geom_text(aes(label=.rownames), colour="blue")
#y=top_n(5)

library(ggrepel)

D3 + geom_text_repel(aes(label = .rownames))

#######
#Cooks Distane
plot(My.Mod,which=4)

D4 <- augment(My.Mod) %>% 
  ggplot(aes(x=1:32, y=.cooksd)) +
  geom_point() +
  geom_col(width = .1) +
  labs(title="Cooks Dist",subtitle=My.Mod$call)

D4
