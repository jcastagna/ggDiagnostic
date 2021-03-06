
# Tinker

library(tidyverse)
library(broom)

lm.1 <- lm(mpg ~ wt,data=mtcars)
glm.1 <- glm(vs ~ wt, family = binomial, data = mtcars)

########


#par(mfrow=c(2,3)) 
#plot(lm.1) 
#par(mfrow=c(1,1)) 

#resid vs fitt
tidy(lm.1)
glance(lm.1)
augment(lm.1)

############
My.Mod <- glm.1
My.Mod <- lm.1

Tidy.Mod <- augment(My.Mod)

class(My.Mod)
attributes(My.Mod)

################
# Res v Fitt
plot(My.Mod,which=1)

My.Method="auto"

D1  <- augment(My.Mod) %>% 
  ggplot(aes(x=.fitted,y=.std.resid)) +
  geom_point() +
  geom_smooth(method=My.Method,se=FALSE,colour="red",size=.25) +
  geom_hline(yintercept=0,linetype=4) +
  geom_text(aes(label=ifelse((.std.resid>1*IQR(.std.resid)),.rownames,"")),
             hjust=-0.1,vjust=-0.1,size=2.5) +
  labs(title="Residuls vs Fitted",subtitle=My.Mod$call)
#My.Mod$call
#glue("Model is {My.Mod$call} ")
D1

# geom_text(aes(label=ifelse((x>4*IQR(x)|y>4*IQR(y)),label,"")), hjust=1.1)
D1 +geom_text(aes(label=ifelse((.std.resid>1*IQR(.std.resid)),
                               .rownames,"")),
              hjust=-0.1,vjust=-0.1,size=2.5)

D1 +geom_text(aes(label=ifelse((.std.resid>1*IQR(.std.resid)),
                               .rownames,"")),
              hjust=-0.1,vjust=-0.1,size=2.5)


D1 +geom_text(aes(label=ifelse((.std.resid>1*IQR(.std.resid)|
                                  .fitted>abs(4*IQR(.fitted))),
                               .rownames,"")),
              hjust=-0.1,vjust=-0.1,size=2.5)

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

D2 + geom_abline(intercept = 20, slope = 37.285/5.344,colour="red")
D2 + geom_abline(intercept = -5.344 , slope = 37.285,colour="red")

D2 + geom_smooth(aes(x=sample,y=1))

####
y     <- quantile(My.Mod, c(0.25, 0.75)) # Find the 1st and 3rd quartiles
x     <- qnorm( c(0.25, 0.75))         # Find the matching normal values on the x-axis
slope <- diff(y) / diff(x)             # Compute the line slope
int   <- y[1] - slope * x[1]           # Compute the line intercept

D2 


################
#Scale Location
head(Tidy.Mod)
summary(sqrt(Tidy.Mod$.std.resid))

plot(My.Mod,which=3)

D3  <- augment(My.Mod) %>% 
  ggplot(aes(x=.fitted, y=sqrt(abs(.std.resid)))) +
  geom_point()+
  labs(title="Scale Location",subtitle=My.Mod$call) +
  geom_smooth(se=FALSE,colour="red",size=.25) 


D3

## text?
D3 + geom_text(aes(label=.rownames),
               nudge_x = 1,nudge_y = .1, colour="blue")

D3 + geom_text(aes(label=.rownames), colour="blue")
#y=top_n(5)

library(ggrepel)

D3 + geom_text_repel(aes(label = .rownames))

D3 + geom_text(aes(label=ifelse((.std.resid>1*IQR(.std.resid)),
                           .rownames,"")),
          hjust=-0.1,vjust=-0.1,size=2.5)

#######
#Cooks Distane

plot(My.Mod,which=4)

D4 <- augment(My.Mod) %>% 
  ggplot(aes(x=seq_along(.cooksd), y=.cooksd)) +
  geom_point(size = .75) +
  geom_col(width = .1) +
  labs(title="Cooks Dist",subtitle=My.Mod$call)+
  geom_text(aes(label=ifelse((.cooksd>.2),.rownames,"")),
             hjust=-0.1,vjust=-0.1,size=2.5) +
  geom_hline(yintercept=.2,linetype=4,colour="orange")

D4


D4 +geom_text(aes(label=ifelse((.cooksd>.2),.rownames,"")),
              hjust=-0.1,vjust=-0.1,size=2.5) +
  geom_hline(yintercept=.2,linetype=4,colour="orange")


############

plot(My.Mod,which=5)
head(Tidy.Mod)
tidy(My.Mod)
lev = hat(model.matrix(My.Mod))

D5 <- augment(My.Mod) %>% 
  ggplot(aes(x=.hat, y=.std.resid)) +
  geom_point() +
  geom_hline(yintercept=0,linetype=3) +
  labs(title="Resid vs Leverage",subtitle=My.Mod$call)

D5

######
plot(My.Mod,which=6)

D6 <- augment(My.Mod) %>% 
  ggplot(aes(x=.hat, y=.cooksd)) +
  geom_point() +
  labs(title="CooksD vs Leverage",subtitle=My.Mod$call) 

D6


###
#Panel

#grid extra
library(gridExtra)
grid.arrange(D1, D2, D3, D4, D5, D6, ncol=2)

plot(My.Mod,which=)


################################
#Example test


##################

#ifelse(class(My.Mod)=="lm",
#       print("It's a lm object, using broom then ggplot"),
#       ifelse(class(My.Mod)=="glm",
#       print("its glm"),
#       print("Unsure of modeltype object - so trying my best")))



# broke why token?

#if class(My.Mod)=="lm" {
#  print("It's an lm object, using broom then ggplot")
#} else if class(My.Mod)=="glm" {
#  print("It's an lm object, using broom then ggplot")
#} else {
#  print("Its not an lm/glm object - so trying my best")
#}



#plot(My.Mod,which=1)


D1  = augment(My.Mod) %>% 
  ggplot(aes(x=.fitted,y=.std.resid)) +
  geom_point() +
  geom_smooth(se=FALSE,colour="red",size=.25) +
  geom_hline(yintercept=0,linetype=3) +
  labs(title="Residuls vs Fitted",subtitle=My.Mod$call)


###

D1

#####

# Extra Diagnostics to think about as extension
# Note to re-engineer boot & car into ggd
library(boot)
glm.diag.plots(Mod.D)
glm.diag.plots(Mod.H)
resdev=residuals(Mod.H,type="deviance")
head(resdev)
library(car)
av.plots(Mod.H)


