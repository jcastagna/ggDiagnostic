
# Diagnostic Test Funct
#?how to ensure libs are loaded
library(tidyverse)
library(ggplot2)
library(broom)
library(magrittr)

Diag.1 <- function(My.Model){
  
My.Mod=My.Model

print("Attempting to use broom then ggplot on model")

# suppressWarnings()
D.0  <- broom::augment(My.Mod)

D.1 <- D.0 %>% 
    ggplot(aes(x=.fitted,y=.std.resid)) +
    geom_point() +
    geom_smooth(se=FALSE,colour="red",size=.25) +
    geom_hline(yintercept=0,linetype=3) +
    labs(title="Residuls vs Fitted",subtitle=My.Mod$call)
  
return(D.1)

}

################################
#Example test

# Test models
lm.1=lm(mpg ~ wt,data=mtcars)
glm.1 <- glm(formula= vs ~ wt + disp, data=mtcars, family=binomial)

# Example as function
Diag.1(lm.1)
Diag.1(glm.1)

# Example with pipe
lm.1 %>% Diag.1
glm.1 %>% Diag.1

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


