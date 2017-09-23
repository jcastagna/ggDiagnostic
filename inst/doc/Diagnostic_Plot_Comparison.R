## ----setup, include=TRUE,echo=FALSE--------------------------------------
knitr::opts_chunk$set(echo = TRUE, message=FALSE,warning=FALSE,warnings=FALSE,messages=FALSE)

## ----Libs----------------------------------------------------------------
library(tidyverse)
library(broom)
library(gridExtra)

# Traditional Model
lm.1 <- lm(mpg ~ wt,data=mtcars)

# Generic name
My.Mod <- lm.1
# Tidy output
Tidy.Mod <- augment(My.Mod)

# Attributes
class(My.Mod)
attributes(My.Mod)
head(Tidy.Mod,4)

## ----D1, echo=TRUE, collapse=TRUE,fig.align='center',eval=FALSE----------
#  
#  # base diagnostic
#  plot(My.Mod,which=1)
#  
#  # ggDiagnostic plot
#  D1  <- augment(My.Mod) %>%
#    ggplot(aes(x=.fitted,y=.std.resid)) +
#    geom_point() +
#    geom_smooth(se=FALSE,colour="red",size=.25) +
#    geom_hline(yintercept=0,linetype=3,colour="blue") +
#    geom_text(aes(label=ifelse((.std.resid>1*IQR(.std.resid)),.rownames,"")),
#               hjust=-0.1,vjust=-0.1,size=2.5) +
#    labs(title="Residuls vs Fitted",subtitle=My.Mod$call)
#  
#  D1

## ----ref.label=c("D1"),eval=TRUE,echo=FALSE, fig.align='center', fig.height=3,fig.width=3,fig.show='hold'----

# base diagnostic
plot(My.Mod,which=1)

# ggDiagnostic plot
D1  <- augment(My.Mod) %>% 
  ggplot(aes(x=.fitted,y=.std.resid)) +
  geom_point() +
  geom_smooth(se=FALSE,colour="red",size=.25) +
  geom_hline(yintercept=0,linetype=3,colour="blue") +
  geom_text(aes(label=ifelse((.std.resid>1*IQR(.std.resid)),.rownames,"")),
             hjust=-0.1,vjust=-0.1,size=2.5) +
  labs(title="Residuls vs Fitted",subtitle=My.Mod$call)

D1

## ----D2, echo=TRUE, collapse=TRUE,fig.align='default',eval=FALSE---------
#  
#  # base diagnostic
#  plot(My.Mod,which=2)
#  
#  # ggDiagnostic plot
#  D2  <- augment(My.Mod) %>%
#    ggplot(aes(sample=.fitted)) +
#    stat_qq() +
#    labs(title="Normal QQ",subtitle=My.Mod$call)
#  
#  
#  D2
#  

## ----ref.label=c("D2"),eval=TRUE,echo=FALSE, fig.align='default', fig.height=3.5,fig.width=3.5----

# base diagnostic
plot(My.Mod,which=2)

# ggDiagnostic plot
D2  <- augment(My.Mod) %>% 
  ggplot(aes(sample=.fitted)) +
  stat_qq() +
  labs(title="Normal QQ",subtitle=My.Mod$call)
  

D2 


## ----D3, echo=TRUE, collapse=TRUE,fig.align='default',eval=FALSE---------
#  # base diagnostic
#  plot(My.Mod,which=3)
#  
#  # ggDiagnostic plot
#  D3  <- augment(My.Mod) %>%
#    ggplot(aes(x=.fitted, y=sqrt(abs(.std.resid)))) +
#    geom_point()+
#    labs(title="Scale Location",subtitle=My.Mod$call) +
#    geom_smooth(se=FALSE,colour="red",size=.25)
#  
#  D3
#  
#  

## ----ref.label=c("D3"),eval=TRUE,echo=FALSE, fig.align='default', fig.height=3.5,fig.width=3.5----
# base diagnostic
plot(My.Mod,which=3)

# ggDiagnostic plot
D3  <- augment(My.Mod) %>% 
  ggplot(aes(x=.fitted, y=sqrt(abs(.std.resid)))) +
  geom_point()+
  labs(title="Scale Location",subtitle=My.Mod$call) + 
  geom_smooth(se=FALSE,colour="red",size=.25) 

D3



## ----D4, echo=TRUE, collapse=TRUE,fig.align='default',eval=FALSE---------
#  # Cooks Distance
#  # base diagnostic
#  plot(My.Mod,which=4)
#  
#  # ggDiagnostic plot
#  D4 <- augment(My.Mod) %>%
#    ggplot(aes(x=seq_along(.cooksd), y=.cooksd)) +
#    geom_point(size = .75) +
#    geom_col(width = .1) +
#    geom_hline(yintercept=.2,linetype=4,colour="orange") +
#    labs(title="Cooks Dist",subtitle=My.Mod$call) +
#    geom_text(aes(label=ifelse((.cooksd>.2),.rownames,"")),
#                hjust=-0.1,vjust=-0.1,size=2.5)
#  
#  D4

## ----ref.label=c("D4"),eval=TRUE,echo=FALSE, fig.align='default', fig.height=3.5,fig.width=3.5----
# Cooks Distance
# base diagnostic
plot(My.Mod,which=4)

# ggDiagnostic plot
D4 <- augment(My.Mod) %>% 
  ggplot(aes(x=seq_along(.cooksd), y=.cooksd)) +
  geom_point(size = .75) +
  geom_col(width = .1) +
  geom_hline(yintercept=.2,linetype=4,colour="orange") +
  labs(title="Cooks Dist",subtitle=My.Mod$call) +
  geom_text(aes(label=ifelse((.cooksd>.2),.rownames,"")),
              hjust=-0.1,vjust=-0.1,size=2.5)

D4

## ----D5, echo=TRUE, collapse=TRUE,fig.align='default',eval=FALSE---------
#  
#  # Resid vs Leverage
#  
#  # base diagnostic
#  plot(My.Mod,which=5)
#  
#  # ggDiagnostic plot
#  D5 <- augment(My.Mod) %>%
#    ggplot(aes(x=.hat, y=.std.resid)) +
#    geom_point(size=.75) +
#    geom_hline(yintercept=0,linetype=3,colour="blue") +
#    labs(title="Resid vs Leverage",subtitle=My.Mod$call)
#  
#  D5
#  

## ----ref.label=c("D5"),eval=TRUE,echo=FALSE, fig.align='default', fig.height=3.5,fig.width=3.5----

# Resid vs Leverage

# base diagnostic
plot(My.Mod,which=5)

# ggDiagnostic plot
D5 <- augment(My.Mod) %>% 
  ggplot(aes(x=.hat, y=.std.resid)) +
  geom_point(size=.75) +
  geom_hline(yintercept=0,linetype=3,colour="blue") +
  labs(title="Resid vs Leverage",subtitle=My.Mod$call)

D5


## ----D6, echo=TRUE, collapse=TRUE,fig.align='default',eval=FALSE---------
#  # base diagnostic
#  plot(My.Mod,which=6)
#  
#  # ggDiagnostic plot
#  D6 <- augment(My.Mod) %>%
#    ggplot(aes(x=.hat, y=.cooksd)) +
#    geom_point() +
#    labs(title="CooksD vs Leverage",subtitle=My.Mod$call)
#  
#  D6

## ----ref.label=c("D6"),eval=TRUE,echo=FALSE, fig.align='default', fig.height=3.5,fig.width=3.5----
# base diagnostic
plot(My.Mod,which=6)

# ggDiagnostic plot
D6 <- augment(My.Mod) %>% 
  ggplot(aes(x=.hat, y=.cooksd)) +
  geom_point() +
  labs(title="CooksD vs Leverage",subtitle=My.Mod$call) 

D6

## ----D7, echo=TRUE, collapse=TRUE,fig.align='default',eval=FALSE---------
#  
#  #library(gridExtra)
#  
#  D.Panel <- grid.arrange(D1, D2, D3, D4, D5, D6, ncol=2)
#  D.Panel

## ----ref.label=c("D7"),eval=TRUE,echo=FALSE, fig.align='default'---------

#library(gridExtra)

D.Panel <- grid.arrange(D1, D2, D3, D4, D5, D6, ncol=2)
D.Panel

## ------------------------------------------------------------------------
par(mfrow=c(2,3))
plot(My.Mod,which=1:6)

