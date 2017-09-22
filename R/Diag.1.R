
#' Diagnostic Test Functions
#'
#' Question: How to ensure libs are loaded; library(tidyverse), library(broom)
#'
#' Quick Model Diagnostics in GGplot.
#' Using the Base version diagnostic plots as a template.
#' 
#' @param set A model list eg lm or glm
#' 
#' @return A ggplot diagnostic plot (modeled on the base version)
#' 
#' @author Justin, \email{justin.castagna@@students.mq.edu.au}
#' @keywords ggplot2 model diagnostic
#' 
#' @examples
#' 
#' Test models
#' lm.1=lm(mpg ~ wt,data=mtcars)
#' glm.1 <- glm(formula= vs ~ wt + disp, data=mtcars, family=binomial)

# Example as function
#' Diag.1(lm.1)
#' Diag.1(glm.1)

# Example with magrittr pipe
#' lm.1 %>% Diag.1
#' glm.1 %>% Diag.1
#' 
#' Output shoud be a ggplot object
#' 
#' 

#' 
#' @export
#' 

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

