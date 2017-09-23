
#' Diagnostic ggplots for models
#'
#'
#' Quick Model Diagnostics in GGplot.
#' Using the Base version diagnostic plots as a template.
#' 
#' @param My.Model A model in a list format. eg lm or glm
#' 
#' @return A ggplot object; a diagnostic plot (modeled on the base version)
#' 
#' @author Justin Castagna, \email{justin.castagna@@students.mq.edu.au}
#' @keywords ggplot2 model diagnostic
#' 
#' @examples
#' 
#' Test models
#'
#' lm.1 <- lm(mpg ~ wt,data=mtcars)
#' glm.1 <- glm(formula= vs ~ wt + disp, data=mtcars, family=binomial)
#'
#' Example as function()
#' Diag.2(lm.1)
#' Diag.2(glm.1)
#'
#' Example with magrittr pipe
#' lm.1 %>% Diag.2()
#' glm.1 %>% Diag.2()
#' 
#' Output shoud be a ggplot object in form of diagnostic plot.
#' 
#' 

#' 
#' @export
#' 

Diag.2 <- function(My.Model){
  
My.Mod=My.Model

print("Attempting to use broom then ggplot on model")

# suppressWarnings()
D.0  <- broom::augment(My.Mod)

D.2 <- D.0 %>% 
  ggplot(aes(sample=.fitted)) +
  stat_qq() +
  labs(title="Normal QQ",subtitle=My.Mod$call)
  
return(D.2)

}

