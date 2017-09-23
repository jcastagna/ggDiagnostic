
#' Scale Location - Diagnostic ggplot for models
#'
#'
#' Quick Model Diagnostics in GGplot.
#' Scale Location
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
#' # Scale Location
#' # Test models
#'
#' lm.1 <- lm(mpg ~ wt,data=mtcars)
#' glm.1 <- glm(formula= vs ~ wt + disp, data=mtcars, family=binomial)
#'
#' # Example as function()
#' Diag.3(lm.1)
#' Diag.3(glm.1)
#'
#' # Example with magrittr pipe
#' lm.1 %>% Diag.3()
#' glm.1 %>% Diag.3()
#' 
#' # Output shoud be a ggplot object in form of diagnostic plot.
#' 
#' # Coresponding Base example
#' plot(lm.1, which=3)
#' plot(glm.1, which=3)
#' 
#' 
#' @export
#' 

Diag.3 <- function(My.Model){
  
My.Mod=My.Model

print("Attempting to use broom then ggplot on model")

# suppressWarnings()
D.0  <- broom::augment(My.Mod)

D.3 <- D.0 %>% 
  ggplot(aes(x=.fitted, y=sqrt(abs(.std.resid)))) +
  geom_point() +
  geom_smooth(se=FALSE,colour="red",size=.25) +
  labs(title="Scale Location",subtitle=My.Mod$call)
  
return(D.3)

}

