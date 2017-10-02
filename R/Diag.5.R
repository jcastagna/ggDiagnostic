
#' Residuals vs Leverage
#'
#'
#' Quick Model Diagnostics in GGplot.
#' Residuals vs Leverage
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
#' # Residuals vs Leverage
#' # Test models
#'
#' lm.1 <- lm(mpg ~ wt,data=mtcars)
#' glm.1 <- glm(formula= vs ~ wt + disp, data=mtcars, family=binomial)
#'
#' # Example as function()
#' Diag.5(lm.1)
#' Diag.5(glm.1)
#'
#' # Example with magrittr pipe
#' lm.1 %>% Diag.5()
#' glm.1 %>% Diag.5()
#' 
#' # Output shoud be a ggplot object in form of diagnostic plot.
#' 
#' # Coresponding Base example
#' plot(lm.1, which=5)
#' plot(glm.1, which=5)
#' 
#' 
#' @export
#' 

Diag.5 <- function(My.Model){
  
My.Mod=My.Model

print("Attempting to use broom then ggplot on model")

# suppressWarnings()
D.0  <- broom::augment(My.Mod)

D.5 <- D.0 %>% 
  ggplot(aes(x=.hat, y=.std.resid)) +
  geom_point(size=.75) +
  geom_hline(yintercept=0,linetype=3,colour="blue") +
  labs(title="Resid vs Leverage",subtitle=My.Mod$call)
  
return(D.5)

}

