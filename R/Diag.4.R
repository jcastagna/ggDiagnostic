
#' Cooks Distance - Diagnostic ggplot for models
#'
#'
#' Quick Model Diagnostics in GGplot.
#' Cooks Distance
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
#' # Cooks Distance
#' # Test models
#'
#' lm.1 <- lm(mpg ~ wt,data=mtcars)
#' glm.1 <- glm(formula= vs ~ wt + disp, data=mtcars, family=binomial)
#'
#' # Example as function()
#' Diag.4(lm.1)
#' Diag.4(glm.1)
#'
#' # Example with magrittr pipe
#' lm.1 %>% Diag.4()
#' glm.1 %>% Diag.4()
#' 
#' # Output shoud be a ggplot object in form of diagnostic plot.
#' 
#' # Coresponding Base example
#' plot(lm.1, which=4)
#' plot(glm.1, which=4)
#' 
#' 
#' @export
#' 

Diag.4 <- function(My.Model){
  
My.Mod=My.Model

print("Attempting to use broom then ggplot on model")

# suppressWarnings()
D.0  <- broom::augment(My.Mod)

D.4 <- D.0 %>% 
  ggplot(aes(x=seq_along(.cooksd), y=.cooksd)) +
  geom_point(size = .75) +
  geom_col(width = .1) +
  geom_hline(yintercept=.2,linetype=4,colour="orange") +
  labs(title="Cooks Dist",subtitle=My.Mod$call) +
  geom_text(aes(label=ifelse((.cooksd>.2),.rownames,"")),
            hjust=-0.1,vjust=-0.1,size=2.5)

  
return(D.4)

}

