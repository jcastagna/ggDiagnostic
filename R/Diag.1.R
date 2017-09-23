
#' Diagnostic ggplots for models
#' 
#' 
#' Quick Model Diagnostics in GGplot.
#' Standardised Residules v Fitted Values
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
#' Example as function
#' Diag.1(lm.1)
#' Diag.1(glm.1)
#'
#' Example with magrittr pipe
#' lm.1 %>% Diag.1()
#' glm.1 %>% Diag.1()
#' 
#' Output shoud be a ggplot object in form of diagnostic plot.
#' 
#' Coresponding Base example
#' plot(lm.1, which=1)
#' plot(glm.1, which=1)
#' 
#' 
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
    geom_hline(yintercept=0,linetype=4) +
    geom_text(aes(label=ifelse((.std.resid>1*IQR(.std.resid)),.rownames,"")),
            hjust=-0.1,vjust=-0.1,size=2.5) +
    labs(title="Residuls vs Fitted",subtitle=My.Mod$call)
  
return(D.1)

}

