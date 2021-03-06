% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Diag.5.R
\name{Diag.5}
\alias{Diag.5}
\title{Residuals vs Leverage}
\usage{
Diag.5(My.Model)
}
\arguments{
\item{My.Model}{A model in a list format. eg lm or glm}
}
\value{
A ggplot object; a diagnostic plot (modeled on the base version)
}
\description{
Quick Model Diagnostics in GGplot.
Residuals vs Leverage
Using the Base version diagnostic plots as a template.
}
\examples{

# Residuals vs Leverage
# Test models

lm.1 <- lm(mpg ~ wt,data=mtcars)
glm.1 <- glm(formula= vs ~ wt + disp, data=mtcars, family=binomial)

# Example as function()
Diag.5(lm.1)
Diag.5(glm.1)

# Example with magrittr pipe
lm.1 \%>\% Diag.5()
glm.1 \%>\% Diag.5()

# Output shoud be a ggplot object in form of diagnostic plot.

# Coresponding Base example
plot(lm.1, which=5)
plot(glm.1, which=5)


}
\author{
Justin Castagna, \email{justin.castagna@students.mq.edu.au}
}
\keyword{diagnostic}
\keyword{ggplot2}
\keyword{model}
