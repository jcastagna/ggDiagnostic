
lm.1=lm(mpg ~ wt,data=mtcars)

My.Mod=lm.1

plot(My.Mod,which=1)

D1  = augment(My.Mod) %>% 
  ggplot(aes(x=.fitted,y=.std.resid)) +
  geom_point() +
  geom_smooth(se=FALSE,colour="red",size=.25) +
  geom_hline(yintercept=0,linetype=3) +
  labs(title="Residuls vs Fitted",subtitle=My.Mod$call)


###

D1


