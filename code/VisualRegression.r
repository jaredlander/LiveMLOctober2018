# this is a comment

library(ggplot2)
data(father.son, package='UsingR')
head(father.son)

ggplot(father.son, aes(x=fheight, y=sheight)) + geom_point() +
    geom_smooth(method='lm')

heightMod <- lm(sheight ~ fheight, data=father.son)
heightMod

33.88 + 0.51*70
33.88 + 0.51*60
33.88 + 0.51*80
