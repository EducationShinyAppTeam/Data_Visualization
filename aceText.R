initialAceText <- 'This is some markdown text. It may also have embedded R code which will be executed. Please also read the output message for more hints.

You can add a new code chuck with following two lines
```{r}
```
```{r}
#structure on datasets we used in previous cases
str(cars)
str(trees)
str(iris)
```
It can even include graphical elements.
```{r}
#ggplot with one variable
#ggplot(aes(x=dist), data=cars)+geom_histogram()
```
```{r}
#ggplot with two variable
#ggplot(aes(x=Sepal.Length, y=Petal.Length), data=iris)+
#geom_line()
```
```{r}
#Rplot with one variable
plot(cars$speed)
```
'