pw### install packages; you only need to this once (ever)
install.packages("ggplot2", "data.table")

yes
### libraries
library(ggplot2)
library(data.table)

### Hardy-Weinberg function
HWB_fun <- function(p) {
  q <- 1 - p
  
  FAA <- p^2
  FAa <- 2*p*q
  Faa <- q^2
  
  return(data.table(p=p, FAA=FAA, FAa=FAa, Faa=Faa))
}

### generate data
out <- HWB_fun(p=seq(0, 1, by=0.01))

### convert wide to long
out_long <- melt(
  out,
  id.vars="p",
  measure.vars=c("FAA", "FAa", "Faa")
)

### graph
p1 <- ggplot(data=out_long) +
  geom_line(aes(x=p, y=value, group=variable, color=variable)) +
  labs(
    x="p",
    y="value",
    color="variable",
    title="Hardy-Weinberg Equilibrium"
  ) +
  theme_classic()

### graph
p1

### graph
ggsave(p1, file="~/Hardy_Weinberg_plot.pdf")

getwd()
list.files()

