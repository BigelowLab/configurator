# configurator

Simple configuration file (INI) handling in R

#### Installation
It's easy to install using Hadley Wickham's [devtools](http://cran.r-project.org/web/packages/devtools/index.html).

```r
library(devtools)
install_github('BigelowLab/configurator')
```

#### Usage

```R
# read the example
Cfg <- Configurator(system.file("extdata", "example.cfg", package = "configurator"))
# show it
Cfg
[Foo]
Friday=yeah!
time=lunch
[Bar]
dog=Spike    
fleas=31000
state=sleeping
# get an item
n_fleas <- as.numeric(Cfg$get("Bar", "fleas", default = 0))
n_fleas
```
