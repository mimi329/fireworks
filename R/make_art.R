#' Creates fireworks animation
#' @export
#' @param your_number numeric variable of size 1

require(dplyr)
require(ggplot2)
require(ggthemes)
require(charlatan)
require(plotly)
require(gganimate)

make_art <- function(your_number){
  c<-your_number
  set.seed(c)
  C1 <- sample(ncol(ChickWeight):nrow(ChickWeight), 300)
  dfC1 <- ChickWeight[C1,]
  set.seed(c*34)
  C2 <- sample(ncol(ChickWeight):nrow(ChickWeight), 200)
  dfC2<- ChickWeight[C2,]
  set.seed(c*456)
  C3 <- sample(ncol(ChickWeight):nrow(ChickWeight), 100)
  dfC3 <- ChickWeight[C3,]
  set.seed(c*12345)
  C4 <- sample(ncol(ChickWeight):nrow(ChickWeight), 20)
  dfC4 <- ChickWeight[C4,]

  shape_num <- numeric(length=1)
  shape_num <-
    if(your_number == 0) {shape_num <- 1
    } else if (your_number > 0 & your_number <= 5) {shape_num <- 17
    } else if (your_number > 5 & your_number <= 10) {shape_num <- 21
    } else if (your_number > 10 & your_number <= 50) {shape_num <- 10
    } else if (your_number > 50 & your_number <= 100) {shape_num <- 15
    } else if (your_number > 100 & your_number <= 120) {shape_num <- 16
    } else if (your_number > 120 & your_number <= 160) {shape_num <- 18
    } else if (your_number > 160 & your_number <= 180) {shape_num <- 19
    } else if (your_number > 180 & your_number <= 270) {shape_num <- 11
    } else if (your_number > 270 & your_number <= 400) {shape_num <- 22
    } else if (your_number > 400 & your_number <= 500) {shape_num <- 23
    } else if (your_number > 500 & your_number <= 600) {shape_num <- 24
    } else if (your_number > 600 & your_number <= 700) {shape_num <- 25
    } else if (your_number > 700 & your_number <= 800) {shape_num <- 8
    } else if (your_number > 800 & your_number <= 900) {shape_num <- 9
    } else if (your_number > 900 & your_number <= 1000) {shape_num <- 12
    } else if (your_number > 1000 & your_number <= 2000) {shape_num <- 14
    } else {shape_num <- 13
    }

  graph1<-ggplot(mapping = aes(x=Chick, y=weight))+
    geom_point(data = dfC1, show.legend = FALSE, alpha = 1/7, colour = "orange",
               size=2, shape=shape_num) +
    geom_point(data = dfC2, show.legend = FALSE, alpha = 1/3, colour = "red",
               size=1, shape=shape_num) +
    geom_point(data = dfC3, show.legend = FALSE, alpha = 1/5, colour = "yellow",
               size=3, shape=shape_num) +
    geom_point(data = dfC4, show.legend = FALSE, alpha = 1/2, colour = "gold",
               size=4, shape=shape_num) +
    theme(legend.position = "none",
          panel.grid = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.background = element_blank())
  graph1
  graph1.animation <- graph1 +
    transition_time(Time) +
    shadow_wake(wake_length = 0.8)

  animate (graph1.animation, height = 500, width = 800, fps = 30, end_pause = 0, res = 100)
}
