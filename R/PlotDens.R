#' PlotDens Function
#'
#' This function shows the plot of the normal and/or the student-t Distribution and calculates the probability P(a < X < b).
#'
#' The user can choose the `mean`, the standard deviation (`sd`), the degrees of freedom, a range [`a`, `b`] and which plots to show (`show`)
#'
#' @param a `numeric` parameter, lower bound of the range
#' @param b `numeric` parameter, upper bound of the range
#' @param mean `numeric` parameter, mean for the normal distribution
#' @param sd `numeric` parameter, standard deviation for the normal distribution
#' @param df `numeric` parameter, degrees of freedom for the Student-t distribution
#' @param show `character` parameter, which lines to show: "Overlay", "Normal" or "Student"
#'
#' @return Plot with the characteristics chosen
#'
#' @export
#' @import ggplot2
#' @import stats
PlotDens <- function(a = -1, b = 1, mean=0,sd=1,df=1,show="Overlay") {
  p <- pnorm(b,mean=mean,sd=sd) - pnorm(a,mean=mean,sd=sd)
  t <- pt(b,df)-pt(a,df)
  if(show=="Overlay"){
    if(dnorm(a,mean,sd)>=dt(a,df)){
      yea = dnorm(a,mean,sd)
    } else{
      yea=dt(a,df)
    }
    if(dnorm(b,mean,sd)>=dt(b,df)){
      yeb = dnorm(b,mean,sd)
    } else{
      yeb=dt(b,df)
    }
    ggplot() +
      xlim(-5, 5) +
      stat_function(fun = dnorm, args = list(mean = mean, sd = sd), aes(color = paste("Normal (mean =", mean," sd =",sd ,")\n P(a < X < b) = ",round(p,digits=3)))) +
      stat_function(fun = dnorm, args = list(mean = mean, sd = sd), geom="area",fill="red",alpha=0.2,xlim = c(a, b)) +
      labs(x = NULL, y = "density", colour = NULL) +
      theme_minimal() +
      theme(legend.position ="top") +
      geom_segment(aes(x = a, y = 0, xend = a, yend = yea),linetype = "dashed") +
      geom_segment(aes(x = b, y = 0, xend = b, yend = yeb),linetype = "dashed") +
      annotate(geom = "text",x = a,y = -0.01,label = "a") +
      annotate(geom = "text",x = b,y = -0.01,label = "b") +
      stat_function(fun = dt, args = list(df=df), aes(color = paste("Student's t-distribution (df =", df,")\n P(a < X < b) = ",round(t,digits=3))))+
      stat_function(fun = dt, args = list(df=df), geom="area",fill="blue",alpha=0.1,xlim = c(a, b))
  } else if(show=="Normal"){
    ggplot2::ggplot() +
      xlim(-5, 5) +
      stat_function(fun = dnorm, args = list(mean = mean, sd = sd), aes(color = paste("Normal (mean =", mean," sd =",sd ,")\n P(a < X < b) = ",round(p,digits=3)))) +
      stat_function(fun = dnorm, args = list(mean = mean, sd = sd), geom="area",fill="red",alpha=0.2,xlim = c(a, b)) +
      labs(x = NULL, y = "density", colour = NULL) +
      theme_minimal() +
      theme(legend.position ="top") +
      geom_segment(aes(x = a, y = 0, xend = a, yend = dnorm(a,mean=mean,sd=sd)),linetype = "dashed") +
      geom_segment(aes(x = b, y = 0, xend = b, yend = dnorm(b,mean=mean,sd=sd)),linetype = "dashed") +
      annotate(geom = "text",x = a,y = -0.01,label = "a") +
      annotate(geom = "text",x = b,y = -0.01,label = "b")
  } else if(show=="Student"){
    ggplot2::ggplot() +
      xlim(-5, 5) +
      labs(x = NULL, y = "density", colour = NULL) +
      theme_minimal() +
      theme(legend.position ="top") +
      geom_segment(aes(x = a, y = 0, xend = a, yend = dt(a,df=df)),linetype = "dashed") +
      geom_segment(aes(x = b, y = 0, xend = b, yend = dt(b,df=df)),linetype = "dashed") +
      annotate(geom = "text",x = a,y = -0.01,label = "a") +
      annotate(geom = "text",x = b,y = -0.01,label = "b") +
      stat_function(fun = dt, args = list(df=df), aes(color = paste("Student's t-distribution (df =", df,")\n P(a < X < b) = ",round(t,digits=3))))+
      stat_function(fun = dt, args = list(df=df), geom="area",fill="blue",alpha=0.1,xlim = c(a, b))
  }

}

