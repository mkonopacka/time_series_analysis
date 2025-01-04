
``` r
library(forecast)
library(ggplot2)
library(tidyr)
library(gridExtra)
```

# Types of noise

## 1.1.1. White noise

Variable
![w\_t](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;w_t
"w_t") is a white noise if it meets only three conditions: - ![E(w\_t)
= 0](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;E%28w_t%29%20%3D%200
"E(w_t) = 0"), - ![Var(w\_t) =
\\sigma^2](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;Var%28w_t%29%20%3D%20%5Csigma%5E2
"Var(w_t) = \\sigma^2"), - ![cov(w\_t, w\_s)
= 0](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;cov%28w_t%2C%20w_s%29%20%3D%200
"cov(w_t, w_s) = 0").

Note it doesn’t have to be normally distributed (then we call it
“gaussian noise”) - but in many cases it is.

``` r
set.seed(0)
white_noise = rnorm(200)
tsdisplay(white_noise, plot.type = "histogram", points = FALSE)
```

![](types_of_noise_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
mean(white_noise)
```

    ## [1] -0.01144155

``` r
var(white_noise)
```

    ## [1] 0.8524895

``` r
set.seed(0) # for seed = 7 there is an accidental autocorrelation
white_noise2 = runif(200, min = -1, max = 1)
tsdisplay(white_noise2, plot.type = "histogram", points = FALSE)
```

![](types_of_noise_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
mean(white_noise2)
```

    ## [1] 0.03645731

``` r
var(white_noise2) # 1/3 is a theoretical value
```

    ## [1] 0.2909825

## Autoreggresive Processes AR(p)

The Autoregressive process is given by the equation

  
![X\_t = \\sum^p\_{i=1} \\theta\_i X\_{t-i} +
w\_t.](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;X_t%20%3D%20%5Csum%5Ep_%7Bi%3D1%7D%20%5Ctheta_i%20X_%7Bt-i%7D%20%2B%20w_t.
"X_t = \\sum^p_{i=1} \\theta_i X_{t-i} + w_t.")  

### AR(1)

AR(1) process   
![X\_t = \\theta X\_{t-1} +
w\_t](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;X_t%20%3D%20%5Ctheta%20X_%7Bt-1%7D%20%2B%20w_t
"X_t = \\theta X_{t-1} + w_t")  

is stationary if ![|\\theta|
\< 1](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%7C%5Ctheta%7C%20%3C%201
"|\\theta| \< 1"). In R we can simulate such process using `arima.sim`
function. Greater absolute value results in greater “amplitude” and
negative theta causes oscilations from positive to negative.

``` r
N <- 200
ar1_processes <- matrix(nrow = 7, ncol = N)
ar1_processes[1,] <- 1:N
ar1_processes[2,] <- arima.sim(model = list(ar = c(0.1)), n = N)
ar1_processes[3,] <- arima.sim(model = list(ar = c(0.5)), n = N)
ar1_processes[4,] <- arima.sim(model = list(ar = c(0.9)), n = N)
ar1_processes[5,] <- arima.sim(model = list(ar = c(-0.1)), n = N)
ar1_processes[6,] <- arima.sim(model = list(ar = c(-0.5)), n = N)
ar1_processes[7,] <- arima.sim(model = list(ar = c(-0.9)), n = N)

# Note: you can use arima.sim only to simulate a stationary process. To simulate non-stationary one,
# use Arima.

ar1_processes <- as.data.frame(t(ar1_processes)) 
colnames(ar1_processes) <- c(
  "time", "theta_0.1", "theta_0.5", "theta_0.9", "theta_-0.1", "theta_-0.5", "theta_-0.9")

ar1_processes %>% pivot_longer(cols = 2:7) %>% 
  ggplot(aes(x = time, y = value, col = name)) +
  geom_line() + 
  ggtitle("AR(1) processes with different coefficient (only stationary, i.e. |theta| < 1 allowed by arima.sim)") +
  theme(legend.position = "bottom")
```

![](types_of_noise_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
ar1_acfs <- list()
for (name in colnames(ar1_processes)[-1]){
  ar1_acfs[[name]] <- ar1_processes[name] %>% ggAcf() +
    ggtitle(paste("AR(1) with", name))
}

gridExtra::grid.arrange(
  grobs = ar1_acfs, nrow = 2, top = "ACF plots for AR(1) process with different theta")
```

![](types_of_noise_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
ar1_pacfs <- list()
for (name in colnames(ar1_processes)[-1]){
  ar1_pacfs[[name]] <- ar1_processes[name] %>% ggPacf() +
    ggtitle(paste("AR(1) with", name))
}

gridExtra::grid.arrange(
  grobs = ar1_pacfs, nrow = 2, top = "PACF plots for AR(1) process with different theta")
```

![](types_of_noise_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->
