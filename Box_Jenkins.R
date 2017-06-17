library('ggplot2')
library('forecast')
library('tseries')
##Assuming that return.data is the data is inputted when the function is recalled.
                    
box.jenkins <- function(return.data)
{
  
  ggplot(return.data)
  count_ts = ts(return.data[,c('cnt')])
  return.data$cnt = tsclean(count_ts)
  ggplots() + geom_line(data = return.data, aes(x='',y=clean_cnt) )
  return.data$cnt_ma = ma(return.data$clean_cnt, order = 7)
  return.data$cnt_ma30 = ma(return.data$clean_cnt, order = 30)
  
  ggplot() + 
    geom_line(data = return.data, aes(x = Date ,y = clean_cnt))
    geom_line(data = return.data, aes(x = Date, y = cnt_ma))    #Weekly Moving Average
    geom_line(data = return.data, aes(x = Date, y = cnt_ma30))  #Monthly Moving Average  
  
    count_ma = ts(na.omit(return.data$cnt_ma), frequency = 30)
    decomp = stl(count_ma, s.window ="periodic")
    deseasonal_cnt <- seasadj(decomp)
    plot(decomp)
    
    adf.test(count_ma, alternative = "Stationary")
    acf(count_ma, main = '')
    pacf(count_ma, main = '')
    
    auto.arima(deseasonal_cnt, seasonal = FALSE)
    fit <- auto.arima(deseasonal_cnt, seasonal=FALSE)
    tsdisplay(residuals(fit), lag.max=45, main='')
    #Iterated once.
    }
