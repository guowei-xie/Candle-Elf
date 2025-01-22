trade_buy <- function(accnt, curr, price){
  if(price >= curr$low & price <= curr$high){
    lots <- floor(accnt / (100 * price))
  }else{
    lots <- 0
  }
  
  # cache result
  # ...
  
  return(lots)
}

trade_sell <- function(hold, curr, price){
  if(price >= curr$low & price <= curr$high){
    lots <- hold
  }else{
    lots <- 0
  }
  
  # cache result
  # ...
  
  return(lots)
} 
