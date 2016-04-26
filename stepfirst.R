#stepfirst
fn <- function (x) {
  ifelse(x>46 & x<52, 1, 0)
}    
res <- fn(40:60)

fn <- function (x,y) {
  ifelse(x>46 & x<52 & y<12, 1, 0)
}    
datagrid <- expand.grid(i = 40:60, j = 0:20)
res <- fn(datagrid$i, datagrid$j)

#An other option is to use the functions for the apply-family

fn <- function (x) {
  ifelse(x>46 & x<52, 1, 0)
}    
res <- sapply(40:60, fn)

fn <- function (x,y) {
  ifelse(x>46 & x<52 & y<12, 1, 0)
}    
datagrid <- expand.grid(i = 40:60, j = 0:20)
res <- apply(datagrid, 1, function(z){
  fn(z["i"], z["j"])
}) 

#or you can use a nested loop

res <-NULL 
for (i in 40:60){
  for(j in 0:20){
    res <-c(res,fn(i,j))
  }
}
