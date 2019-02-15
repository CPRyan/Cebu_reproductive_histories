# A concentrated "ifelse" wrapper for running long conditional arguments. 
# Located here: https://edwinth.github.io/blog/ifelse-wrapper/



i <- function(if_stat, then) {
  if_stat <- lazyeval::expr_text(if_stat)
  then    <- lazyeval::expr_text(then)
  sprintf("ifelse(%s, %s, ", if_stat, then)
}




e <- function(else_ret) {
  else_ret <- lazyeval::expr_text(else_ret)
  else_ret
}


ie <- function(...) {
  args <- list(...)
  
  for (i in 1:(length(args) - 1) ) {
    if (substr(args[[i]], 1, 6) != "ifelse") {
      stop("All but the last argument, need to be i functions.", call. = FALSE)
    }
  }
  if (substr(args[[length(args)]], 1, 6) == "ifelse"){
    stop("Last argument needs to be an e function.", call. = FALSE)
  }
  args$final <- paste(rep(')', length(args) - 1), collapse = '')
  eval_string <- do.call('paste', args)
  eval(parse(text = eval_string))
}



set.seed(0310)
x <- runif(1000, 1, 20)
y <- runif(1000, 1, 20)


ie(
  i(x < 5 & y < 5,   'A'),
  i(x < 5 & y < 15,  'B'),
  i(x < 5,           'C'),
  i(x < 15 & y < 5,  'D'),
  i(x < 15 & y < 15, 'E'),
  i(y < 5,           'F'),
  i(y < 15,          'G'),
  e('H')
)
