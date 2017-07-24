re_trim <- function(x, side = "both") {
  switch(side[1],
         both = re_trim_both,
         left = re_trim_left,
         right = re_trim_right
  )(x)
}
re_trim_both <- function(x) re_replace_all(x,"^\\s+|\\s+$","")
re_trim_left <- function(x) re_replace(x,"^\\s+","")
re_trim_right <- function(x) re_replace(x,"\\s+$","")
