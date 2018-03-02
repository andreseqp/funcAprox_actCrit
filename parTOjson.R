# ------------------------ generate json files  ------------------------------------------ #

library("jsonlite")

x <- list( alpha = 1:5, beta = "Bravo",
           gamma = list(a=1:3, b=NULL),
           delta = c(TRUE, FALSE) )

json <- toJSON( x )
prettify(json)



write_json(json,path = "test2.json")

