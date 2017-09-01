paste_and <- function (strings) {
  
  noStrings = length(strings)
  if(noStrings > 1) {
    output = paste(strings[1:noStrings-1], collapse=", ")
    output = paste(output, "and", strings[noStrings], collapse=" ")
    # if(noStrings > 2) {
    #   output = paste(strings[1:noString-1], collapse=", ")
    #   output = paste(output, "and", strings[noString], collapse=" ")
    # } else {
    #   output = paste(strings[1], "and", strings[noString], collapse=" ")
    # }
  } else {
    output = strings
  }
}
