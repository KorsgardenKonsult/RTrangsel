
jsonList2DFrame <- function(jsonList){
  return(rbind(read.table(textConnection(""), col.names = jsonList$header), 
               matrix(unlist(jsonList$data),ncol=length(jsonList$header),byrow=TRUE,
                      dimnames = list(NULL, jsonList$header))))
}
