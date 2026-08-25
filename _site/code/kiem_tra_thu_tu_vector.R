kiem_tra_thu_tu_vector <- function(input_1, input_2){
  
if( all (length(setdiff(input_1, input_2)) == 0,
         length(setdiff(input_2, input_1)) == 0,
         setequal(input_1, input_2)
         )
    ){
  
  if(!identical(input_1, input_2)){
    
  kq <- "Hai vector giống nhau về thành phần, nhưng khác thứ tự"
    
  } else {
    
    kq <- "Hai vector giống y chang nhau về thành phần và thứ tự"
    
  }
  
  
} else {
  
  kq <-  "Hai vector khác nhau về thành phần"
}
  
  print(kq)
  
}