monthquarter <- function(m){
  if(m=="Jan"||m=="Feb"||m=="Mar"){
    result="Q1"
  }
  else if(m=="Apr"||m=="May"||m=="Mar"){
    result="Q2"
  }
  else if(m=="Aug"||m=="Jun"||m=="Jul"){
    result="Q3"
  }
  else{
    result="Q4"
  }
  return(result)
  
}