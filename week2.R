calculateProportion <- function(groupCount, totalCount) {
  proportion <- groupCount / totalCount
  roundedProportion <- round(proportion, 3)
  print(roundedProportion)
}

toDecimal <- function(num){
   num / 100
}

myProportion(0.916, toDecimal(12.4))
