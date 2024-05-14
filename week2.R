calculateProportion <- function(groupCount, otherGroupCount) {
  totalCount <- groupCount + otherGroupCount
  proportion <- groupCount / totalCount
  print(proportion)
}

myProportion(155, 172)
