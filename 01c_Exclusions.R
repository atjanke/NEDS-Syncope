# These are functions to help facilitate applying the exclusion criteria


'%ni%' <- Negate('%in%')

Exclude <- function(DATA,X) {
  DATA = subset(DATA,i10_dx1 %ni%  X & i10_dx2 %ni%  X & i10_dx3 %ni%  X & i10_dx4 %ni%  X & i10_dx5 %ni%  
                  X & i10_dx6 %ni%  X & i10_dx7 %ni%  X & i10_dx8 %ni%  X & i10_dx9 %ni%  X & i10_dx10 %ni% 
                  X & i10_dx11 %ni% X & i10_dx12 %ni% X & i10_dx13 %ni% X & i10_dx14 %ni% X & i10_dx15 %ni% 
                  X & i10_dx16 %ni% X & i10_dx17 %ni% X & i10_dx18 %ni% X & i10_dx19 %ni% X & i10_dx20 %ni% 
                  X & i10_dx21 %ni% X & i10_dx22 %ni% X & i10_dx23 %ni% X & i10_dx24 %ni% X & i10_dx25 %ni% 
                  X & i10_dx26 %ni% X & i10_dx27 %ni% X & i10_dx28 %ni% X & i10_dx29 %ni% X & i10_dx30 %ni% 
                  X & i10_dx31 %ni% X & i10_dx32 %ni% X & i10_dx33 %ni% X & i10_dx34 %ni% X & i10_dx35 %ni% X)
  return(DATA)
}

Identify <- function(DATA,X) {
  DATA = subset(DATA,i10_dx1 %in%  X | i10_dx2 %in%  X | i10_dx3 %in%  X | i10_dx4 %in%  X | i10_dx5 %in%  X
                | i10_dx6 %in%  X | i10_dx7 %in%  X | i10_dx8 %in%  X | i10_dx9 %in%  X | i10_dx10 %in% X |
                  i10_dx11 %in% X | i10_dx12 %in% X | i10_dx13 %in% X | i10_dx14 %in% X | i10_dx15 %in% X |
                  i10_dx16 %in% X | i10_dx17 %in% X | i10_dx18 %in% X | i10_dx19 %in% X | i10_dx20 %in% X |
                  i10_dx21 %in% X | i10_dx22 %in% X | i10_dx23 %in% X | i10_dx24 %in% X | i10_dx25 %in% X |
                  i10_dx26 %in% X | i10_dx27 %in% X | i10_dx28 %in% X | i10_dx29 %in% X | i10_dx30 %in% X |
                  i10_dx31 %in% X | i10_dx32 %in% X | i10_dx33 %in% X | i10_dx34 %in% X | i10_dx35 %in% X)
  return(DATA)
}
