get_MI<-function(input_matrix){
  #col is gene, row is cell
  row_num<-nrow(input_matrix)#sum of cells
  col_num<-ncol(input_matrix)#sum of genes
  MI_matrix<-diag(x=1,row_num,row_num)#MI matrix
 #get MI
  i=1
  while(i<=(row_num-2)){
    
  print(i)
    
  cell<-input_matrix[1,]
  input_matrix<-input_matrix[-1,]
  MI<-getAllMIWz(input_matrix,cell,negateMI = TRUE)
  MI_matrix[(i+1):row_num,i]<-MI
  MI_matrix[i,(i+1):row_num]<-MI
  i=i+1
  
  }
  cell<-input_matrix[1,]
  input_matrix<-input_matrix[-1,]
  MI<-getMI(cell,input_matrix)
  MI_matrix[row_num,(row_num-1)]<-MI
  MI_matrix[(row_num-1),row_num]<-MI
  return(MI_matrix)
}

