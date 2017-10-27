
impute<-function(count_matrix,MI_matrix,SPM_matrix,PRN_matrix){
  
  a1=1.0
  a=0.6
  b=0.3
  c=0.1
  x1=2
  x2=30
  impute_matrix_MI<-count_matrix
  impute_matrix_SPM<-count_matrix
  impute_matrix_PRN<-count_matrix
  sum_cell<-nrow(count_matrix)
  sum_gene<-ncol(count_matrix)
  
  
  n=1
  while(n<=sum_cell){
  #EACH CELL
  cell<-count_matrix[n,]
  MI<-MI_matrix[n,]
  order_MI<-order(-MI)
  flag=TRUE
  while(flag){
  i=1
  
  while(i<=sum_gene){
    
    flag=FALSE
    if(cell[i]==0){
      gene<-count_matrix[,i]
      near_gene_MI_1<-mean(gene[order_MI[2:11]])

      impute_gene<-near_gene_MI_1
      if(impute_gene!=0){
        flag=TRUE
      }
      cell[i]<-impute_gene
    }
    i=i+1
  }
 }
  impute_matrix_MI[n,]<-cell
  print(n)
  n=n+1
  }
  
  n=1
  while(n<=sum_cell){
    #EACH CELL
    cell<-count_matrix[n,]
    SPM<-SPM_matrix[n,]
    order_SPM<-order(-SPM)
    flag=TRUE
    while(flag){
      i=1
      
      while(i<=sum_gene){
        
        flag=FALSE
        if(cell[i]==0){
          gene<-count_matrix[,i]
          near_gene_SPM_1<-mean(gene[order_SPM[2:11]])
          #near_gene_SPM_2<-mean(gene[order_SPM[x1+x2+1:x1+2*x2]])
          #near_gene_SPM_3<-mean(gene[order_SPM[x1+2*x2+1:x1+3*x2]])
          impute_gene<-a1*near_gene_SPM_1#+b*near_gene_SPM_2+c*near_gene_SPM_3
          if(impute_gene!=0){
            flag=TRUE
          }
          cell[i]<-impute_gene
        }
        i=i+1
      }
    }
    impute_matrix_SPM[n,]<-cell
    print(n)
    n=n+1
  }
  
  n=1
  while(n<=sum_cell){
    #EACH CELL
    cell<-count_matrix[n,]
    PRN<-PRN_matrix[n,]
    order_PRN<-order(-PRN)
    flag=TRUE
    while(flag){
      i=1
      
      while(i<=sum_gene){
        
        flag=FALSE
        if(cell[i]==0){
          gene<-count_matrix[,i]
          near_gene_PRN_1<-mean(gene[order_PRN[2:11]])
         # near_gene_PRN_2<-mean(gene[order_PRN[x1+x2+1:x1+2*x2]])
          #near_gene_PRN_3<-mean(gene[order_PRN[x1+2*x2+1:x1+3*x2]])
          impute_gene<-a1*near_gene_PRN_1#+b*near_gene_PRN_2+c*near_gene_PRN_3
          if(impute_gene!=0){
            flag=TRUE
          }
          cell[i]<-impute_gene
        }
        i=i+1
      }
    }
    impute_matrix_PRN[n,]<-cell
    print(n)
    n=n+1
  }
  
  impute_matrix <- (impute_matrix_PRN+impute_matrix_SPM+impute_matrix_MI)/3
  return(impute_matrix)
}