# R-file-Matrix-Ratios
# This function can return all possible ratios in any given matrix.  At the moment, it lends itself to comparison by linear model, but not chloropleth mapping
#At the moment there is no way to handle missing values


```{r setup, include=FALSE}
setwd("E:/Thesis/Thesis-Excel-files/8-23")
getwd()

# I'm only doing this on a subset of my dataset
data <- myreflectance23[100:400]

#defining the initial function, the prefix stands for wavelength
pairwise.ratios <- function(x, prefix="wv", char=":"){
        n <- ncol(x)
        cn <- colnames(x)
        if(length(cn) == 0){
                cn <- gsub(" ", "0", formatC(seq.int(n), width=nchar(n)))
                cn <- paste(prefix, cn, sep="")
        }
        #utilizing the combine function in R
        cmb <- combn(n, 2)
        #setting up the ratio
        r1 <- apply(cmb, 2, function(j) x[, j[1]]/x[, j[2]])
        r2 <- apply(cmb, 2, function(j) x[, j[2]]/x[, j[1]])
        #applying the column names
        colnames(r1) <- apply(cmb, 2, function(j) paste(cn[j], collapse=char))
        colnames(r2) <- apply(cmb, 2, function(j) paste(cn[rev(j)], collapse=char))
        #binding names
        cbind(r1, r2)[, order(c(colnames(r1), colnames(r2)))]
}

results <- pairwise.ratios(data)
write.csv(results, file = "ratios23.csv", sep = ",")
write.table(t(results), "ratios_results.txt", sep="\t")
```
