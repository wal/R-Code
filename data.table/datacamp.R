DT <- data.table(A = 1:5, B = c("a", "b", "c", "d", "e"), C = 6:10)

DT[, list(B,C)]



DT[, list(Total = sum(A), Mean = mean(C))]
DT[, list(B, C = sum(C))]  # C is recycled to fit lengthof B


# By using a list in J, returns a data.table .. otherwise it just executes whats in J and returns return value from last executed command

class(DT[,B]) # character vector
class(DT[,list(B)]) # data.table

# Chaining
DT <- data.table(A = c("c", "b", "a"), B = 1:6)
DT[,sum(B), by = A][order(A)] # sum and sort in a chain


# .SD - subset of Data - The subset of all columns provided by grouping in by (Does not include the grouping columns). It is only available in j
DT <- as.data.table(iris)
DT[,list(Sepal.Length = median(Sepal.Length), Sepal.Width = median(Sepal.Width)), by = Species]

DT[,lapply(.SD, median), by = Species]



# The := operator updates/deletes data.table columns by reference - therefore does not need an explicit assignment (e.g resuls <- )

# Create new columns with :=  in j
# Remove columns by assigning them to NULL in j   ex: DT[,y := NULL]
DT[, paste0("prefix",1:4) := NULL]


# Functional :=
DT[, `:=` (y = 1:5, x = 2)]

# combining := with i,j
DT[2:4, z := sum(y), by=x]





# set is a loopable, low overhead version of :=
DT <- data.table(x = 1:5, y = c(1,8,1,1,1), z = 2:6)
DT

for(i in 1:5) { 
  print(i)
  set(DT, i, 3L, i+1)
}

DT

# setnames - change column names
names(DT)
setnames(DT,"y","j")
names(DT)

# setcolorder - change column order
names(DT)
setcolorder(DT,c("z","x","j"))
names(DT)

# setkey - ability to set key on a DT

DT <- data.table(A = c("c", "b", "a"), B = 1:6)
DT["b"]
setkey(DT,A)
DT # Observe that data.table is now sorted by A

DT["b"]
DT["b", mult = "first"]
DT["b", mult = "last"]

DT[c("b", "d")] # returns NA for keys not in the data.table (e.g D)
DT[c("b", "d"), nomatch=0] # Does not return NA rows for unknown keys (e.g D)

# two column key
DT <- data.table(A = c("c", "b", "a"), B = 1:6)
setkey(DT, A,B)
DT # notice sorting
DT[list("b", 5)]

# Ordered Rolling Joins
DT <- data.table(A = c("a","a","b","b","c","c"), B=c(2,6,1,5,3,4), C=c(6,3,2,5,4,1)) 
setkey(DT, A, B) # 2 column key
DT
DT[.("b",4)]  # an equi-join
#    A B  C
# 1: b 4 NA
## Returns NA as no match in DT for C. It falls in the gap between 1 and 5

DT[.("b",4), roll = TRUE]  # a rolling join
#    A B C
# 1: b 4 2
## (2) because the value before the gap being rolled forward

DT[.("b",4), roll = "nearest"]  # a rolling join
#    A B C
# 1: b 4 5
## (5) because 4 is nearer to 5 than 2

DT[.("b",4), roll = Inf]  # always forward
DT[.("b",4), roll = -Inf]  # always backward




# Joining to prevaling observation (within a window)

DT <- data.table(A = c("a","a","b","b","c","c"), B=c(2,6,1,5,3,4), C=c(6,3,2,5,4,1)) 
setkey(DT, A, B) # 2 column key
DT
DT[.("b",4), roll = 2]


# control ends - what happens when data falls after the end ?

DT <- data.table(A = c("a","a","b","b","c","c"), B=c(2,6,1,5,3,4), C=c(6,3,2,5,4,1)) 
setkey(DT, A, B) # 2 column key
DT[.("b",5:8), roll=TRUE] # returns values, despite 7,8 not being in DT
DT[.("b",5:8), roll=TRUE, rollends=FALSE] # shows NA for 7,8







