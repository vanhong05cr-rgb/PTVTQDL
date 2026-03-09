# Tạo matrix từ vector
# Cách 1: Chỉ định số hàng, số cột 

mat1<-matrix(1:6,nrow = 2, ncol = 3);
mat1

# Cách 2: chỉ định cách điền (byrow)
mat2<-matrix(1:6, nrow = 3, byrow = TRUE)
mat2

# Tạo matrix rỗng :
empty_mat<-matrix(NA, nrow = 3, ncol = 5)
empty_mat

#  Vd:
grades<-matrix(c(8.5, 9, 7.5, 8.8,
                 9.2, 7.8, 8.5, 9.0,
                 7.6, 8.2, 8.9, 8.5),
               nrow = 3,
               ncol = 4,
               byrow = TRUE)
grades
rownames(grades)<-c("Nguyen Thi Hong Van", "NAUADEN", "Nguyen Nau")
colnames(grades)<-c("Toán", "Văn", "Tin", "R")
grades

# Dữ liệu bán hàng theo tháng
sales <- matrix(
  c(150, 200, 180,
    120, 160, 140,
    180, 220, 200),
  nrow = 3,
  ncol = 3,
  byrow = TRUE,
  dimnames = list(
    c("SP1", "SP2", "SP3"),
    c("T1", "T2", "T3")
  )
)
sales

# Một phần tử tại ví trị hàng 1, cột 2
sales[1, 2]

# Một hàng, một cột
sales[1,]
sales[,2]

# Nhiều hàng, nhiều cột
sales[1:2,]
sales[,2:3]

# Tính toán
rowSums(sales)
colSums(sales)
rowMeans(sales)
colMeans(sales)

barplot(rowSums(sales))
