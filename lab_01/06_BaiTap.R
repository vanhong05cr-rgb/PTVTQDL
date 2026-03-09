# 1. Tạo một factor với các giá trị "a", "b", "c" có độ dài 7. 
# Thêm nhãn "Letter A", "Letter B", "Letter C". 
# Tóm tắt giá trị của factor.

f <- factor(
  c("a", "b", "c", "a", "b", "c", "a"),
  levels = c("a", "b", "c"),
  labels = c("Letter A", "Letter B", "Letter C")
)

summary(f)


# 2. Tạo một vector số có giá trị từ 1-4 và độ dài 10. 
# Bạn có thể sử dụng bất kỳ hàm nào để tạo vector. 
# Giá trị có thể được sắp xếp ngẫu nhiên. 
# Tóm tắt biến và kiểm tra kiểu của nó. 
# Sau đó sử dụng vector này để tạo một factor có thứ tự. 
# Đặt các mức "low" "medium" "high" "very high". 
# Tóm tắt giá trị và so sánh với vector ban đầu.

v <- sample(1:4, size = 10, replace = TRUE)
v

summary(v)
class(v)

f_ord <- factor(
  v,
  levels = c(1, 2, 3, 4),
  labels = c("low", "medium", "high", "very high"),
  ordered = TRUE
)

summary(f_ord)


# 3. Tạo một ma trận có 5 hàng và 2 cột, điền số 0. 
# Lưu vào biến "table".
table <- matrix(0, nrow = 5, ncol = 2)
table

# a) Điền cột 1 với giá trị 3
table[, 1] <- 3

# b) Đặt phần tử thứ 3 của cột 2 thành 20
table[3, 2] <- 20

# c) In các giá trị của cột 2. Kiểm tra kiểu giá trị trong cột này
table[, 2]
class(table[, 2])

# d) Thay đổi phần tử thứ 4 của cột 2 thành "twelve". 
table[4, 2] <- "twelve"
table[, 2]
class(table[, 2])

# In lại giá trị của cột 2. Kiểm tra kiểu của chúng. Có gì khác?
# e) Kiểu giá trị của cột 1 là gì? Tại sao?
class(table[, 1])

# 4. Tạo bốn biến với các kiểu khác nhau (vectors, matrices, 
#                                           single values).
# Tạo một list từ các đối tượng này đặt tên "myList".
v1 <- c(1, 2, 3)
m1 <- matrix(1:4, nrow = 2)
s1 <- 10
v2 <- c("a", "b", "c", "d", "e")

myList <- list(v1, m1, s1, v2)
myList

# a) Lấy phần tử thứ hai của list và thêm một giá trị vào đó.
# Lưu thay đổi để nó hiển thị trong list.
myList[[2]] <- c(myList[[2]], 99)

# b) Thêm phần tử mới vào cuối list - tạo thành vector 6 phần tử 
# với bất kỳ kiểu nào.
myList[[5]] <- c(1, 2, 3, 4, 5, 6)

# c) In phần tử thứ 4 của đối tượng cuối cùng trong list.
myList[[5]][4]

# d) Thay đổi giá trị của phần tử thứ 5 của đối tượng cuối cùng 
# thành NA.
myList[[5]][5] <- NA
myList[[5]]
