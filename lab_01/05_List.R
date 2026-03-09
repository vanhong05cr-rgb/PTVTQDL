# Tạo một list đơn giản 
student<-list(name = 'Van',
              age = 21,
              grades = c(8.5, 9.0, 9.5),
              passed = TRUE)

# Truy cập dữ liệu 
# Cách 1: Dùng [] trả về list con
student[1]
class(student[1])

# Cách 2: Dùng [[]] trả về giá trị 
student[[1]]
class(student[[1]])

student[[3]][c(1,3)]

# Cách 3: Dùng $ truy cập theo tên
student$grades

# Thêm giá trị vào 
student$id<-'0112345678'
print(student)

# Cập nhật giá trị
student$grades<-c(10,10,10)
student
