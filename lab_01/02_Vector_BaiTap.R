# 1. Bài tập về chuyển đổi kiểu dữ liệu số: 
# Tạo một số thập phân, sau đó chuyển đổi số đó thành số nguyên 
# rồi thành ký tự. Quan sát và giải thích sự thay đổi về giá trị 
# và cách hiển thị của số đó qua mỗi lần chuyển đổi.

num<-3.12
print(num)
class(num)

num_int<-as.integer(num)
print(num_int)
class(num_int)

num_cha<-as.character(num_int)
print(num_cha)
class(num_cha)

# 2. Bài tập về ghép chuỗi: 
# Tạo hai biến chứa văn bản, tìm hiểu tài liệu về hàm paste() 
# và sử dụng nó để ghép các biến văn bản đã tạo. So sánh kết quả 
# của hàm paste() với hàm c() và giải thích sự khác biệt giữa chúng.

a1<-"Hồng"
b1<-"Vân"
v1<-paste(a1,b1)
print(v1)

v2<-paste(a1,b1, sep="-")
print(v2)

v3 <- paste("a", "b", "c", sep='->')
print(v3)

v4 <- paste0(a1, b1)
print(v4)

vectors_s <- c("a", "b", "c")
v5 <- paste(vectors_s)
print(v5)

v5 <- paste(vectors_s, collapse = ", ")
print(v5)

v6 <- paste(c("x", "y"), c("1", "2", "3", "4"), sep = "-", collapse = ";")
print(v6)

v7 <- paste(c(), c("1", "2", "3", "4"), recycle0 = FALSE)
print(v7)

# 3. Bài tập về xử lý ngày tháng: 
# Cho vector vecDate <- c("09:12:12", "28:02:16", "31:05:22"). 
# Hãy: 

vecDate <- c("09:12:12", "28:02:16", "31:05:22")
print(vecDate)

# a) Chuyển đổi vector này sang kiểu Date

dateVec <- as.Date(vecDate, format = "%d:%m:%y")
print(dateVec)

# b) Tính số ngày giữa các ngày trong vector với ngày hiện tại.

today<-Sys.Date()
diff_days<-today-dateVec
print(diff_days)

# 4. Bài tập tạo vector số: 
# Tạo vector "vec1" chứa các số từ 2 đến 8 và từ 17 đến 30 
# bằng cách viết code ngắn gọn nhất có thể.

vec1<-c(2:8, 17:30)
print(vec1)

# 5. Bài tập sử dụng hàm seq(): 
# Tạo vector "vec2" có cấu trúc: (2, 8, 14, 20, 26, 32) 
# bằng cách sử dụng hàm seq().

vec2<-seq(2,35, by=6)
print(vec2)

# 6. Bài tập lặp chuỗi: 
# Tạo một vector có cấu trúc: "2", "7", "a", "2", "7", "a", 
# "2", "7", "a" bằng cách sử dụng hàm rep().

v6<-rep(c("2","7","a"), times=3)
print(v6)

# 7. Bài tập về số chia hết: 
# Tạo một vector độ dài 100 chứa các số liên tiếp chia hết cho 3.

v7<-seq(from=3, by=3, length.out=100)
print(vec7)

# 8. Bài tập tạo mẫu phức tạp: 
# Sử dụng một dòng code duy nhất để tạo vector "vec3" có cấu trúc: 
#   (1, 1, 3, 3, 5, 5, 7, 7, 9, 9) lặp lại 3 lần.

vec3<-rep(seq(from=1, to=9), times=3, each=2)
print(vec3)

# 9. Bài tập về số ngẫu nhiên: 
# Tạo vector "vec4" gồm 50 số bằng hàm runif(). 
# Giải thích chức năng của hàm runif() và sử dụng các số đã tạo 
# để tạo vector mới chứa 50 số nguyên ngẫu nhiên trong khoảng 0-20.
# runif() tạo số ngẫu nhiên phân phối đều 

vec4<-runif(50)
print(vec4)

vec4<-(vec4*21)
print(vec4)

vec4<-floor(vec4)
print(vec4)


# 10. Bài tập truy xuất phần tử: 
# In ra các giá trị của phần tử thứ 5, 10 và 26 từ vector 
# vừa tạo ở câu 9

vec4[5]
vec4[c(5,10,26)]

# 11. Bài tập về dãy có quy luật: 
# In ra giá trị của các phần tử cách đều nhau trong vector từ câu 9, 
# bắt đầu từ phần tử thứ 5 và lấy cứ mỗi phần tử thứ hai. 
# Gợi ý: Sử dụng hàm seq().
indices<-seq(from=5, to=length(vec4),by=2)
result<-vec4[indices]
result

