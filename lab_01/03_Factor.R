# Unordered Factor (Factor không có thứ tự)
colors <-factor(c("red", "green", "blue", "yellow"))
class(colors)
levels(colors)


# Ordered Factor(Factor có thứ tự)
ratings <-factor( c("low", "high", "medium", "high"),
                  levels = c("low", "medium", "high"), # low < medium < high
                  ordered = TRUE)
str(ratings)


# Dữ liệu giáo dục
education<-factor( c("High School", "Bachelor", "Master", "PhD"),
                   levels = c("High School", "Bachelor", "Master", "PhD"),
                   ordered = TRUE)
str(education)
levels(education)


# Tạo dữ liệu điểm số của học sinh
grades<-factor(
  c("Giỏi", "Khá", "Trung bình", "Giỏi", "Khá", "Yếu"),
  levels = c("Yếu", "Trung bình", "Khá", "Giỏi"),
  ordered = TRUE  # Có thứ tự từ yếu đến giỏi
)
table(grades)


# Khảo sát khách hàng 
satisfaction<-factor(
  c("Rất thích", "Thích", "Bình thường", "Không thích", 
    "Thích", "Rất thích", "Bình thường"),
  levels = c("Không thích", "Bình thường", "Thích", "Rất thích"),
  ordered = TRUE
)
table(satisfaction)


# Vẽ biểu đồ
barplot(table(satisfaction))
# Kích cỡ áo
sizes<-factor(c("M", "L", "S", "XL", "M", "S", "M", "L", "XL", "M"),
              levels = c("S", "M", "L", "XL"),
              ordered = TRUE)
# Thống kê
summary(sizes)