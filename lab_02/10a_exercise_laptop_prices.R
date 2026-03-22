# BƯỚC 1: Load và Khám phá Dữ liệu

# Load dữ liệu
laptop <- read.csv("D:/PTVTQDL/data/laptops.csv")

# Xem dữ liệu
View(laptop)

# Xem 6 dòng đầu tiên
head(laptop)

# Xem 6 dòng cuối
tail(laptop)

# Kiểm tra cấu trúc dữ liệu
str(laptop)

# Tóm tắt thống kê
summary(laptop)

# Kiểm tra số dòng và cột
dim(laptop)

# Kiểm tra tên cột
colnames(laptop)

# Xem giá trị unique của một số cột
unique(laptop$Company)
unique(laptop$TypeName)
unique(laptop$OpSys)
unique(laptop$Cpu)
unique(laptop$Gpu)

# Đếm số lượng từng loại
table(laptop$Company)
table(laptop$TypeName)
table(laptop$OpSys)

# Loại bỏ cột đầu tiên (cột ID không cần thiết)

# Xem trước khi xóa
head(laptop[,-1])

# Xóa cột đầu
laptop <- laptop[,-1]

# BƯỚC 2: Xử lý Missing Data

# Phát hiện missing data
laptop[!complete.cases(laptop), ]

# Đếm số dòng bị thiếu
length(laptop[!complete.cases(laptop), ])

# Đếm số NA từng cột
colSums(is.na(laptop))

# Xóa dữ liệu trùng lặp

sum(duplicated(laptop)) # Đếm dòng trùng

laptop <- laptop[!duplicated(laptop), ] # Xóa dòng trùng

# Xử lý biến số (Numeric)

# 1. Inches

# Xem phân bố
summary(laptop$Inches)

# Tính median
median_inches <- median(laptop$Inches, na.rm = TRUE)

# Điền missing bằng median
laptop$Inches[is.na(laptop$Inches)] <- median_inches

# Kiểm tra lại
laptop$Inches[is.na(laptop$Inches)]

# 2. Price_euros

# Xem phân bố
summary(laptop$Price_euros)

# Tính median
median_price <- median(laptop$Price_euros, na.rm = TRUE)

# Điền missing bằng median
laptop$Price_euros[is.na(laptop$Price_euros)] <- median_price

# Kiểm tra lại
laptop$Price_euros[is.na(laptop$Price_euros)]

# 3. Ram

# Xem phân bố
summary(laptop$Ram)

# Tính median
median_ram <- median(laptop$Ram, na.rm = TRUE)

# Điền missing nếu có
laptop$Ram[is.na(laptop$Ram)] <- median_ram

# Kiểm tra lại
laptop$Ram[is.na(laptop$Ram)]

# 4. Weight

# Xem phân bố
summary(laptop$Weight)

# Tính median
median_weight <- median(laptop$Weight, na.rm = TRUE)

# Điền missing
laptop$Weight[is.na(laptop$Weight)] <- median_weight

# Kiểm tra lại
laptop$Weight[is.na(laptop$Weight)]

# 5. Cpu_speed

# Xem phân bố
summary(laptop$Cpu_speed)

# Tính median
median_cpu <- median(laptop$Cpu_speed, na.rm = TRUE)

# Điền missing
laptop$Cpu_speed[is.na(laptop$Cpu_speed)] <- median_cpu

# Kiểm tra lại
laptop$Cpu_speed[is.na(laptop$Cpu_speed)]

# 6. Memory_size

# Xem phân bố
summary(laptop$Memory_size)

# Tính median
median_memory <- median(laptop$Memory_size, na.rm = TRUE)

# Điền missing
laptop$Memory_size[is.na(laptop$Memory_size)] <- median_memory

# Kiểm tra lại
laptop$Memory_size[is.na(laptop$Memory_size)]

# 7. PPI

# Xem phân bố
summary(laptop$PPI)

# Tính median
median_ppi <- median(laptop$PPI, na.rm = TRUE)

# Điền missing
laptop$PPI[is.na(laptop$PPI)] <- median_ppi

# Kiểm tra lại
laptop$PPI[is.na(laptop$PPI)]

# 8. Res_width

# Xem phân bố
summary(laptop$Res_width)

# Tính median
median_width <- median(laptop$Res_width, na.rm = TRUE)

# Điền missing
laptop$Res_width[is.na(laptop$Res_width)] <- median_width

# Kiểm tra lại
laptop$Res_width[is.na(laptop$Res_width)]

# 9. Res_height

# Xem phân bố
summary(laptop$Res_height)

# Tính median
median_height <- median(laptop$Res_height, na.rm = TRUE)

# Điền missing
laptop$Res_height[is.na(laptop$Res_height)] <- median_height

# Kiểm tra lại
laptop$Res_height[is.na(laptop$Res_height)]

# 10. SSD

# Xem phân bố
summary(laptop$SSD)

# Tính median
median_ssd <- median(laptop$SSD, na.rm = TRUE)

# Điền missing
laptop$SSD[is.na(laptop$SSD)] <- median_ssd

# Kiểm tra lại
laptop$SSD[is.na(laptop$SSD)]

# 11. HDD

# Xem phân bố
summary(laptop$HDD)

# Tính median
median_hdd <- median(laptop$HDD, na.rm = TRUE)

# Điền missing
laptop$HDD[is.na(laptop$HDD)] <- median_hdd

# Kiểm tra lại
laptop$HDD[is.na(laptop$HDD)]

# 12. Cpu_speed (backup kiểm tra lại lần 2 - tăng độ chắc chắn)

# Xem phân bố
summary(laptop$Cpu_speed)

# Tính median
median_cpu2 <- median(laptop$Cpu_speed, na.rm = TRUE)

# Điền missing
laptop$Cpu_speed[is.na(laptop$Cpu_speed)] <- median_cpu2

# Kiểm tra lại
laptop$Cpu_speed[is.na(laptop$Cpu_speed)]

# 13. Price_euros (kiểm tra lại lần cuối)

# Xem phân bố
summary(laptop$Price_euros)

# Tính median
median_price2 <- median(laptop$Price_euros, na.rm = TRUE)

# Điền missing
laptop$Price_euros[is.na(laptop$Price_euros)] <- median_price2

# Kiểm tra lại
laptop$Price_euros[is.na(laptop$Price_euros)]

# Xử lý biến phân loại (Categorical)
laptop$Company[is.na(laptop$Company)] <- "Unknown"
laptop$Product[is.na(laptop$Product)] <- "Unknown"
laptop$TypeName[is.na(laptop$TypeName)] <- "Unknown"
laptop$ScreenResolution[is.na(laptop$ScreenResolution)] <- "Unknown"
laptop$Cpu[is.na(laptop$Cpu)] <- "Unknown"
laptop$Memory[is.na(laptop$Memory)] <- "Unknown"
laptop$Gpu[is.na(laptop$Gpu)] <- "Unknown"
laptop$OpSys[is.na(laptop$OpSys)] <- "Unknown"

# Kiểm tra lại
laptop[!complete.cases(laptop), ]

# BƯỚC 3: Chuyển đổi Categorical Data thành Factor

str(laptop)

# Company
summary(factor(laptop$Company))
laptop$Company <- factor(laptop$Company)

# TypeName
summary(factor(laptop$TypeName))
laptop$TypeName <- factor(laptop$TypeName)

# OpSys
summary(factor(laptop$OpSys))
laptop$OpSys <- factor(laptop$OpSys)

# Cpu
summary(factor(laptop$Cpu))
laptop$Cpu <- factor(laptop$Cpu)

# Gpu
summary(factor(laptop$Gpu))
laptop$Gpu <- factor(laptop$Gpu)

# Kiểm tra lại
str(laptop)

# BƯỚC 4: Tự động hóa với lapply()

# Danh sách biến categorical
catVariables <- c("Company", "TypeName", "Cpu", "Gpu", "OpSys")

# Xem dữ liệu
laptop[, catVariables]

# Kiểm tra từng biến
lapply(laptop[, catVariables], summary)

# Chuyển sang factor
laptop[, catVariables] <- lapply(laptop[, catVariables], factor)

# Kiểm tra lại
lapply(laptop[, catVariables], levels)

# Xử lý dữ liệu bẩn (case study OpSys)

# Chuẩn hóa khoảng trắng
laptop$OpSys <- trimws(laptop$OpSys)

# Windows
laptop$OpSys[laptop$OpSys %in% c("Windows 10", "Windows 10 S")] <- "Windows 10"

# macOS
laptop$OpSys[laptop$OpSys %in% c("Mac OS X", "macOS")] <- "macOS"

# Linux (nếu có biến thể)
laptop$OpSys[laptop$OpSys %in% c("linux")] <- "Linux"

# Kiểm tra lại
summary(factor(laptop$OpSys))


# BƯỚC 5: Xử lý biến mức độ

# RAM
laptop$Ram <- gsub("GB", "", laptop$Ram)
laptop$Ram <- as.numeric(laptop$Ram)

summary(laptop$Ram)

# Weight
laptop$Weight <- gsub("kg", "", laptop$Weight)
laptop$Weight <- as.numeric(laptop$Weight)

summary(laptop$Weight)

# BƯỚC 6: Kiểm tra kết quả cuối cùng

str(laptop)

summary(laptop)

sum(!complete.cases(laptop)) # Phải = 0

sum(duplicated(laptop)) # Phải = 0

# Lưu dữ liệu sạch

write.csv(laptop, "laptop_cleaned.csv", row.names = FALSE)

