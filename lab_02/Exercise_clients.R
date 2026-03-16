# Đọc dữ liệu
clients<-read.csv("D:/PTVTQDL/data/clients.csv")
View(clients)

# 6 dòng đầu 
head(clients)

# kiểm tra cấu trúc
str(clients)

# Tóm tắt thống kê
summary(clients)

# Loại bỏ cột đầu tiên
head(clients[,-1]) # Xem trước khi xóa
clients<-clients[,-1] # Xóa cột 1

# Tìm các dòng dữ liệu thiếu
clients[!complete.cases(clients),]

# Đếm số dòng bị thiếu
length(clients[!complete.cases(clients),])

# Kiểm tra missing value theo từng cột
colSums(is.na(clients))

# Xử lý biến số - Income
# Kiểm tra phân bố thu nhập
summary(clients$Income)

# Kiểm tra kiểu dữ liệu
str(clients$Income)

# Chuyển sang numeric
clients$Income<-as.numeric(as.character(clients$Income))

# Tính median(bỏ NA)
median(clients$Income, na.rm = TRUE)

# Điền missing value bằng median
clients$Income[is.na(clients$Income)]<-median(clients$Income, na.rm=TRUE)

# Kiểm tra lại
clients$Income[is.na(clients$Income)]

# Kiểm tra phân bố 
summary(clients$Year_Birth)

# Điền giá trị NA bằng median
clients$Year_Birth[is.na(clients$Year_Birth)] <- round(median(clients$Year_Birth, na.rm = TRUE))

# Kiểm tra lại dữ liệu thiếu 
sum(!complete.cases(clients)) # Kiểm tra tất cả NA đã được điền chưa
clients[!complete.cases(clients), ] # Hiển thị tất cả các dòng còn missing

# Chuyển các biến sang kiểu factor
# Marital_Status
clients$Marital_Status <- factor(clients$Marital_Status)

# Education
clients$Education <- factor(
  clients$Education,
  levels = c("Basic", "2n Cycle", "Graduation", "Master", "PhD"),
  ordered = TRUE
)

# Response
clients$Response <- factor(clients$Response)

# Campaign variables
campaignVars <- c("AcceptedCmp1","AcceptedCmp2","AcceptedCmp3",
                  "AcceptedCmp4","AcceptedCmp5","Complain")

clients[campaignVars] <- lapply(clients[campaignVars], factor)

# Lưu kết quả
save(clients, file = "clientsInR.RData")
