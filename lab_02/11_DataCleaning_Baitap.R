### LẤY ĐIỂM THỰC HÀNH
### Các bài tập ######################################################################
# 1. Đọc mô tả dữ liệu phân tích tính cách khách hàng và tải 
# vào R (file clients.csv) với tên biến là "clients".

# Đọc dữ liệu
clients<-read.csv("D:/PTVTQDL/data/clients.csv")
View(clients)

# 6 dòng đầu 
head(clients)

# 2. Xem qua cấu trúc dữ liệu và kiểm tra các lớp (classes) đã được gán 
# cho các biến trong bộ dữ liệu.

# kiểm tra cấu trúc
str(clients)

# Xem kiểu dữ liệu chi tiết
sapply(clients, class)

# Tóm tắt thống kê
summary(clients)

# Loại bỏ cột đầu tiên
head(clients[,-1]) # Xem trước khi xóa
clients<-clients[,-1] # Xóa cột 1

# 3. Kiểm tra xem có giá trị nào bị thiếu trong bộ dữ liệu không.
# a) Những biến nào có chứa giá trị bị thiếu?

clients[!complete.cases(clients),]
colSums(is.na(clients))

# b) Điền các giá trị bị thiếu bằng giá trị trung bình hoặc trung vị của biến đó.
# Trước khi điền, hãy xem xét bản chất của biến. Nếu là số nguyên (ví dụ: năm sinh),
# thì hãy điền giá trị phù hợp với bản chất của biến (chúng ta không muốn năm sinh là 1995.832, phải không? ;)).

# Với biến số numeric
clients$Income<-as.numeric(as.character(clients$Income))
# Xóa ký tự không phải số
clients$Income <- gsub("[^0-9]", "", clients$Income)
# Chuyển sang numeric
clients$Income <- as.numeric(clients$Income)

#Điền NA
clients$Income[is.na(clients$Income)] <- median(clients$Income, na.rm = TRUE)

# Xử lý MntWines (biến số)
clients$MntWines[is.na(clients$MntWines)] <- median(clients$MntWines, na.rm = TRUE)

# Với biến Year_Birth (số nguyên)
clients$Year_Birth[is.na(clients$Year_Birth)] <- round(median(clients$Year_Birth, na.rm = TRUE))

# Với biến phân loại (categorical)
# Tìm mode
mode_edu <- names(sort(table(clients$Education), decreasing = TRUE))[1]

# Điền NA
clients$Education[is.na(clients$Education)] <- mode_edu

# Tìm mode
mode_response <- names(sort(table(clients$Response), decreasing = TRUE))[1]
# Ép về numeric
clients$Response <- as.numeric(clients$Response)
clients$Response[is.na(clients$Response)] <- as.numeric(mode_response)
# Điền NA
clients$Response[is.na(clients$Response)] <- mode_response

# c) Bạn sử dụng đoạn mã nào để điền các giá trị bị thiếu của Year_Birth (nếu có)?

clients$Year_Birth[is.na(clients$Year_Birth)] <- round(median(clients$Year_Birth, na.rm = TRUE))

# 4. a) Kiểm tra xem tất cả các giá trị bị thiếu đã được điền đầy đủ chưa. Nếu chưa, lặp lại bước 3

colSums(is.na(clients))
sum(!complete.cases(clients))

# b) Bạn sẽ dùng đoạn mã nào để hiển thị tất cả các dòng vẫn còn chứa dữ liệu bị thiếu?

clients[!complete.cases(clients), ]

# 5. a) Xem xét những biến nào nên chuyển đổi thành kiểu "factor"?
# Gợi ý: Đây thường là các biến văn bản có một số giá trị cụ thể và lặp lại.
# Chúng cũng có thể là các biến được biểu diễn bằng số nhưng không mang "ý nghĩa số học"
# - ví dụ: biến "education" với các giá trị 2, 3, 4 thực chất đại diện cho các cấp độ
# giáo dục liên tiếp (ý nghĩa logic) thay vì số năm học tập chính xác (ý nghĩa số học).

# Xem các biến dạng character (ứng viên cho factor)
sapply(clients, class)

# Liệt kê các biến dạng character
names(clients)[sapply(clients, is.character)]

# b) Bạn sẽ dùng đoạn mã ngắn nhất nào để chuyển đổi biến Marital_Status?

clients$Marital_Status <- as.factor(clients$Marital_Status)

# 6. a) Xem xét biến nào trong số các biến đã xác định ở trên nên được
# chuyển đổi thành kiểu 'ordered factor' (biến phân loại có thứ tự).
# Gợi ý: Biến kiểu 'ordered factor' nên chứa các mức có thứ tự logic
# - ví dụ: biến 'education' với các giá trị 'primary', 'secondary'
# và 'tertiary'. Trong trường hợp này, việc giữ thứ tự các mức là quan trọng.
# Một ví dụ điển hình khác của biến ordered factor là các câu trả lời
# khảo sát sử dụng thang đo Likert (https://en.wikipedia.org/wiki/Likert_scale).

# Xem các giá trị của Education để kiểm tra thứ tự
unique(clients$Education)

# Hoặc xem tần suất
table(clients$Education)

# b) Bạn sẽ dùng đoạn mã nào để chuyển đổi biến Education? Giả sử rằng
# 2n nghĩa là giáo dục trung học và graduation tương đương với bảo vệ bằng cử nhân.

clients$Education <- factor(
  clients$Education,
  levels = c("Basic", "2n Cycle", "Graduation", "Master", "PhD"),
  ordered = TRUE
)

# 7. Chuyển đổi các biến đã xác định trong bước 5 và 6 thành các lớp thích hợp.

# Factor thường
clients$Marital_Status <- as.factor(clients$Marital_Status)

# Ordered factor
clients$Education <- factor(
  clients$Education,
  levels = c("Basic", "2n Cycle", "Graduation", "Master", "PhD"),
  ordered = TRUE
)

clients$Response <- factor(clients$Response)

campaignVars <- c("AcceptedCmp1","AcceptedCmp2","AcceptedCmp3",
                  "AcceptedCmp4","AcceptedCmp5","Complain")

clients[campaignVars] <- lapply(clients[campaignVars], factor)

# 8. Lưu kết quả để tham khảo sau này! Sử dụng file RData với tên "clientsInR".

save(clients, file = "clientsInR.RData")

