# BÀI THỰC HÀNH: PHÂN TÍCH CHẤT LƯỢNG NƯỚC BẰNG HỒI QUY LOGISTIC
# ==========================================================

# TÓM TẮT BÀI TOÁN:
# Bài thực hành này sử dụng bộ dữ liệu water_potability để dự đoán liệu một mẫu nước có uống được hay không
# dựa trên các chỉ số hóa học và vật lý khác nhau. Chúng ta sẽ sử dụng mô hình hồi quy logistic để
# xây dựng bộ phân loại nhị phân, đánh giá hiệu suất của mô hình và trực quan hóa kết quả.
# Biến mục tiêu: Potability (0 = không uống được, 1 = uống được)

install.packages("readr")
library(caret) # Để chia dữ liệu, đánh giá mô hình
library(pROC) # Để vẽ đường cong ROC

library(readr)
water_potability <- read_csv("D:/PTVTQDL/data/water_potability.csv")
View(water_potability)
dataset_name <- "Water Potability Dataset"

# Chuyển biến mục tiêu thành factor
water_potability$Potability <- as.factor(water_potability$Potability)
levels(water_potability$Potability) <- c("Not Drinkable", "Drinkable")

# Thông tin cơ bản
cat("Thông tin về tập dữ liệu", dataset_name, ":\n")
cat("Số mẫu:", nrow(water_potability), "\n")
cat("Số đặc trưng:", ncol(water_potability)-1, "\n\n")

# Cấu trúc dữ liệu
cat("Cấu trúc dữ liệu:\n")
str(water_potability)

# Hiển thị dữ liệu
print(head(water_potability))
print(summary(water_potability))

# Kiểm tra NA
cat("\nSố giá trị thiếu:\n")
print(colSums(is.na(water_potability)))

# Xử lý NA (thay bằng trung bình)
for(i in 1:ncol(water_potability)){
  water_potability[is.na(water_potability[,i]), i] <- mean(water_potability[,i], na.rm = TRUE)
}

# Kiểm tra lại NA
cat("\nSau khi xử lý NA:\n")
print(colSums(is.na(water_potability)))

# Phân phối biến mục tiêu
cat("\nPhân phối biến mục tiêu:\n")
print(table(water_potability$Potability))
print(prop.table(table(water_potability$Potability))*100)


# Bước 3: Chia dữ liệu
set.seed(42)
train_indices <- createDataPartition(water_potability$Potability, p=0.7, list=FALSE)

train_data <- water_potability[train_indices, ]
test_data  <- water_potability[-train_indices, ]

cat("\nKích thước train:", dim(train_data))
cat("\nKích thước test:", dim(test_data))

# 🔥 FIX QUAN TRỌNG: CÂN BẰNG DỮ LIỆU
# ==========================================================

train_balanced <- upSample(
  x = train_data[, -ncol(train_data)],
  y = train_data$Potability
)

colnames(train_balanced)[ncol(train_balanced)] <- "Potability"

print(table(train_balanced$Potability))


# Bước 4: Xây dựng mô hình
model <- glm(Potability ~ .,
             data = train_data,
             family = binomial(link="logit"))


# Bước 5: Kiểm tra mô hình
cat("\nTóm tắt mô hình:\n")
summary_model <- summary(model)
print(summary_model)

# Hệ số
cat("\nHệ số hồi quy:\n")
coef_df <- data.frame(
  feature = names(coef(model)),
  coefficients = coef(model),
  odds_ratio = exp(coef(model)),
  p_value = summary_model$coefficients[,4]
)
print(coef_df)


# Bước 6: Dự đoán
probabilities <- predict(model, newdata=test_data, type="response")

predicted_classed <- ifelse(probabilities > 0.3, "Drinkable", "Not Drinkable")
predicted_classed <- factor(predicted_classed, levels=c("Not Drinkable","Drinkable"))

print(predicted_classed)


# Bước 7: Đánh giá
conf_matrix <- confusionMatrix(predicted_classed, test_data$Potability)
print(conf_matrix$table)

cat("\nCác chỉ số đánh giá:\n")
cat("Accuracy:", round(conf_matrix$overall["Accuracy"],4), "\n")
cat("Sensitivity:", round(conf_matrix$byClass["Sensitivity"],4), "\n")
cat("Specificity:", round(conf_matrix$byClass["Specificity"],4), "\n")
cat("Precision:", round(conf_matrix$byClass["Pos Pred Value"],4), "\n")
cat("F1:", round(conf_matrix$byClass["F1"],4), "\n")


# Bước 8: ROC và AUC
roc_obj <- roc(test_data$Potability, probabilities)
auc_value <- auc(roc_obj)

cat("AUC:", round(auc_value,4), "\n")

plot(roc_obj, main="Đường cong ROC", col="blue", lwd=2)

# vẽ confusion matrix
# In ma trận
table(Predicted = predicted_classed, Actual = test_data$Potability)

# Dùng caret
conf_matrix <- confusionMatrix(predicted_classed, test_data$Potability)
conf_matrix$table

# Vẽ biểu đồ
# Cài nếu chưa có
if(!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

cm <- as.data.frame(table(Predicted = predicted_classed, Actual = test_data$Potability))

ggplot(cm, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "white", size = 6) +
  scale_fill_gradient(low = "skyblue", high = "violet") +
  labs(title = "Confusion Matrix", x = "Actual", y = "Predicted")
