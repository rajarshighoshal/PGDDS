# ML-lib

# Load SparkR
spark_path <- '/usr/local/spark'
if (nchar(Sys.getenv("SPARK_HOME")) < 1) {
  Sys.setenv(SPARK_HOME = spark_path)
}
library(SparkR, lib.loc = c(file.path(Sys.getenv("SPARK_HOME"), "R", "lib")))

# Initialise the sparkR session
sparkR.session(master = "yarn-client", sparkConfig = list(spark.driver.memory = "1g"))

# Create a Spark DataFrame and examine structure
data_acc <- read.df("/common_folder/accelerometer_data/Phones_accelerometer.csv", source = "csv",
                    header = TRUE, inferSchema = TRUE)
head(data_acc)
nrow(data_acc)

# Before executing any hive-sql query from RStudio, you need to add a jar file in RStudio 
sql("ADD JAR /opt/cloudera/parcels/CDH/lib/hive/lib/hive-hcatalog-core-1.1.0-cdh5.11.2.jar")

# For using SQL, you need to create a temporary view
createOrReplaceTempView(data_acc, "data_acc_tbl")

# check if all the gt values are present in the table
typesofGT <- SparkR::sql("SELECT gt, COUNT(*) FROM data_acc_tbl GROUP BY gt")
head(typesofGT)

# Splitting the data into train and test
train_acc <- sample(data_acc, withReplacement = FALSE, fraction = 0.7)
test_acc <- SparkR::except(data_acc, train_acc)

# Check if the split is correct
nrow(train_acc)
nrow(test_acc)

# Building a model
model_glm_acc <- SparkR::glm(gt ~ x + y, data = train_acc, family = "gaussian")
SparkR::summary(model_glm_acc)

# To see the system time
system.time(SparkR::glm(gt ~ x + y, data = train_acc, family = "gaussian"))

# Predicting the output from model
predictions_rf_acc <- SparkR::predict(model_glm_acc, newData = test_acc)

# Random Forest Model
model_rf_acc <- spark.randomForest(data = train_acc, gt ~ x+y, "classification",
                                   numTrees = 10)
SparkR::summary(model_rf_acc)

## stop 
sparkR.stop()
