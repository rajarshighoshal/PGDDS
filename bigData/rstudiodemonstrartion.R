# Load SparkR
spark_path <- '/usr/local/spark'
if (nchar(Sys.getenv("SPARK_HOME")) < 1) {
  Sys.setenv(SPARK_HOME = spark_path)
}
library(SparkR, lib.loc = c(file.path(Sys.getenv("SPARK_HOME"), "R", "lib")))

# Initialise the sparkR session
sparkR.session(master = "yarn-client", sparkConfig = list(spark.driver.memory = "1g"))

df <- read.df("hdfs:///common_folder/online_retail/", source = "csv", 
                            inferSchema = "true", header = "true")

# Before executing any hive-sql query from RStudio, you need to add a jar file in RStudio 
sql("ADD JAR /opt/cloudera/parcels/CDH/lib/hive/lib/hive-hcatalog-core-1.1.0-cdh5.11.2.jar")

createOrReplaceTempView(df, "df_view")
results <- SparkR::sql("select count(*) from df_view")
head(results)