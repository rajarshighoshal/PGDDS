## initialising spark
# load SparkR
spark_path <- '/usr/local/spark'

if (nchar(Sys.getenv("SPARK_HOME")) < 1) {
  Sys.setenv(SPARK_HOME = spark_path)
}

library(SparkR, lib.loc = c(file.path(Sys.getenv("SPARK_HOME"), "R", "lib")))

sparkR.session(master = "yarn", sparkConfig = list(spark.driver.memory = "1g"))



## loading data
data_electronics <- read.df("hdfs:///common_folder/amazon_reviews/original_dataset.json", 
                            source = "json", inferSchema = "true", header = "true")

## examine data
head(data_electronics)
nrow(data_electronics)
ncol(data_electronics)
str(data_electronics)
printSchema(data_electronics)

## summarisation of data
df <- collect(describe(data_electronics))
df


## creating histogram
hist <- histogram(data_electronics, data_electronics$overall, nbins = 12)
## plot the histogram
library(ggplot2)
plot <- ggplot(hist, aes(x = centroids, y = counts)) + geom_bar(stat = 'identity') +
  xlab('overall rating') + ylab('frequency')
plot

## select certain rows
head(select(data_electronics, data_electronics$summary))

## apply a filter
head(filter(data_electronics, data_electronics$overall <= 4))
nrow(filter(data_electronics, data_electronics$overall <= 4))

## groupBy
head(summarize(groupBy(data_electronics, data_electronics$overall), 
               count = n(data_electronics$overall)))
## group by + sort
overall_counts <- summarize(groupBy(data_electronics, data_electronics$overall), 
                            count = n(data_electronics$overall))
head(arrange(overall_counts, desc(overall_counts$count)))

## SQL ##
createOrReplaceTempView(data_electronics, 'data_elec_tbl')

# Before executing any hive-sql query from RStudio, you need to add a jar file in RStudio 
sql("ADD JAR /opt/cloudera/parcels/CDH/lib/hive/lib/hive-hcatalog-core-1.1.0-cdh5.11.2.jar")

data_rated_5 <- SparkR::sql('select * from data_elec_tbl where overall = 5')
head(data_rated_5)
nrow(data_rated_5)


## binning
data_withLength <- SparkR::sql("SELECT helpful,overall,reviewText,summary,asin,
                               LENGTH(reviewText) AS reviewLength FROM data_elec_tbl")

createOrReplaceTempView(data_withLength, 'data_elec_tbl')

# Binning into different lengths of reviewtext
bins <- sql("SELECT reviewLength, \
            CASE  WHEN reviewLength <= 1000  THEN 1\
            WHEN (reviewLength > 1000  and reviewLength <= 2000) THEN 2\
            WHEN (reviewLength > 2000 and reviewLength <= 3000) THEN 3\
            WHEN (reviewLength > 3000 and reviewLength <= 4000) THEN 4\
            ELSE 5 END  as bin_number FROM data_elec_tbl")

# To check the above performed operations
head(select(bins, c(bins$reviewLength, bins$bin_number)))