
library(sparklyr)
config <- spark_config()
config$`sparklyr.cores.local` <- "16"
# config$`spark.sql.shuffle.partitions.local` <- "16"
config$`sparklyr.shell.driver-memory` <- '20G'
config$`sparklyr.shell.executor-memory` <- '1G'
config$`spark.driver.maxResultSize` <- "4G"
sc <- spark_connect(master = "local", config = config)

df_tbl <- spark_read_parquet(sc, "df",
  path = "Desktop/Rally/CoorteKI_Rally/KI_Rally_Sprint11A/DB_ORIGINAL/2018_11_06_Dados/",
  infer_schema = TRUE,
  header = TRUE)

sdf_num_partitions(df_tbl)

# make it one partition and write a single csv file to disk
# (this will be read in and transformed in cidacs_cadu_process.R)
df_tbl <- sdf_repartition(df_tbl, 1)
spark_write_csv(df_tbl, path = "Desktop/rally_data")