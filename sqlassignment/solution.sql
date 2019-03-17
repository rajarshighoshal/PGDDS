# enabling 'ONLY_FULL_GROUP_BY' mode
SET GLOBAL sql_mode = 'ONLY_FULL_GROUP_BY';
# checking if mode is 'ONLY_FULL_GROUP_BY'
SELECT @@GLOBAL.sql_mode;

# using superstoredb schema
USE superstoresdb;
DESCRIBE cust_dimen;
DESCRIBE market_fact;
DESCRIBE orders_dimen;
DESCRIBE prod_dimen;
DESCRIBE shipping_dimen;

# Task 1: Understanding the data in hand

# A. Describe the data in hand in your own words. (Word Limit is 500)
#    superstoresDB contains of 5 tables:- 
#    1) cust_dimen which contains information about customers;
#    2) market_fact which contains facts about market like sales, order quantity, profit based on combination 
#       of specific order ids and product ids;
#    3) orders_dimen which contains data on each and every order in details including order priority, date etc.;
#    4) prod_dimen which contains data about product id, category and sub-category;
#    5) shipping_dimen which contains information about order id and shipping id and mode of shipping and date of shipping

# B. Identify and list the Primary Keys and Foreign Keys for this dataset (Hint: If a table
#    don’t have Primary Key or Foreign Key, then specifically mention it in your answer.)
#    1) cust_dimen: primary key- Cust_id
#                   foreign key- NA
#    2) market_fact: primary key- Ord_id
#                    foreign key- Prod_id, Cust_id, Ship_id
#    3) orders_dimen: primary key- Order_ID
#                     foreign key- Ord_id
#    4) prod_dimen: primary key- prod_id
#                   foreign key- NA
#    5) shipping_dimen: primary key- ship_id
#                       foreign key- Order_ID
#     * NA means there is not a single key of that type


# Task 2: Basic Analysis

# A. Find the total and the average sales (display total_sales and avg_sales) 
SELECT 
    SUM(Sales), AVG(Sales)
FROM
    market_fact;
# B. Display the number of customers in each region in decreasing order of
# no_of_customers. The result should contain columns Region, no_of_customers
SELECT 
    Region, COUNT(Cust_id) AS 'no_of_customers'
FROM
    cust_dimen
GROUP BY Region
ORDER BY no_of_customers DESC;
# C. Find the region having maximum customers (display the region name and
# max(no_of_customers)
SELECT 
    Region, COUNT(Cust_id) AS 'max_no_of_customers'
FROM
    cust_dimen
GROUP BY Region
HAVING max_no_of_customers >= ALL (SELECT 
        COUNT(Cust_id)
    FROM
        cust_dimen
    GROUP BY Region);
# D. Find the number and id of products sold in decreasing order of products sold (display
# product id, no_of_products sold)
SELECT 
    Prod_id, SUM(Order_Quantity) AS 'no_of_products_sold'
FROM
    market_fact
GROUP BY Prod_id
ORDER BY no_of_products_sold DESC;
# E. Find all the customers from Atlantic region who have ever purchased ‘TABLES’ and
# the number of tables purchased (display the customer name, no_of_tables purchased)
SELECT 
    Customer_name,
    SUM(Order_Quantity) AS 'no_of_tables',
    Product_Sub_Category
FROM
    cust_dimen
        INNER JOIN
    market_fact ON cust_dimen.Cust_id = market_fact.Cust_id
        INNER JOIN
    prod_dimen ON market_fact.Prod_id = prod_dimen.Prod_id
WHERE
    Product_Sub_Category = 'TABLES'
        AND Region = 'Atlantic'
GROUP BY cust_dimen.Cust_id, Customer_Name;
  
# Task 3: Advanced Analysis

# A. Display the product categories in descending order of profits (display the product
# category wise profits i.e. product_category, profits)?
SELECT 
    Product_Category, SUM(Profit)
FROM
    market_fact
        INNER JOIN
    prod_dimen ON market_fact.Prod_id = prod_dimen.Prod_id
GROUP BY Product_category
ORDER BY SUM(Profit) DESC;
# B. Display the product category, product sub-category and the profit within each subcategory
# in three columns. 
SELECT 
    Product_Category, Product_Sub_Category, SUM(Profit)
FROM
    market_fact
        INNER JOIN
    prod_dimen ON market_fact.Prod_id = prod_dimen.Prod_id
GROUP BY Product_Sub_Category, Product_Category
ORDER BY SUM(Profit) DESC;
# C. Where is the least profitable product subcategory shipped the most? For the least
# profitable product sub-category, display the region-wise no_of_shipments and the
# profit made in each region in decreasing order of profits (i.e. region,
# no_of_shipments, profit_in_each_region)

# region where least profitable sub category shipped the most
SELECT 
    Region, COUNT(shipping_dimen.Ship_id)
FROM
    cust_dimen
        INNER JOIN
    market_fact ON cust_dimen.Cust_id = market_fact.Cust_id
        INNER JOIN
    shipping_dimen ON shipping_dimen.Ship_id = market_fact.Ship_id
        INNER JOIN
    prod_dimen ON market_fact.Prod_id = prod_dimen.Prod_id
WHERE
    Product_Sub_Category = 'TABLES'
GROUP BY Region
HAVING COUNT(Shipping_dimen.Ship_id) >= ALL (SELECT 
        COUNT(Shipping_dimen.Ship_id)
    FROM
        cust_dimen
            INNER JOIN
        market_fact ON cust_dimen.Cust_id = market_fact.Cust_id
            INNER JOIN
        shipping_dimen ON shipping_dimen.Ship_id = market_fact.Ship_id
            INNER JOIN
        prod_dimen ON market_fact.Prod_id = prod_dimen.Prod_id
    WHERE
        Product_Sub_Category = 'TABLES'
    GROUP BY Region);
  
  # region wise no of shipments and profits made in each region for least profitable sub category
  SELECT 
    Region,
    COUNT(shipping_dimen.Ship_id) AS 'no_of_shipments',
    SUM(Profit) AS 'profit_in_each_region'
FROM
    cust_dimen
        INNER JOIN
    market_fact ON cust_dimen.Cust_id = market_fact.Cust_id
        INNER JOIN
    shipping_dimen ON shipping_dimen.Ship_id = market_fact.Ship_id
        INNER JOIN
    prod_dimen ON market_fact.Prod_id = prod_dimen.Prod_id
WHERE
    Product_Sub_Category = 'TABLES'
GROUP BY Region
ORDER BY profit_in_each_region DESC; 
  
  