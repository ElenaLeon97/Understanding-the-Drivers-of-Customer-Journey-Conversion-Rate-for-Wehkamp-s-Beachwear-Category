/* Joining session on articleevents with sanity check */
DROP TABLE IF EXISTS RESULTS1;
CREATE TEMP TABLE RESULTS1 AS
SELECT S.session_id,
S.session_date,
AE.article_event_date_time,
11AE.article_event_type,
AE.article_id,
AE.customer_id
FROM articleevents AE
LEFT JOIN public.session S ON S.session_id=AE.session_id;
/* Sanity check: both 81573 */
SELECT COUNT(DISTINCT(session_id))
FROM articleevents;
SELECT COUNT(DISTINCT(session_id))
FROM RESULTS1;
/* Adding gender from customer table */
DROP TABLE IF EXISTS RESULTS2;
CREATE TEMP TABLE RESULTS2 AS
SELECT R1.*,
CASE WHEN C.sex='F' THEN 1
WHEN C.sex='M' THEN 0 ELSE NULL END AS sex
FROM RESULTS1 R1
LEFT JOIN customer C ON C.customer_id=R1.customer_id;
/* Sanity check: still both 81573, customer count consistent */
SELECT COUNT(DISTINCT(session_id)), COUNT(customer_id)
FROM RESULTS1;
SELECT COUNT(DISTINCT(session_id)), COUNT(customer_id)
FROM RESULTS2;
/* Adding needed data from article table to results table */
DROP TABLE IF EXISTS RESULTS3;
CREATE TEMP TABLE RESULTS3 AS
SELECT R2.*,
A.current_price
FROM RESULTS2 R2
LEFT JOIN article A ON A.article_id=R2.article_id;
/* Getting the amount of sales per article in 2022 and counting distinct article ID's as sanity
check*/
DROP TABLE IF EXISTS sales_amount_article;
CREATE TEMP TABLE sales_amount_article AS
SELECT SUM(items) as article_popularity,
article_id
FROM public.order
12WHERE order_date >= '2022-01-01'
GROUP BY article_id;
/* Sanity check */
SELECT COUNT(DISTINCT(article_id))
FROM sales_amount_article;
SELECT COUNT(DISTINCT(article_id))
FROM RESULTS3;
/* Not all articles in the article event table have actually been ordered in 2022, therefore we
should be careful only left joining the available data onto our results table so we don't end up
having missing session data. We can later replace NULL with 0 */
DROP TABLE IF EXISTS RESULTS4;
CREATE TEMP TABLE RESULTS4 AS
SELECT R3.*,
article_popularity
FROM RESULTS3 R3
LEFT JOIN sales_amount_article SAA ON SAA.article_id=R3.article_id;
/* Adding seasonal and dates dummy's */
DROP TABLE IF EXISTS RESULTS5;
CREATE TEMP TABLE RESULTS5 AS
SELECT R4.*,
CASE WHEN session_date BETWEEN '2022-01-01' AND '2022-02-28' THEN 'WINTER'
WHEN session_date BETWEEN '2022-03-01' AND '2022-05-31' THEN 'SPRING'
WHEN session_date BETWEEN '2022-06-01' AND '2022-08-31' THEN 'SUMMER'
WHEN session_date BETWEEN '2022-09-01' AND '2022-11-30' THEN 'AUTUMN'
WHEN session_date BETWEEN '2022-12-01' AND '2022-12-31' THEN 'WINTER' END
AS season,
CASE WHEN EXTRACT(DOW FROM session_date) IN (0, 6) THEN 1 ELSE 0 END AS
weekend
FROM RESULTS4 R4;
/* Aggregating the results table to session level and adding final variables based on the session
level */
DROP TABLE IF EXISTS RESULTS6;
CREATE TEMP TABLE RESULTS6 AS
SELECT session_id,
MIN(session_date) AS session_date,
MIN(customer_id) AS customer_id,
MIN(season) AS season,
MAX(CASE WHEN article_event_type='40' THEN 1 ELSE 0 END) AS conversion,
13CASE WHEN MAX(EXTRACT(HOUR FROM article_event_date_time)) >= 12 THEN 0
ELSE 1 END AS morning,
AVG(current_price) AS avg_product_price,
AVG(article_popularity) AS avg_product_popularity,
MIN(sex) AS sex,
(MAX(article_event_date_time) - MIN(article_event_date_time)) AS session_time,
MIN(weekend) AS weekend
FROM RESULTS5 R5
GROUP BY session_id;
/* Lastly adding Past purchase incidence from conversion dummy's */
DROP TABLE IF EXISTS FINAL_RESULTS;
CREATE TEMP TABLE FINAL_RESULTS AS
SELECT R6.*,
COUNT(CASE WHEN conversion = 1 THEN 1 END) OVER (PARTITION BY customer_id
ORDER BY session_date ROWS BETWEEN 30 PRECEDING AND 1 PRECEDING) as
past_purchase
FROM RESULTS6 R6;
SELECT *
FROM FINAL_RESULTS;
/* Last disaster check: Still 81573 */
SELECT COUNT(*)
FROM FINAL_RESULTS 
