-- Tredence test

/*
1. In FY18, out of all customers who opted for pick up (PUT + S2S), how many (count and %age) 
   have never placed a pick-up order of over $35?

	Some filters to help:

	·         where visit_date between X and Y

	·         and channel = 'DOTCOM'

	·         and service_id in (8, 11)
*/
	
	
-- Using the below given convoluted method of getting to the answer (which uses group by to get unique values) because it will work faster than a 'count(distinct ugc_id)',
-- as the ugc_id is not aprtitioned on. But for a small dataset, count(distinct ugc_id) will be best
select Count_35, (100.0*Count_35/Total_count) as perc_count
from
(
	select
	Total_count =  	(select count(*) from   (select ugc_id		-- Getting unique customer IDs that satisfy all the conditions
											from Order_Table
											where (visit_date between cast(to_date(from_unixtime(unix_timestamp('01-02-2017', 'dd-MM-yyyy'))) as date)
													and cast(to_date(from_unixtime(unix_timestamp('31-01-2018', 'dd-MM-yyyy'))) as date)) 
											and channel = 'DOTCOM' and service_id in (8, 11) group by 1) as A),

	Count_35 =  	(select count(*) from   (select ugc_id		-- Getting unique customer IDs that satisfy the amount condition as well
											from Order_Table
											where (visit_date between cast(to_date(from_unixtime(unix_timestamp('01-02-2017', 'dd-MM-yyyy'))) as date)
													and cast(to_date(from_unixtime(unix_timestamp('31-01-2018', 'dd-MM-yyyy'))) as date)) 
											and channel = 'DOTCOM' and service_id in (8, 11) and amount>35 group by 1) as B)
) as C


/*	
2. Cumulative revenue for “DOTCOM” and “OG” until end of each month of FY18 i.e. total revenue 
   until end of Feb’17, until end of March’17… until end of Jan’18
*/

select
A.*,
sum(mth_revenue) over (partition by channel order by dt_mth rows between unbounded preceding an and current row) as channel_cumulative_revenue
from
(
	select channel, TRUNC(visit_date,'MM') as dt_mth, sum(amount) as mth_revenue
	from Order_Table
	where channel in ('DOTCOM', 'OG')
	and visit_date between cast(to_date(from_unixtime(unix_timestamp('01-02-2017', 'dd-MM-yyyy'))) as date)
	and cast(to_date(from_unixtime(unix_timestamp('31-01-2018', 'dd-MM-yyyy'))) as date)
	group by 1,2
	
) as A

/*
3. For each quarter of a fiscal year - what percentage of shoppers (dotcom only) shopping in a fiscal quarter, 
   will shop again (repeat) in the following quarter? You’d have to look at Q1 for the next FY to get repeat rate for Q4
*/

select
sum(Y1Q2)/sum(Y1Q1) as q1_perc,
sum(Y1Q3)/sum(Y1Q2) as q1_perc,
sum(Y1Q4)/sum(Y1Q2) as q1_perc,
sum(Y2Q1)/sum(Y1Q2) as q1_perc
from
(
	-- This step can also be done using a pivot as well (Check commented query at the end of the document)
	select
	ugc_id,
	max(case when fy_qtr_strt_dt = cast(to_date(from_unixtime(unix_timestamp('01-02-2017', 'dd-MM-yyyy'))) as date) then 1 else 0 end) as Y1Q1,
	max(case when fy_qtr_strt_dt = cast(to_date(from_unixtime(unix_timestamp('01-02-2017', 'dd-MM-yyyy'))) as date) then 1 else 0 end) as Y1Q2,
	max(case when fy_qtr_strt_dt = cast(to_date(from_unixtime(unix_timestamp('01-02-2017', 'dd-MM-yyyy'))) as date) then 1 else 0 end) as Y1Q3,
	max(case when fy_qtr_strt_dt = cast(to_date(from_unixtime(unix_timestamp('01-02-2017', 'dd-MM-yyyy'))) as date) then 1 else 0 end) as Y1Q4,
	max(case when fy_qtr_strt_dt = cast(to_date(from_unixtime(unix_timestamp('01-02-2017', 'dd-MM-yyyy'))) as date) then 1 else 0 end) as Y2Q1
	from
	(
		-- The output of this inner query is a indicator for each quarter when a customer has ordered
		-- This dataset will not contain rows for a quarter when a customer hasn't made an order
		-- Hence we use a pivot to mimic the presence of the data row
		-- Level of data is Customer ID, Quarter
		select 
			ugc_id, 
			add_months(add_months(add_months(TRUNC(visit_date,'MM'),-1),-(month(dt)-1)%3),1) as fy_qtr_strt_dt, 
			1 as order_flag
		
		from Order_Table
		where channel = 'DOTCOM'
		and visit_date between cast(to_date(from_unixtime(unix_timestamp('01-02-2017', 'dd-MM-yyyy'))) as date)
		and cast(to_date(from_unixtime(unix_timestamp('30-04-2018', 'dd-MM-yyyy'))) as date)
		group by 1,2,3
	) qtr_order_data
	group by 1
) qtr_perc_repeaters




/*

-- Using PIVOT

select * from 
(	
	-- The output of this inner query is a indicator for each quarter when a customer has ordered
	-- This dataset will not contain rows for a quarter when a customer hasn't made an order
	-- Hence we use a pivot to mimic the presence of the data row
	-- Level of data is Customer ID, Quarter
	select 
		ugc_id, 
		add_months(add_months(add_months(TRUNC(visit_date,'MM'),-1),-(month(dt)-1)%3),1) as fy_qtr_strt_dt, 
		1 as order_flag
	
	from Order_Table
	where channel = 'DOTCOM'
	and visit_date between cast(to_date(from_unixtime(unix_timestamp('01-02-2017', 'dd-MM-yyyy'))) as date)
	and cast(to_date(from_unixtime(unix_timestamp('30-04-2018', 'dd-MM-yyyy'))) as date)
	group by 1,2,3
	
) qtr_order_data	
PIVOT

(MAX(coalesce(order_flag,0)) FOR fy_qtr_strt_dt IN 
													(cast(to_date(from_unixtime(unix_timestamp('01-02-2017', 'dd-MM-yyyy'))) as date),
													 cast(to_date(from_unixtime(unix_timestamp('01-05-2017', 'dd-MM-yyyy'))) as date),
													 cast(to_date(from_unixtime(unix_timestamp('01-08-2017', 'dd-MM-yyyy'))) as date),
													 cast(to_date(from_unixtime(unix_timestamp('01-11-2017', 'dd-MM-yyyy'))) as date),
													 cast(to_date(from_unixtime(unix_timestamp('01-02-2018', 'dd-MM-yyyy'))) as date))

) dt
*/