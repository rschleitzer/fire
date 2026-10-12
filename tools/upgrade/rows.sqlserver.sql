-- What the steps of tools/upgrade-test.sh wrote on SQL Server, in a fixed order.
set nocount on;
select 'gender|' + id + '|' + isnull(gender, '') from [Patient] order by id;
select 'family|' + resource_id + '|' + cast(id as varchar(10)) + '|' + value from [Patient].search_family order by resource_id, id;
select 'period|' + resource_id + '|' + cast(id as varchar(10)) + '|' + convert(varchar(40), value_low, 121) + '|' + convert(varchar(40), value_high, 121) from [Practitioner].search_qualification_period order by resource_id, id;
