-- What the steps of tools/upgrade-test.sh wrote, in a fixed order.
select 'gender', id, "gender" from "Patient" order by id;
select 'family', resource_id, id, value from "Patient".search_family order by resource_id, id;
select 'period', resource_id, id, value_low, value_high from "Practitioner".search_qualification_period order by resource_id, id;
select 'key', count(*) from pg_constraint where conrelid = '"Binary".resource_data'::regclass and contype = 'f';
