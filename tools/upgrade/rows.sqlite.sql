-- What the steps of tools/upgrade-test.sh wrote on SQLite, in a fixed order.
select 'gender', id, gender from Patient order by id;
select 'family', resource_id, id, value from Patient_search_family order by resource_id, id;
select 'period', resource_id, id, value_low, value_high from Practitioner_search_qualification_period order by resource_id, id;
