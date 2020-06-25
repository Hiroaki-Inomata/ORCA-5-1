--
-- tbl_chkreigai
--
-- add column tanicd,taniname
--
-- Create Date : 2013/12/19

\set ON_ERROR_STOP

alter table tbl_chkreigai add  COLUMN tanicd smallint;
alter table tbl_chkreigai add  COLUMN taniname varchar(12);
update tbl_chkreigai set tanicd = 0;
update tbl_chkreigai set taniname = '';
