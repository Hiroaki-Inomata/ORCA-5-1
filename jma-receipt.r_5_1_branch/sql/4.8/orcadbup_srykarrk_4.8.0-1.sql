--                                      
--  tbl_srykarrk                      
--                                    
-- 
--   alter column type
--    hospnum : numeric -> smallint
--    ptid    : numeric -> bigint
-- Create Date : 2014/2/20       

\set ON_ERROR_STOP

alter table tbl_srykarrk alter column hospnum type smallint;
alter table tbl_srykarrk alter column ptid    type bigint;
