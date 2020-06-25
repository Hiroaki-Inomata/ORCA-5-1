--                                    --
-- 点数マスタ附加情報                 --
--                                    --
-- 整数項目のデータ型変更             --
--                                    --
-- Create Date : 2010/10/14           --
--                                    --

\set ON_ERROR_STOP

alter table tbl_tensuplus alter column shoyukoketa type smallint;
alter table tbl_tensuplus alter column saiketukbn type smallint;
alter table tbl_tensuplus alter column inpchkkbn type smallint;
alter table tbl_tensuplus alter column tenpuyakkbn type character varying(2);
alter table tbl_tensuplus alter column allsuryokbn type character varying(1);
alter table tbl_tensuplus alter column kanzantanicd type character varying(3);
alter table tbl_tensuplus alter column santeirrkkbn type smallint;
alter table tbl_tensuplus alter column jgncnt type smallint;
alter table tbl_tensuplus alter column jgncnt1d type smallint;
alter table tbl_tensuplus alter column jgncnterr type smallint;
alter table tbl_tensuplus alter column souryohenkbn type smallint;
alter table tbl_tensuplus alter column ipnkisaikbn type smallint;
alter table tbl_tensuplus alter column ipnkanzantanicd type character varying(3);
alter table tbl_tensuplus alter column hospnum type smallint;
alter table tbl_tensuplus alter column jgncntetcm type smallint;
alter table tbl_tensuplus alter column jgncntetc type smallint;
alter table tbl_tensuplus alter column zenknskbn type smallint;
