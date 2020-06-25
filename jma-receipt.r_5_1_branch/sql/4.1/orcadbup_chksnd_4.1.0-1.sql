\set ON_ERROR_STOP

alter table tbl_chksnd add  column utagaikbn  character(1);
update tbl_chksnd set utagaikbn='0' where chkkbn = '1';
update tbl_chksnd set utagaikbn='1' where chkkbn in ('2','6');
