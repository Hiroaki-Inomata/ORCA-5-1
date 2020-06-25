\set ON_ERROR_STOP

alter table tbl_syumei add  column hospnum numeric(2,0);
alter table tbl_syumei add  column syurrknumwk numeric(2,0);
alter table tbl_syumei add  column syuedanumwk numeric(2,0);
alter table tbl_syumei drop column hospid;
update tbl_syumei set hospnum = 1,syurrknumwk=syurrknum,syuedanumwk=syuedanum ;
alter table tbl_syumei drop column syurrknum;
alter table tbl_syumei drop column syuedanum;
alter table tbl_syumei add  column syurrknum numeric(2,0);
alter table tbl_syumei add  column syuedanum numeric(2,0);
update tbl_syumei set syurrknum=syurrknumwk,syuedanum=syuedanumwk ;
alter table tbl_syumei drop column syurrknumwk;
alter table tbl_syumei drop column syuedanumwk;

alter table tbl_syumei add constraint tbl_syumei_primary_key primary key (hospnum, nyugaikbn, ptid, denpnum, denpedanum);
