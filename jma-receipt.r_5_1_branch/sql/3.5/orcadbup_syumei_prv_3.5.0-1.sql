\set ON_ERROR_STOP

alter table tbl_syumei_prv add  column hospnum numeric(2,0);
alter table tbl_syumei_prv add  column syurrknumwk numeric(2,0);
alter table tbl_syumei_prv add  column syuedanumwk numeric(2,0);
alter table tbl_syumei_prv drop column hospid;
update tbl_syumei_prv set hospnum = 1,syurrknumwk=syurrknum,syuedanumwk=syuedanum ;
alter table tbl_syumei_prv drop column syurrknum;
alter table tbl_syumei_prv drop column syuedanum;
alter table tbl_syumei_prv add  column syurrknum numeric(2,0);
alter table tbl_syumei_prv add  column syuedanum numeric(2,0);
update tbl_syumei_prv set syurrknum=syurrknumwk,syuedanum=syuedanumwk ;
alter table tbl_syumei_prv drop column syurrknumwk;
alter table tbl_syumei_prv drop column syuedanumwk;

alter table tbl_syumei_prv add constraint tbl_syumei_prv_primary_key primary key (hospnum, nyugaikbn, ptid, denpnum, denpedanum);
