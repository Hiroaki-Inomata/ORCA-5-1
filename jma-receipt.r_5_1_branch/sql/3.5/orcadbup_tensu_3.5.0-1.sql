\set ON_ERROR_STOP

alter table tbl_tensu add column hospnum numeric(2,0);
update tbl_tensu set hospnum = 1;
alter table tbl_tensu alter column hospnum set not null;
alter table tbl_tensu drop constraint tbl_tensu_primary_key;
alter table tbl_tensu add constraint tbl_tensu_primary_key primary key (hospnum, srycd, yukostymd, yukoedymd);
drop index idx_tensu_kananame;
create index idx_tensu_kananame on tbl_tensu(kananame); 
drop index idx_tensu_kensagrp;
create index idx_tensu_kensagrp on tbl_tensu(hospnum, srykbn, knsjisgrpkbn); 
drop index idx_tensu_name;
create index idx_tensu_name on tbl_tensu(name); 
drop index idx_tensu_formalname;
create index idx_tensu_formalname on tbl_tensu(formalname); 
