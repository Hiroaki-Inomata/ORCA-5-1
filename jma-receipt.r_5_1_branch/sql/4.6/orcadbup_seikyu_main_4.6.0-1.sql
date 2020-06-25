-- tbl_seikyu_main --

\set ON_ERROR_STOP

alter table tbl_seikyu_main add column skyiryo_nissu smallint default 0;
alter table tbl_seikyu_main add column skyiryo_totalten integer  default 0;
alter table tbl_seikyu_main add column skyiryo_ichibuftn integer  default 0;
alter table tbl_seikyu_main add column skyiryo_hokenja  integer  default 0;
alter table tbl_seikyu_main add column skyiryo_kohftn   integer  default 0;
alter table tbl_seikyu_main add column skyskj_kaisu     smallint default 0;
alter table tbl_seikyu_main add column skyskj_ryoyohi   integer  default 0;
alter table tbl_seikyu_main add column skyskj_ichibuftn integer  default 0;
alter table tbl_seikyu_main add column skyskj_hokenja   integer  default 0;
alter table tbl_seikyu_main add column skyskj_kohftn    integer  default 0;
alter table tbl_seikyu_main add column hkncombi1 smallint default 0;
alter table tbl_seikyu_main add column hkncombi2 smallint default 0;
alter table tbl_seikyu_main add column hkncombi3 smallint default 0;
alter table tbl_seikyu_main add column hkncombi4 smallint default 0;
alter table tbl_seikyu_main add column hkncombi5 smallint default 0;
alter table tbl_seikyu_main add column hkncombi6 smallint default 0;
alter table tbl_seikyu_main add column hkncombi7 smallint default 0;
alter table tbl_seikyu_main add column hkncombi8 smallint default 0;
alter table tbl_seikyu_main add column hkncombi9 smallint default 0;
alter table tbl_seikyu_main add column hkncombi10 smallint default 0;
update tbl_seikyu_main set skyiryo_nissu=0,skyiryo_totalten=0,skyiryo_ichibuftn=0,skyiryo_hokenja=0,skyiryo_kohftn=0,skyskj_kaisu=0,skyskj_ryoyohi=0,skyskj_ichibuftn=0,skyskj_hokenja=0,skyskj_kohftn=0,hkncombi1=0,hkncombi2=0,hkncombi3=0,hkncombi4=0,hkncombi5=0,hkncombi6=0,hkncombi7=0,hkncombi8=0,hkncombi9=0,hkncombi10=0;

