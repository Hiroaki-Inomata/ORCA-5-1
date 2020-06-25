\set ON_ERROR_STOP

alter table tbl_wksryact add column inputkbn_1 char(1);
alter table tbl_wksryact add column inputkbn_2 char(1);
alter table tbl_wksryact add column inputkbn_3 char(1);
alter table tbl_wksryact add column inputkbn_4 char(1);
alter table tbl_wksryact add column inputkbn_5 char(1);
alter table tbl_wksryact add column kansuryo_1 numeric(10,5) DEFAULT 0;
alter table tbl_wksryact add column kansuryo_2 numeric(10,5) DEFAULT 0;
alter table tbl_wksryact add column kansuryo_3 numeric(10,5) DEFAULT 0;
alter table tbl_wksryact add column kansuryo_4 numeric(10,5) DEFAULT 0;
alter table tbl_wksryact add column kansuryo_5 numeric(10,5) DEFAULT 0;
update tbl_wksryact set inputkbn_1 = ' ';
update tbl_wksryact set inputkbn_2 = ' ';
update tbl_wksryact set inputkbn_3 = ' ';
update tbl_wksryact set inputkbn_4 = ' ';
update tbl_wksryact set inputkbn_5 = ' ';
