\set ON_ERROR_STOP

alter table tbl_inputset add column inputkbn char(1);
alter table tbl_inputset add column kansuryo numeric(10,5) DEFAULT 0;
update tbl_inputset set inputkbn = ' ';
