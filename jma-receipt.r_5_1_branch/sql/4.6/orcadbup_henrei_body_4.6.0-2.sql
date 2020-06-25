-- tbl_henrei_body --

\set ON_ERROR_STOP

ALTER TABLE tbl_henrei_body DROP CONSTRAINT tbl_henrei_body_primary_key;
alter table tbl_henrei_body add column teisyutusaki2  char(1);
update tbl_henrei_body set teisyutusaki2 = '0';
alter table tbl_henrei_body alter column teisyutusaki2 set not null;
ALTER TABLE ONLY tbl_henrei_body
ADD CONSTRAINT tbl_henrei_body_primary_key PRIMARY KEY (hospnum, teisyutusaki, teisyutusaki2, syoriym, receipt_no, pt_rennum);
