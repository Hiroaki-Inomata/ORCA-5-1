-- tbl_onrece_body --

\set ON_ERROR_STOP

ALTER TABLE tbl_onrece_body DROP CONSTRAINT tbl_onrece_body_primary_key;
alter table tbl_onrece_body add column hospcd character varying(7);
update tbl_onrece_body set hospcd='';
alter table tbl_onrece_body alter column hospcd set not null;
ALTER TABLE ONLY tbl_onrece_body
ADD CONSTRAINT tbl_onrece_body_primary_key PRIMARY KEY (HOSPNUM,TEISYUTUSAKI,HOSPCD,SRYYM,UCNT,SCNT,RENNUM,RECEIPT_NO);

