-- tbl_onrece_kanri --

\set ON_ERROR_STOP

ALTER TABLE tbl_onrece_kanri DROP CONSTRAINT tbl_onrece_kanri_primary_key;
alter table tbl_onrece_kanri add column hospcd character varying(7);
update tbl_onrece_kanri set hospcd='';
alter table tbl_onrece_kanri alter column hospcd set not null;
ALTER TABLE ONLY tbl_onrece_kanri
ADD CONSTRAINT tbl_onrece_kanri_primary_key PRIMARY KEY (HOSPNUM,TEISYUTUSAKI,HOSPCD,SRYYM,UCNT,SCNT,RENNUM);

