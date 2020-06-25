-- tbl_henrei_kanri --

\set ON_ERROR_STOP

ALTER TABLE tbl_henrei_kanri DROP CONSTRAINT tbl_henrei_kanri_primary_key;
alter table tbl_henrei_kanri add column hospcd character varying(7);
update tbl_henrei_kanri set hospcd='';
alter table tbl_henrei_kanri alter column hospcd set not null;
ALTER TABLE ONLY tbl_henrei_kanri
ADD CONSTRAINT tbl_henrei_kanri_primary_key PRIMARY KEY (hospnum, teisyutusaki,teisyutusaki2,hospcd,syoriym);

