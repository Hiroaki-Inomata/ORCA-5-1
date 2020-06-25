\set ON_ERROR_STOP

ALTER TABLE  master.tbl_recekisai DROP CONSTRAINT tbl_recekisai_primary_key ;

ALTER TABLE ONLY master.tbl_recekisai
    ADD CONSTRAINT tbl_recekisai_primary_key PRIMARY KEY (srycd, itemno, edano, stym, edym, rennum);

