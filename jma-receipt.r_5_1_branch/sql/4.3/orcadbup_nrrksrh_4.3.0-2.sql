\set ON_ERROR_STOP

ALTER TABLE tbl_nrrksrh  drop constraint tbl_nrrksrh_primary_key  ;
ALTER TABLE ONLY tbl_nrrksrh ADD CONSTRAINT tbl_nrrksrh_primary_key PRIMARY KEY (hospnum,ptid,sryymd,zainum,rrknum);
