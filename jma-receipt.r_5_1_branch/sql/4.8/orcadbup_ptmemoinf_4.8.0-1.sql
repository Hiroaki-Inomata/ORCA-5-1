ALTER TABLE ONLY tbl_ptmemoinf
  DROP CONSTRAINT tbl_ptmemoinf_key;
ALTER TABLE ONLY tbl_ptmemoinf
  ADD CONSTRAINT tbl_ptmemoinf_primary_key PRIMARY KEY (hospnum, ptid, sysymd, memokbn, rennum, sryka);
