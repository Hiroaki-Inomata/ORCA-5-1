ALTER TABLE ONLY tbl_hktsrycd
  DROP CONSTRAINT tbl_hktsrycd_pkey;
ALTER TABLE ONLY tbl_hktsrycd
  ADD CONSTRAINT tbl_hktsrycd_primary_key PRIMARY KEY (kanricd, rennum, yukostymd, yukoedymd);
