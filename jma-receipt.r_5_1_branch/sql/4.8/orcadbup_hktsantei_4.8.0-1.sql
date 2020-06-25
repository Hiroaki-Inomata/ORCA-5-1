ALTER TABLE ONLY tbl_hktsantei
  DROP CONSTRAINT tbl_hktsantei_pkey;
ALTER TABLE ONLY tbl_hktsantei
  ADD CONSTRAINT tbl_hktsantei_primary_key PRIMARY KEY (kanricd, yukostymd, yukoedymd, srycd);
