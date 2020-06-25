ALTER TABLE ONLY tbl_passwd
  DROP CONSTRAINT tbl_passwd_key;
ALTER TABLE ONLY tbl_passwd
  ADD CONSTRAINT tbl_passwd_primary_key PRIMARY KEY (USERID);
