--                                    --
-- パスワードテーブルの作成           --
--                                    --
-- Create Date : 2014/01/12           --
--                                    --

CREATE TABLE tbl_passwd (
  USERID character varying(16) NOT NULL,
  PASSWORD character varying(256)
);

ALTER TABLE ONLY tbl_passwd
  ADD CONSTRAINT tbl_passwd_primary_key PRIMARY KEY (USERID);
