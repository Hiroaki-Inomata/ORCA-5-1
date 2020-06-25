\set ON_ERROR_STOP

--				--
-- ユーザー管理テーブルの作成	--
--				--
-- Create Date : 2007/06/26	--
--				--

-- TBL_SYSUSER 作成		--

CREATE TABLE tbl_sysuser (
	userid		character varying(16) NOT NULL,
	hospnum		numeric(2,0) NOT NULL,
	supervisor	character(1),
	upymd		character(8)
);

ALTER TABLE ONLY tbl_sysuser
    ADD CONSTRAINT tbl_sysuser_primary_key PRIMARY KEY (userid);
