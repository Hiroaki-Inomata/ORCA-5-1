\set ON_ERROR_STOP
--
-- クライアント印刷制御用テーブル
-- Create Date : 2011/04/10
--

create	table	tbl_prtctrl	(
	HOSPNUM		smallint NOT NULL,
	TBL_KEY		char(8),
	RENNUM		integer DEFAULT 0,
	TBL_GROUP	char(14),
	TBL_UUID	char(36),
	SHORI_RENNUM	integer DEFAULT 0,
	PAGE		integer DEFAULT 0,
	DATA_KBN	smallint NOT NULL,
	CONT_FLG	smallint NOT NULL,
	GYOUMU_CD	integer DEFAULT 0,
	DNAME		varchar(200),
	TERMID		char(64),
	OPID		varchar(16),
	CREYMD		char(8),
	CREHMS		char(6)
);
ALTER TABLE ONLY tbl_prtctrl
	ADD CONSTRAINT tbl_prtctrl_primary_key PRIMARY KEY (HOSPNUM, TBL_KEY, RENNUM, TBL_GROUP, TBL_UUID, SHORI_RENNUM, PAGE)
;

