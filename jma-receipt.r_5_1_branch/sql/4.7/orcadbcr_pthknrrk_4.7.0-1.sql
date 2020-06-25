\set ON_ERROR_STOP
--
-- 患者保険確認年月日履歴テーブル
-- Create Date : 2011/08/05
--

create	table	tbl_pthknrrk	(
	HOSPNUM		smallint NOT NULL,
	PTID		bigint,
	HKNID		bigint,
	RENNUM		integer DEFAULT 0,
	KAKUNINYMD	char(8),
	TERMID		varchar(16),
	OPID		varchar(16),
	CREYMD		char(8),
	UPYMD		char(8),
	UPHMS		char(6)
);
ALTER TABLE ONLY tbl_pthknrrk
	ADD CONSTRAINT tbl_pthknrrk_primary_key PRIMARY KEY (HOSPNUM, PTID,HKNID, RENNUM)
;

