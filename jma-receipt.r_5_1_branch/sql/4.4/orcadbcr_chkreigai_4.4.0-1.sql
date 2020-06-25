\set ON_ERROR_STOP
--
-- チェック例外テーブル
--
-- Create Date : 2009/03/05         --
--

create	table	tbl_chkreigai	(
	HOSPNUM	smallint,
	PTID	bigint,
	RENNUM	integer,
	CHKNUM	smallint,
	ERRKBN	char(4),
	NYUGAIKBN	char(1),
	SRYCD	char(9),
	STYUKYMD	char(8),
	EDYUKYMD	char(8),
	TERMID	varchar(16),
	OPID	varchar(16),
	CREYMD	char(8),
	UPYMD	char(8),
	UPHMS	char(6)
);
ALTER TABLE ONLY tbl_chkreigai ADD CONSTRAINT tbl_chkreigai_primary_key PRIMARY KEY (hospnum,ptid,rennum);
