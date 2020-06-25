\set ON_ERROR_STOP
--
-- 患者メモテーブル
--
-- Create Date : 2007/08/08         --
--
create	table	tbl_ptmemoinf	(
	HOSPNUM		numeric(2,0) NOT NULL,
	PTID		numeric(10,0)  NOT NULL,
	SYSYMD     	char(8),
	MEMOKBN		numeric(1,0),
	RENNUM		numeric(2,0),
	SRYKA		char(2),
	MEMO	  	varchar(1000),
	TERMID	varchar(16),
	OPID	varchar(16),
	CREYMD	char(8),
	UPYMD	char(8),
	UPHMS	char(6),
	Constraint "tbl_ptmemoinf_key" primary	key(
		HOSPNUM,
		PTID,
		SYSYMD,
		MEMOKBN,
		RENNUM,
		SRYKA
	)
);
