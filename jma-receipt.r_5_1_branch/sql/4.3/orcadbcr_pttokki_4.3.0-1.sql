\set ON_ERROR_STOP
--
-- 患者特記事項テーブル
--
-- Create Date : 2008/06/24         --
--
create	table	tbl_pttokki	(
	HOSPNUM		numeric(2,0) NOT NULL,
	NYUGAIKBN	char(1)  NOT NULL,
	PTID		numeric(10,0)  NOT NULL,
	RENNUM  	numeric(2,0)  NOT NULL,
	CD  		char(2),
	NAIYO   	char(12),
	STYM	   	char(6),
	EDYM	   	char(6),
	TERMID		varchar(16),
	OPID		varchar(16),
	CREYMD		char(8),
	UPYMD		char(8),
	UPHMS		char(6),
	Constraint "tbl_pttokki_key" primary	key(
		HOSPNUM,
		NYUGAIKBN,
		PTID,
		RENNUM
	)
);
