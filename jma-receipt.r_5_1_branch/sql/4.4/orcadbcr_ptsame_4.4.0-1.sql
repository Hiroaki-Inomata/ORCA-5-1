\set ON_ERROR_STOP
--
-- 同一患者識別テーブル
--
-- Create Date : 2009/01/23         --
--
create	table	tbl_ptsame	(
	HOSPNUM	smallint NOT NULL,
	PTID		bigint  NOT NULL,
	SAME_HOSPNUM	smallint  NOT NULL,
	SAME_PTID	bigint  NOT NULL,
	TERMID	varchar(16),
	OPID		varchar(16),
	CREYMD	char(8),
	UPYMD		char(8),
	UPHMS		char(6),
	Constraint "tbl_ptsame_key" primary	key(
		HOSPNUM,
		PTID
	)
);
