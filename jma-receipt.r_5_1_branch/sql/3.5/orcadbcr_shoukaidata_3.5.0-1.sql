\set ON_ERROR_STOP
--
-- 患者照会検索条件データテープル
--
-- Create Date : 2007/08/08         --
--
create	table	tbl_shoukaidata	(
	HOSPNUM		numeric(2,0) NOT NULL,
	UID				char(36) NOT NULL,
	ITEMID		numeric(5,0),
	ITEM 		  varchar(100),
	TERMID		varchar(32),
	OPID			varchar(16),
	CREYMD		char(8),
	UPYMD			char(8),
	UPHMS			char(6),
	Constraint "tbl_shoukaidata_primary_key" primary	key(
		HOSPNUM,
		UID,
		ITEMID
	)
);
