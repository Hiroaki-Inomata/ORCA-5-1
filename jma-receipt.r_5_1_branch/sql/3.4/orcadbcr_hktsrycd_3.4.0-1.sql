create	table	tbl_hktsrycd	(
	KANRICD	numeric(3,0),
	RENNUM	numeric(2,0),
	YUKOSTYMD	char(8),
	YUKOEDYMD	char(8),
	SRYCD	char(9),
	TERMID	varchar(16),
	OPID	varchar(16),
	CREYMD	char(8),
	UPYMD	char(8),
	UPHMS	char(6),
	primary	key(
		KANRICD,
		RENNUM,
		YUKOSTYMD,
		YUKOEDYMD
	)
);
