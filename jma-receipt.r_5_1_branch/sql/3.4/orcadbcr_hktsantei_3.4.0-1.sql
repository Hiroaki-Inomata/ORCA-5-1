create	table	tbl_hktsantei	(
	KANRICD	numeric(3,0),
	YUKOSTYMD	char(8),
	YUKOEDYMD	char(8),
	SRYCD	char(9),
	SANTEIKBN	char(1),
	KENTAIKBN	char(1),
	TERMID	varchar(16),
	OPID	varchar(16),
	CREYMD	char(8),
	UPYMD	char(8),
	UPHMS	char(6),
	primary	key(
		KANRICD,
		YUKOSTYMD,
		YUKOEDYMD,
		SRYCD
	)
);
