create	table	tbl_csv_info	(
	HOSPNUM	numeric(2,0),
	TBL_KEY	char(8),
	SRYYM	varchar(6),
	SHELLID	varchar(20),
	RENNUM	numeric(3,0),
	SHORI_RENNUM	numeric(3,0),
	SEQ_NO	numeric(6,0),
	NYUGAIKBN	char(1),
	SKYYMD	varchar(8),
	TEISYUTUSAKI	char(1),
	PTID	numeric(10,0),
	CSVDATA	varchar(2000),
	CREYMD	varchar(8),
	CREHMS	varchar(6),
	primary	key(
		HOSPNUM,
		TBL_KEY,
		SRYYM,
		SHELLID,
		RENNUM,
		SHORI_RENNUM,
		SEQ_NO
	)
);
