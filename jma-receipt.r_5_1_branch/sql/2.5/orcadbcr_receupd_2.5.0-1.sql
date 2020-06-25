create	table	tbl_receupd	(
	HOSPID	char(24),
	PTID	numeric(10,0),
	NYUGAIKBN	char(1),
	SRYKA	char(2) NOT NULL,
	SRYYM	char(6) NOT NULL,
	HKNKBN	char(1) NOT NULL,
	HKNKBN2	char(1) NOT NULL,
	RECEYMD	char(8),
	ONLUPD	char(1),
	ONLUPD1	char(1),
	ONLUPD2	char(1),
	ONLUPD3	char(1),
	ONLUPD4	char(1),
	ONLUPD5	char(1),
	ONLUPD6	char(1),
	ONLUPD7	char(1),
	ONLUPD8	char(1),
	ONLUPD9	char(1),
	ONLUPD10	char(1),
	TERMID	varchar(64),
	OPID	varchar(16),
	CREYMD	char(8),
	UPYMD	char(8),
	UPHMS	char(6),
	primary	key(
		HOSPID,
		PTID,
		NYUGAIKBN,
		SRYKA,
		SRYYM,
		HKNKBN,
		HKNKBN2
	)
);
CREATE INDEX idx_receupd_upymd ON tbl_receupd USING btree (hospid, upymd, nyugaikbn);
