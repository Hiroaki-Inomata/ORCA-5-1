create	table	tbl_nrrksrh	(
	HOSPNUM	numeric(2,0),
	PTID	numeric(10,0),
	SRYYMD	char(8),
	ZAINUM	numeric(9,0),
	TERMID	varchar(16),
	OPID	varchar(16),
	CREYMD	char(8),
	UPYMD	char(8),
	UPHMS	char(6)
);

ALTER TABLE ONLY tbl_nrrksrh
    ADD CONSTRAINT tbl_nrrksrh_primary_key PRIMARY KEY (hospnum,ptid,sryymd,zainum);

COMMENT ON TABLE tbl_nrrksrh IS 'Æþ±¡ÍúÎò¾È²ñ';
