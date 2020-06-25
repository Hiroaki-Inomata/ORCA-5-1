create	table	tbl_nsrysrh	(
	HOSPNUM	numeric(2,0),
	PTID	numeric(10,0),
	ZAINUM	numeric(9,0),
	SRYYM	char(6),
	SRYKA	char(2),
	HKNCOMBINUM	char(4),
	SRYKBN	char(2),
	SRYCD	char(9),
	BRMNUM	char(8),
	RM_SAGAKU char(2),
	TERMID	varchar(16),
	OPID	varchar(16),
	CREYMD	char(8),
	UPYMD	char(8),
	UPHMS	char(6)
);

ALTER TABLE ONLY tbl_nsrysrh
    ADD CONSTRAINT tbl_nsrysrh_primary_key PRIMARY KEY (hospnum,ptid,zainum,sryym);

COMMENT ON TABLE tbl_nsrysrh IS 'Æþ±¡¿ÇÎÅ¹Ô°Ù¾È²ñ';
