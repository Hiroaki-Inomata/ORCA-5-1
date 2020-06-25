\set ON_ERROR_STOP
--
-- Åý·×£Ã£Ó£ÖÌÀºÙ¥Æ¡¼¥Ö¥ë
--
-- Create Date : 2009/09/27         --

create	table	tbl_toukeicsv_b	(
	HOSPNUM		smallint	 DEFAULT 0 NOT NULL,
	TBL_KEY		char(8)		 NOT NULL,
	RENNUM		smallint	 DEFAULT 0 NOT NULL,
	TBL_GROUP	char(14)	 NOT NULL,
	SHORI_RENNUM	smallint	 DEFAULT 0 NOT NULL,
	SHELLID		varchar(20)	 NOT NULL,
	SRYYM		varchar(6)	 NOT NULL,
	SRYYMD		varchar(8)	 NOT NULL,
	PTID		bigint		 DEFAULT 0,
	SEQ_NO		integer		 DEFAULT 0,
	CSVDATA		varchar(20000)	 NOT NULL,
	CREYMD		char(8),
	UPYMD		char(8),
	UPHMS		char(6),
	Constraint "tbl_toukeicsv_b_primary_key" primary key(
		HOSPNUM,
		TBL_KEY,
		RENNUM,
		TBL_GROUP,
		SHORI_RENNUM,
		SEQ_NO
	)
);
