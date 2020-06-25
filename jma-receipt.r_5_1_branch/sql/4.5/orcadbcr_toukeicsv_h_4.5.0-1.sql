\set ON_ERROR_STOP
--
-- Åý·×£Ã£Ó£Ö´ÉÍý¥Æ¡¼¥Ö¥ë
--
-- Create Date : 2009/09/27         --

create	table	tbl_toukeicsv_h	(
	HOSPNUM		smallint	DEFAULT 0 NOT NULL,
	TBL_KEY		char(8)		NOT NULL,
	RENNUM		smallint	DEFAULT 0 NOT NULL,
	TBL_GROUP	char(14)	NOT NULL,
	SHORI_RENNUM	smallint	DEFAULT 0 NOT NULL,
	SHELLID		varchar(20)	NOT NULL,
	SRYYM		varchar(6)	NOT NULL,
	SRYYMD		varchar(8)	NOT NULL,
	CNT		integer		DEFAULT 0,
	NYUGAIKBN	char(1)		NOT NULL,
	TO_FOLDER	varchar(200)	NOT NULL,
	TO_DATA		varchar(200)	NOT NULL,
	CODE_TYPE	char(1)		NOT NULL,
	PUTFLG		char(1)		NOT NULL,
	TITLE		varchar(100)	NOT NULL,
	CREYMD		char(8),
	UPYMD           char(8),
	UPHMS           char(6),
	Constraint "tbl_toukeicsv_h_primary_key" primary key(
		HOSPNUM,
		TBL_KEY,
		RENNUM,
		TBL_GROUP,
		SHORI_RENNUM
	)
);
