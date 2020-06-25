\set ON_ERROR_STOP
--
-- 算定履歴付加テーブル
--
-- Create Date : 2009/11/16         --
--
create	table	tbl_santeiplus	(
	HOSPNUM		smallint NOT NULL,
	PTID		bigint DEFAULT 0 NOT NULL,
	SRYYM		char(6) NOT NULL,
	SRYCD		char(9) NOT NULL,
	NYUGAIKBN	char(1) NOT NULL,
	SRYKA 		char(2) NOT NULL,
	HKNCOMBINUM	smallint NOT NULL,
	DAYKEY	        smallint NOT NULL,
	RENNUM	        smallint NOT NULL,
	COMMENT  	varchar(80),
	TERMID		varchar(16),
	OPID    	varchar(16),
	CREYMD  	char(8),
	UPYMD    	char(8),
	UPHMS		char(6),
	Constraint "tbl_santeiplus_key" primary	key(
		HOSPNUM,
		PTID,
		SRYYM,
		SRYCD,
		NYUGAIKBN,
		SRYKA,
		HKNCOMBINUM,
		DAYKEY,
		RENNUM
	)
);
