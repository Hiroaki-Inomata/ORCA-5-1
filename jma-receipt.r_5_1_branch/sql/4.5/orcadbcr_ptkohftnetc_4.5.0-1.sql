\set ON_ERROR_STOP
--
-- ���Ը�����ô¾���ǡ����ơ��֥�
--
-- Create Date : 2009/07/06         --
--
create	table	tbl_ptkohftnetc	(
	HOSPNUM		smallint,
	PTID		bigint,
	KOHID		bigint,
	SRYYMD	        char(8),
	RENNUM		integer,
	KOHNUM	        char(3),
	NYUGAIKBN	char(1),
	FTNMONEY	integer,
	TERMID		varchar(16),
	OPID		varchar(16),
	CREYMD		char(8),
	UPYMD		char(8),
	UPHMS		char(6),
	Constraint "tbl_ptkohftnetc_primary_key" primary key(
		HOSPNUM,
		PTID,
		KOHID,
		SRYYMD,
		RENNUM
	)
);
