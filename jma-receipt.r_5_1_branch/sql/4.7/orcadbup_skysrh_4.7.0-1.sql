--                               
-- 照会請求情報テーブル
--                               
-- SKYKBN_RECEDEN追加            
--                               
-- Create Date : 2012/2/8        

\set ON_ERROR_STOP

drop table tbl_skysrh;

create	table	tbl_skysrh	(
	HOSPNUM	smallint,
	TERMID	char(64),
	PTID	bigint,
	NYUGAIKBN	char(1),
	SRYYM	char(6),
	HKNID	bigint,
	HKNNUM	char(3),
	KOHNUM1	char(3),
	KOHNUM2	char(3),
	TOTALTEN	bigint,
	TOKKI1	varchar(2),
	TOKKI2	varchar(2),
	TOKKI3	varchar(2),
	TOKKI4	varchar(2),
	TOKKI5	varchar(2),
	TOKKINM1	varchar(12),
	TOKKINM2	varchar(12),
	TOKKINM3	varchar(12),
	TOKKINM4	varchar(12),
	TOKKINM5	varchar(12),
	ETC_YKZFTN1	integer ,
	SKYKBN_RECEDEN	char(1)
) with oids ;

