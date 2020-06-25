--                                    --
-- 点数マスタ投薬付加テーブルの作成   --
--                                    --
--                                    --
-- Create Date : 2006/12/11           --
--                                    --

-- TBL_TENSUPLUS2  作成               --

create	table	tbl_tensuplus2	(
	SRYCD		char(9) not null,
	CHOKINISSU 	numeric(3,0),
	CHOKIKIGEN	char(8),
	CHOKIERR  	numeric(1,0),
	TOUYOKGNAGE1	char(2),
	TOUYOJGNAGE1	char(2),
	TOUYORYO11	numeric(10,5),
	TOUYORYO21	numeric(10,5),
	TOUYOKGNAGE2	char(2),
	TOUYOJGNAGE2	char(2),
	TOUYORYO12	numeric(10,5),
	TOUYORYO22	numeric(10,5),
	TOUYOKGNAGE3	char(2),
	TOUYOJGNAGE3	char(2),
	TOUYORYO13	numeric(10,5),
	TOUYORYO23	numeric(10,5),
	TOUYOKGNAGE4	char(2),
	TOUYOJGNAGE4	char(2),
	TOUYORYO14	numeric(10,5),
	TOUYORYO24	numeric(10,5),
	TOUYOKGNAGE5	char(2),
	TOUYOJGNAGE5	char(2),
	TOUYORYO15	numeric(10,5),
	TOUYORYO25	numeric(10,5),
	TOUYOKGNAGE6	char(2),
	TOUYOJGNAGE6	char(2),
	TOUYORYO16	numeric(10,5),
	TOUYORYO26	numeric(10,5),
	TOUYOKGNAGE7	char(2),
	TOUYOJGNAGE7	char(2),
	TOUYORYO17	numeric(10,5),
	TOUYORYO27	numeric(10,5),
	TOUYOKGNAGE8	char(2),
	TOUYOJGNAGE8	char(2),
	TOUYORYO18	numeric(10,5),
	TOUYORYO28	numeric(10,5),
	TOUYOKGNAGE9	char(2),
	TOUYOJGNAGE9	char(2),
	TOUYORYO19	numeric(10,5),
	TOUYORYO29	numeric(10,5),
	TOUYOKGNAGE10	char(2),
	TOUYOJGNAGE10	char(2),
	TOUYORYO110	numeric(10,5),
	TOUYORYO210	numeric(10,5),
	TOUYOERR 	numeric(1,0),
	NAIFUKUKBN 	numeric(1,0),
	TERMID	        varchar(16),
	OPID	        varchar(16),
	CREYMD	        char(8),
	UPYMD	        char(8),
	UPHMS	        char(6),
	constraint tbl_tensuplus2_primary_key primary key(
		SRYCD
	)
);
