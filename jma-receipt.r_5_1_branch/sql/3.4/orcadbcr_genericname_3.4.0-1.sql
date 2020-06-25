--                                    --
-- 一般名マスタテーブルの作成         --
--                                    --
--                                    --
-- Create Date : 2006/12/20           --
--                                    --

-- TBL_GENERICNAME 作成               --

create	table	tbl_genericname	(
	YAKKAKJNCD    	char(9) not null,
	YUKOKETA	numeric(2,0),
	GENERICNAME     varchar(200),
	TANICD     	char(3),
	TANINAME        varchar(24),
	KANZANCHI 	numeric(10,5),
	TERMID	        varchar(16),
	OPID	        varchar(16),
	CREYMD	        char(8),
	UPYMD	        char(8),
	UPHMS	        char(6),
	constraint tbl_genericname_primary_key primary key(
		YAKKAKJNCD
	)
);
