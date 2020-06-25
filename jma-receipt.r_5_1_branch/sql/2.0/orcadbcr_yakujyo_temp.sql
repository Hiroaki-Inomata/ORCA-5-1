--                                --
-- ORCA DataBase Table Definition --
--                                --
-- ÌôºÞ¾ðÊó»²¾ÈÍÑ£Ä£Â			  --
--                                --
-- Table Name  : TBL_YAKUJYO_TEMP --
-- Create Date : 2005/2/7	  --
-- 				  --

create table TBL_YAKUJYO_TEMP (
	TERMID   		char(64)		not null,
	SRYCD			char(9)			not null,
	YJCD			char(12)		not null,
	RENNUM			numeric(2,0)	default 0,
	SHAPE			varchar(360),
	COLOR			varchar(360),
	MARK			varchar(360),
	EFFECT			varchar(400),
	CAUTION			varchar(850),
	MAKE_COMPANY		varchar(400),
	SALE_COMPANY		varchar(400),
	PHOTO1_FILENAME		varchar(256),
	PHOTO2_FILENAME		varchar(256),
	PHOTO3_FILENAME		varchar(256),
	CREYMD   		char(8),
constraint TBL_YAKUJYO_TEMP_primary_key primary key (TERMID,SRYCD,YJCD,RENNUM));
