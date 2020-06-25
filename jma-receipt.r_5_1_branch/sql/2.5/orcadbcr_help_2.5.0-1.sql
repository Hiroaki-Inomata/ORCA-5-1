--                                --
-- ORCA DataBase Table Definition --
--                                --
-- ¥Ø¥ë¥×			  --
--                                --
-- Table Name  : TBL_HELP	  --
-- Create Date : 2005/1/19	  --
-- 				  --

create table TBL_HELP (
	HELPKBN				char(1) 	not null,
	KEY_OPID			varchar(16)	not null,
	LDNAME				varchar(20) 	not null,
	MENUNUM1			numeric(4,0)	not null,
	MENUNUM2			numeric(4,0) 	not null,
	EDANUM				numeric(4,0)	not null,
	MENU				varchar(60),
	HELP				varchar(100),
	TERMID   			varchar(64),
	OPID     			varchar(16),
	CREYMD   			char(8),
	UPYMD    			char(8),
	UPHMS				char(6),
constraint TBL_HELP_primary_key primary key (HELPKBN,KEY_OPID,LDNAME,MENUNUM1,MENUNUM2,EDANUM));

create index idx_help_key_opid on tbl_help ( key_opid );

create index idx_help_menu on tbl_help ( menu );
