--                                    --
--  ヘルプテーブルの変更              --
--                                    --
-- 項目の追加                         --
--     目次２フラグ   MENU2FLG        --
--                                    --
-- Update Date : 2005/03/17           --
--                                    --

-- TBL_HELPWK 作成                    --

create table TBL_HELPWK (
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
constraint TBL_HELPWK_primary_key primary key (HELPKBN,KEY_OPID,LDNAME,MENUNUM1,MENUNUM2,EDANUM));

-- TBL_HELP → TBL_HELPWK      --

insert into tbl_helpwk
select * from tbl_help;

-- TBL_HELP 削除               --

drop table tbl_help;

-- TBL_HELP 作成               --

create table TBL_HELP (
	HELPKBN				char(1) 	not null,
	KEY_OPID			varchar(16)	not null,
	LDNAME				varchar(20) 	not null,
	MENUNUM1			numeric(4,0)	not null,
	MENUNUM2			numeric(4,0) 	not null,
	EDANUM				numeric(4,0)	not null,
	MENU2FLG			numeric(1,0),
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

-- TBL_HELPWK → TBL_HELP   --

insert into tbl_help
(helpkbn,
key_opid,
ldname,
menunum1,
menunum2,
edanum,
menu2flg,
menu,
help,
termid,
opid,
creymd,
upymd,
uphms
)
select helpkbn,
key_opid,
ldname,
menunum1,
menunum2,
edanum,
1,
menu,
help,
termid,
opid,
creymd,
upymd,
uphms
 from tbl_helpwk;

-- TBL_HELPWK 削除        --

drop table tbl_helpwk;
