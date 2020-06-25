--                                --
-- ORCA DataBase Table Definition --
--                                --
-- £Ð£Ç´ÉÍý¾ðÊó                   --
--                                --
-- Table Name  : TBL_PGKANRI      --
-- Create Date : 2004/01/28       --
--                                --

create table TBL_PGKANRI (
DEBIANVERSION			varchar(24)	not null,
SYSTEMVERSION			varchar(10)	not null,
PGNO				char(3)		not null,
RELEASEYMD			char(8),
PGNAME				varchar(64),
UPDATEFLG			char(1),
RESTARTFLG			char(1),
INSTALLDIR			varchar(256),
COMMENT				varchar(400),
TERMID  			varchar(32),
OPID    			varchar(16),
CREYMD  			char(8),
UPYMD    			char(8),
UPHMS				char(6),
constraint TBL_PGKANRI_primary_key primary key (DEBIANVERSION,SYSTEMVERSION,PGNO));
