--                                    --
-- Ç¯¶âÍúÎò¥Æ¡¼¥Ö¥ë¤ÎÊÑ¹¹             --
--                                    --
-- ¹àÌÜ¤ÎÄÉ²Ã                         --
--   ¡¡Ï·ÎðÊ¡»ãÇ¯¶â¼õµë¼Ô¾Ú¶èÊ¬       --
--            RRI_FUKUSHI_KBN         --
--                                    --
-- Create Date : 2006/09/06           --
--                                    --

\set ON_ERROR_STOP

-- TBL_TNKRRKWK ºîÀ®                 --

create table TBL_TNKRRKWK (
HOSPID				char(24)	not null,
PTID				numeric(10,0)	not null,
STYMD				char(8)		not null,
EDYMD				char(8)		not null,
TERMID  			varchar(16),
OPID    			varchar(16),
CREYMD  			char(8),
UPYMD    			char(8),
UPHMS				char(6),
constraint TBL_TNKRRKWK_primary_key primary key (HOSPID,PTID,STYMD));

-- TBL_TNKRRK ¢ª TBL_TNKRRKWK   --

insert into TBL_TNKRRKWK
select * from TBL_TNKRRK;

-- TBL_TNKRRK ºï½ü               --

drop table TBL_TNKRRK;

-- TBL_TNKRRK ºîÀ®               --

create table TBL_TNKRRK (
HOSPID				char(24)	not null,
PTID				numeric(10,0)	not null,
STYMD				char(8)		not null,
EDYMD				char(8)		not null,
RRI_FUKUSHI_KBN			char(1)		not null,
TERMID  			varchar(16),
OPID    			varchar(16),
CREYMD  			char(8),
UPYMD    			char(8),
UPHMS				char(6),
constraint TBL_TNKRRK_primary_key primary key (HOSPID,PTID,STYMD));

-- TBL_TNKRRKWK ¢ª TBL_TNKRRK   --

insert into TBL_TNKRRK
(HOSPID,
PTID,
STYMD,
EDYMD,
RRI_FUKUSHI_KBN,
TERMID,
OPID,
CREYMD,
UPYMD,
UPHMS
)
select
HOSPID,
PTID,
STYMD,
EDYMD,
' ',
TERMID,
OPID,
CREYMD,
UPYMD,
UPHMS

from TBL_TNKRRKWK;

-- Ï·ÎðÊ¡»ãÇ¯¶â¤ÎÈ½Äê            --
UPDATE TBL_TNKRRK SET RRI_FUKUSHI_KBN = '1' WHERE (SELECT B.BIRTHDAY FROM TBL_PTINF B WHERE B.PTID = TBL_TNKRRK.PTID) <= '19110401';
UPDATE TBL_TNKRRK SET RRI_FUKUSHI_KBN = '0' WHERE (SELECT B.BIRTHDAY FROM TBL_PTINF B WHERE B.PTID = TBL_TNKRRK.PTID) > '19110401';
-- TBL_TNKRRKWK ºï½ü             --

drop table TBL_TNKRRKWK;
