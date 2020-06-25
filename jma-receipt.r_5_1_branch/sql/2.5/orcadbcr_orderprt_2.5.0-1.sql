--                                --
-- ORCA DataBase Table Definition --
--                                --
-- ¥ª¡¼¥ÀÄ¢É¼               --
--                                --
-- Table Name  : TBL_ORDERPRT     --
-- Create Date : 2005/2/14        --
--                                --

CREATE TABLE TBL_ORDERPRT (
HOSPID		char(24)		not null,
PTID		numeric(10)		default 0,
PTNUM		char(20)		not null,
NYUGAIKBN	char(1)			not null,
SRYKA		char(2)			not null,
HKNCOMBI	char(4)			not null,
SYSYMD		char(8)			not null,
PRINTYMD	char(8)			not null,
PRINTHMS	char(6)			not null,
PRTKBN		char(1)			not null,
ORDERNUM	numeric(2)		default 0,
RENNUM		numeric(2)		default 0,
DRCD		char(5),
SRYKBN		char(2),
SRYSYUKBN	char(3),
SRYYMD		char(8),
DAY1		numeric(3)		default 0,
DAY2		numeric(3)		default 0,
DAY3		numeric(3)		default 0,
DAY4		numeric(3)		default 0,
DAY5		numeric(3)		default 0,
DAY6		numeric(3)		default 0,
DAY7		numeric(3)		default 0,
DAY8		numeric(3)		default 0,
DAY9		numeric(3)		default 0,
DAY10		numeric(3)		default 0,
DAY11		numeric(3)		default 0,
DAY12		numeric(3)		default 0,
DAY13		numeric(3)		default 0,
DAY14		numeric(3)		default 0,
DAY15		numeric(3)		default 0,
DAY16		numeric(3)		default 0,
DAY17		numeric(3)		default 0,
DAY18		numeric(3)		default 0,
DAY19		numeric(3)		default 0,
DAY20		numeric(3)		default 0,
DAY21		numeric(3)		default 0,
DAY22		numeric(3)		default 0,
DAY23		numeric(3)		default 0,
DAY24		numeric(3)		default 0,
DAY25		numeric(3)		default 0,
DAY26		numeric(3)		default 0,
DAY27		numeric(3)		default 0,
DAY28		numeric(3)		default 0,
DAY29		numeric(3)		default 0,
DAY30		numeric(3)		default 0,
DAY31		numeric(3)		default 0,
SRYCD		char(9),
SRYSURYO	numeric(8,3)	default 0,
SRYSURYO2	numeric(8,3)	default 0,
SRYKAISU	numeric(3)		default 0,
INPUTCOMENT	varchar(80),
TERMID		varchar(64),
OPID		varchar(16),
CREYMD		char(8),
UPYMD		char(8),
UPHMS		char(6),
Constraint TBL_ORDERPRT_primary_key Primary Key (
HOSPID, PTID, PTNUM, NYUGAIKBN, SRYKA, HKNCOMBI, SYSYMD,
PRINTYMD, PRINTHMS, PRTKBN, ORDERNUM, RENNUM )
) ;

