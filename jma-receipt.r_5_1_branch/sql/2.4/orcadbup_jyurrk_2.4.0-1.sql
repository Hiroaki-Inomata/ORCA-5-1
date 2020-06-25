--                                    --
-- 受診履歴テーブルの変更             --
--                                    --
-- 項目の削除                         --
--   最終区分 LASTKBN                 --
-- 項目の追加                         --
--   まとめ入力伝票番号 GRP_DENPNUM   --
--   まとめ入力連番     GRP_RENNUM    --
--   収納更新区分       SYU_UPDKBN    --
--                                    --
-- Create Date : 2004/07/04           --
--                                    --

-- TBL_JYURRKWK 作成                  --

create table TBL_JYURRKWK (
HOSPID				char(24)	not null,
PTID				numeric(10)	not null,
NYUGAIKBN                       char(01)        not null,
SRYKA				char(02)        not null,
SRYYMD   			char(08)        not null,
RENNUM  			numeric(01)     not null,
DOUJI_RENNUM                    numeric(01)     not null,
KAIKEI_RENNUM                   numeric(03)     not null,
EDANUM                          numeric(01)     not null,
LASTKBN    			char(01),
DRCD                            char(05),
HKNCOMBINUM                     char(04),
HKNKBN                          char(01),
DENPNUM                         numeric(07),
SRYKBN1                         char(02),
SRYKBN2                         char(02),
SRYKBN3                         char(02),
SRYKBN4                         char(02),
SRYKBN5                         char(02),
SRYKBN6                         char(02),
SRYKBN7                         char(02),
SRYKBN8                         char(02),
SRYKBN9                         char(02),
SRYKBN10                        char(02),
SRYKBN11                        char(02),
ZAINUM1                         numeric(08),
ZAINUM2                         numeric(08),
ZAINUM3                         numeric(08),
ZAINUM4                         numeric(08),
ZAINUM5                         numeric(08),
ZAINUM6                         numeric(08),
ZAINUM7                         numeric(08),
ZAINUM8                         numeric(08),
ZAINUM9                         numeric(08),
ZAINUM10                        numeric(08),
ZAINUM11                        numeric(08),
ZAINUM12                        numeric(08),
ZAINUM13                        numeric(08),
ZAINUM14                        numeric(08),
ZAINUM15                        numeric(08),
TERMID  			varchar(16),
OPID    			varchar(16),
CREYMD  			char(8),
UPYMD    			char(8),
UPHMS				char(6),
constraint TBL_JYURRKWK_primary_key primary key (HOSPID,PTID,NYUGAIKBN,SRYKA,
SRYYMD,RENNUM,DOUJI_RENNUM,KAIKEI_RENNUM,EDANUM));


-- TBL_JYURRK → TBL_JYURRKWK   --

insert into TBL_JYURRKWK
select * from TBL_JYURRK;


-- TBL_JYURRKWK2 作成                  --

create table TBL_JYURRKWK2 (
HOSPID				char(24)	not null,
PTID				numeric(10)	not null,
NYUGAIKBN                       char(01)        not null,
SRYKA				char(02)        not null,
SRYYMD   			char(08)        not null,
RENNUM  			numeric(01)     not null,
DOUJI_RENNUM                    numeric(01)     not null,
KAIKEI_RENNUM                   numeric(03)     not null,
EDANUM                          numeric(01)     not null,
LASTKBN    			char(01),
DRCD                            char(05),
HKNCOMBINUM                     char(04),
HKNKBN                          char(01),
DENPNUM                         numeric(07),
SRYKBN1                         char(02),
SRYKBN2                         char(02),
SRYKBN3                         char(02),
SRYKBN4                         char(02),
SRYKBN5                         char(02),
SRYKBN6                         char(02),
SRYKBN7                         char(02),
SRYKBN8                         char(02),
SRYKBN9                         char(02),
SRYKBN10                        char(02),
SRYKBN11                        char(02),
ZAINUM1                         numeric(08),
ZAINUM2                         numeric(08),
ZAINUM3                         numeric(08),
ZAINUM4                         numeric(08),
ZAINUM5                         numeric(08),
ZAINUM6                         numeric(08),
ZAINUM7                         numeric(08),
ZAINUM8                         numeric(08),
ZAINUM9                         numeric(08),
ZAINUM10                        numeric(08),
ZAINUM11                        numeric(08),
ZAINUM12                        numeric(08),
ZAINUM13                        numeric(08),
ZAINUM14                        numeric(08),
ZAINUM15                        numeric(08),
TERMID  			varchar(16),
OPID    			varchar(16),
CREYMD  			char(8),
UPYMD    			char(8),
UPHMS				char(6),
constraint TBL_JYURRKWK2_primary_key primary key (HOSPID,PTID,NYUGAIKBN,SRYKA,
SRYYMD,RENNUM,DOUJI_RENNUM,KAIKEI_RENNUM,EDANUM));

-- TBL_JYURRK から最終区分'0'を削除 --

delete from TBL_JYURRK WHERE LASTKBN = '0';

-- TBL_JYURRK → TBL_JYURRKWK2   --

insert into TBL_JYURRKWK2
select * from TBL_JYURRK;

-- TBL_JYURRK 削除               --

drop table TBL_JYURRK;

-- TBL_JYURRK 作成               --

create table TBL_JYURRK (
HOSPID				char(24)	not null,
PTID				numeric(10)	not null,
NYUGAIKBN                       char(01)        not null,
SRYKA				char(02)        not null,
SRYYMD   			char(08)        not null,
RENNUM  			numeric(01)     not null,
DOUJI_RENNUM                    numeric(01)     not null,
KAIKEI_RENNUM                   numeric(03)     not null,
EDANUM                          numeric(01)     not null,
DRCD                            char(05),
HKNCOMBINUM                     char(04),
HKNKBN                          char(01),
DENPNUM                         numeric(07),
GRP_DENPNUM                     numeric(07)  default 0,
GRP_RENNUM                      numeric(02)  default 0,
SYU_UPDKBN                      char(01)  default ' ',
SRYKBN1                         char(02),
SRYKBN2                         char(02),
SRYKBN3                         char(02),
SRYKBN4                         char(02),
SRYKBN5                         char(02),
SRYKBN6                         char(02),
SRYKBN7                         char(02),
SRYKBN8                         char(02),
SRYKBN9                         char(02),
SRYKBN10                        char(02),
SRYKBN11                        char(02),
ZAINUM1                         numeric(08),
ZAINUM2                         numeric(08),
ZAINUM3                         numeric(08),
ZAINUM4                         numeric(08),
ZAINUM5                         numeric(08),
ZAINUM6                         numeric(08),
ZAINUM7                         numeric(08),
ZAINUM8                         numeric(08),
ZAINUM9                         numeric(08),
ZAINUM10                        numeric(08),
ZAINUM11                        numeric(08),
ZAINUM12                        numeric(08),
ZAINUM13                        numeric(08),
ZAINUM14                        numeric(08),
ZAINUM15                        numeric(08),
TERMID  			varchar(16),
OPID    			varchar(16),
CREYMD  			char(8),
UPYMD    			char(8),
UPHMS				char(6),
constraint TBL_JYURRK_primary_key primary key (HOSPID,PTID,NYUGAIKBN,
SRYKA,SRYYMD,RENNUM,DOUJI_RENNUM,KAIKEI_RENNUM,EDANUM));


-- TBL_JYURRKWK2 → TBL_JYURRK   --

insert into TBL_JYURRK
(HOSPID,
PTID,
NYUGAIKBN,
SRYKA,
SRYYMD,
RENNUM,
DOUJI_RENNUM,
KAIKEI_RENNUM,
EDANUM,
DRCD,
HKNCOMBINUM,
HKNKBN,
DENPNUM,
--GRP_DENPNUM--
--GRP_RENNUM--
--SYU_UPDKBN--
SRYKBN1,
SRYKBN2,
SRYKBN3,
SRYKBN4,
SRYKBN5,
SRYKBN6,
SRYKBN7,
SRYKBN8,
SRYKBN9,
SRYKBN10,
SRYKBN11,
ZAINUM1,
ZAINUM2,
ZAINUM3,
ZAINUM4,
ZAINUM5,
ZAINUM6,
ZAINUM7,
ZAINUM8,
ZAINUM9,
ZAINUM10,
ZAINUM11,
ZAINUM12,
ZAINUM13,
ZAINUM14,
ZAINUM15,
TERMID,
OPID,
CREYMD,
UPYMD,
UPHMS
)
select HOSPID,PTID,NYUGAIKBN,SRYKA,SRYYMD,RENNUM,DOUJI_RENNUM,KAIKEI_RENNUM,
EDANUM,DRCD,HKNCOMBINUM,HKNKBN,DENPNUM,
SRYKBN1,SRYKBN2,SRYKBN3,SRYKBN4,SRYKBN5,SRYKBN6,SRYKBN7,SRYKBN8,
SRYKBN9,SRYKBN10,SRYKBN11,ZAINUM1,ZAINUM2,ZAINUM3,ZAINUM4,ZAINUM5,
ZAINUM6,ZAINUM7,ZAINUM8,ZAINUM9,ZAINUM10,ZAINUM11,ZAINUM12,ZAINUM13,
ZAINUM14,ZAINUM15,TERMID,OPID,CREYMD,UPYMD,UPHMS  from TBL_JYURRKWK2;

-- TBL_JYURRKWK 削除             --

drop table TBL_JYURRKWK;

-- TBL_JYURRKWK2 削除             --

drop table TBL_JYURRKWK2;
