--                                    --
-- �ݸ��ֹ�ơ��֥���ѹ�             --
--                                    --
-- ���ܤ��ɲ�                         --
--   ������ô�ۤ��ݸ��μ�����ô��³� --
--   �������ʤ����Υ쥻�ץȵ��ܤ�   --
--   �谷��ʬ  : RECEJGNDSPKBN        --
--                                    --
--   ������ô�����ۡ�����             --
--             : NYUSKJJYOGAK         --
--                                    --
-- Create Date : 2004/02/06           --
--                                    --

-- TBL_HKNNUMWK ���� --

create table TBL_HKNNUMWK (
hospid			character(24) NOT NULL,
hknnum			character(3) NOT NULL,
tekstymd		character(8) NOT NULL,
paykbn			character(2) NOT NULL,
tekedymd		character(8),
hbtnum			character(2),
seidoname		character varying(100),
tanseidoname		character varying(20),
hknkohsbtkbn		character(1),
hbtnumchkkbn		character(1),
kensnumchkkbn		character(1),
jkyskensnumchkkbn	character(1),
hknkoukbn		character(1),
genhknnum1		character(3),
genhknnum2		character(3),
genhknnum3		character(3),
joken_styear		numeric(3,0) DEFAULT 0,
joken_edyear		numeric(3,0) DEFAULT 0,
tentnk			numeric(3,0) DEFAULT 0,
receskykbn		character(1),
receftndspkbn		character(1),
hon_gaiftnkbn		character(1),
hon_gaikftnrate		numeric(3,0) DEFAULT 0,
hon_gaikkotgak		numeric(6,0) DEFAULT 0,
hon_gaikjgngak		numeric(6,0) DEFAULT 0,
hon_gaidjgngak		numeric(6,0) DEFAULT 0,
hon_gaidjgncnt		numeric(2,0) DEFAULT 0,
hon_gaimnaijgngak	numeric(6,0) DEFAULT 0,
hon_gaimgaijgngak	numeric(6,0) DEFAULT 0,
hon_gaimjgncnt		numeric(2,0) DEFAULT 0,
hon_gaiykzftnkbn	character(1),
hon_nyuftnkbn		character(1),
hon_nyukftnrate		numeric(3,0) DEFAULT 0,
hon_nyukkotgak		numeric(6,0) DEFAULT 0,
hon_nyukjgngak		numeric(6,0) DEFAULT 0,
hon_nyudjgngak		numeric(6,0) DEFAULT 0,
hon_nyudjgncnt		numeric(2,0) DEFAULT 0,
hon_nyumjgngak		numeric(6,0) DEFAULT 0,
hon_nyumjgncnt		numeric(2,0) DEFAULT 0,
hon_nyuskjryykbn	character(1),
kzk_gaiftnkbn		character(1),
kzk_gaikftnrate		numeric(3,0) DEFAULT 0,
kzk_gaikkotgak		numeric(6,0) DEFAULT 0,
kzk_gaikjgngak		numeric(6,0) DEFAULT 0,
kzk_gaidjgngak		numeric(6,0) DEFAULT 0,
kzk_gaidjgncnt		numeric(2,0) DEFAULT 0,
kzk_gaimnaijgngak	numeric(6,0) DEFAULT 0,
kzk_gaimgaijgngak	numeric(6,0) DEFAULT 0,
kzk_gaimjgncnt		numeric(2,0) DEFAULT 0,
kzk_gaiykzftnkbn	character(1),
kzk_nyuftnkbn		character(1),
kzk_nyukftnrate		numeric(3,0) DEFAULT 0,
kzk_nyukkotgak		numeric(6,0) DEFAULT 0,
kzk_nyukjgngak		numeric(6,0) DEFAULT 0,
kzk_nyudjgngak		numeric(6,0) DEFAULT 0,
kzk_nyudjgncnt		numeric(2,0) DEFAULT 0,
kzk_nyumjgngak		numeric(6,0) DEFAULT 0,
kzk_nyumjgncnt		numeric(2,0) DEFAULT 0,
kzk_nyuskjryykbn	character(1),
tsy_gaiftnkbn		character(1),
tsy_gaikftnrate		numeric(3,0) DEFAULT 0,
tsy_gaikkotgak		numeric(6,0) DEFAULT 0,
tsy_gaikjgngak		numeric(6,0) DEFAULT 0,
tsy_gaidjgngak		numeric(6,0) DEFAULT 0,
tsy_gaidjgncnt		numeric(2,0) DEFAULT 0,
tsy_gaimnaijgngak	numeric(6,0) DEFAULT 0,
tsy_gaimgaijgngak	numeric(6,0) DEFAULT 0,
tsy_gaimjgncnt		numeric(2,0) DEFAULT 0,
tsy_gaiykzftnkbn	character(1),
tsy_nyuftnkbn		character(1),
tsy_nyukftnrate		numeric(3,0) DEFAULT 0,
tsy_nyukkotgak		numeric(6,0) DEFAULT 0,
tsy_nyukjgngak		numeric(6,0) DEFAULT 0,
tsy_nyudjgngak		numeric(6,0) DEFAULT 0,
tsy_nyudjgncnt		numeric(2,0) DEFAULT 0,
tsy_nyumjgngak		numeric(6,0) DEFAULT 0,
tsy_nyumjgncnt		numeric(2,0) DEFAULT 0,
tsy_nyuskjryykbn	character(1),
tnk_gaiftnkbn		character(1),
tnk_gaikftnrate		numeric(3,0) DEFAULT 0,
tnk_gaikkotgak		numeric(6,0) DEFAULT 0,
tnk_gaikjgngak		numeric(6,0) DEFAULT 0,
tnk_gaidjgngak		numeric(6,0) DEFAULT 0,
tnk_gaidjgncnt		numeric(2,0) DEFAULT 0,
tnk_gaimnaijgngak	numeric(6,0) DEFAULT 0,
tnk_gaimgaijgngak	numeric(6,0) DEFAULT 0,
tnk_gaimjgncnt		numeric(2,0) DEFAULT 0,
tnk_gaiykzftnkbn	character(1),
tnk_nyuftnkbn		character(1),
tnk_nyukftnrate		numeric(3,0) DEFAULT 0,
tnk_nyukkotgak		numeric(6,0) DEFAULT 0,
tnk_nyukjgngak		numeric(6,0) DEFAULT 0,
tnk_nyudjgngak		numeric(6,0) DEFAULT 0,
tnk_nyudjgncnt		numeric(2,0) DEFAULT 0,
tnk_nyumjgngak		numeric(6,0) DEFAULT 0,
tnk_nyumjgncnt		numeric(2,0) DEFAULT 0,
tnk_nyuskjryykbn	character(1),
termid			character varying(16),
opid			character varying(16),
creymd			character(8),
upymd			character(8),
uphms			character(6),
Constraint "tbl_hknnumwk_primary_key" Primary Key ("hospid", "hknnum", "tekstymd", "paykbn")
);


-- TBL_HKNNUM �� TBL_HKNNUMWK --

insert into TBL_HKNNUMWK
select * from TBL_HKNNUM;

-- TBL_HKNNUM ��� --

drop table TBL_HKNNUM;

-- TBL_HKNNUM ���� --
create table TBL_HKNNUM (
hospid			character(24) NOT NULL,
hknnum			character(3) NOT NULL,
tekstymd		character(8) NOT NULL,
paykbn			character(2) NOT NULL,
tekedymd		character(8),
hbtnum			character(2),
seidoname		character varying(100),
tanseidoname		character varying(20),
hknkohsbtkbn		character(1),
hbtnumchkkbn		character(1),
kensnumchkkbn		character(1),
jkyskensnumchkkbn	character(1),
hknkoukbn		character(1),
genhknnum1		character(3),
genhknnum2		character(3),
genhknnum3		character(3),
joken_styear		numeric(3,0) DEFAULT 0,
joken_edyear		numeric(3,0) DEFAULT 0,
tentnk			numeric(3,0) DEFAULT 0,
receskykbn		character(1),
receftndspkbn		character(1),
recejgndspkbn		character(1) DEFAULT '0',
hon_gaiftnkbn		character(1),
hon_gaikftnrate		numeric(3,0) DEFAULT 0,
hon_gaikkotgak		numeric(6,0) DEFAULT 0,
hon_gaikjgngak		numeric(6,0) DEFAULT 0,
hon_gaidjgngak		numeric(6,0) DEFAULT 0,
hon_gaidjgncnt		numeric(2,0) DEFAULT 0,
hon_gaimnaijgngak	numeric(6,0) DEFAULT 0,
hon_gaimgaijgngak	numeric(6,0) DEFAULT 0,
hon_gaimjgncnt		numeric(2,0) DEFAULT 0,
hon_gaiykzftnkbn	character(1),
hon_nyuftnkbn		character(1),
hon_nyukftnrate		numeric(3,0) DEFAULT 0,
hon_nyukkotgak		numeric(6,0) DEFAULT 0,
hon_nyukjgngak		numeric(6,0) DEFAULT 0,
hon_nyudjgngak		numeric(6,0) DEFAULT 0,
hon_nyudjgncnt		numeric(2,0) DEFAULT 0,
hon_nyumjgngak		numeric(6,0) DEFAULT 0,
hon_nyumjgncnt		numeric(2,0) DEFAULT 0,
hon_nyuskjryykbn	character(1),
hon_nyuskjjyogak	numeric(6,0) DEFAULT 0,
kzk_gaiftnkbn		character(1),
kzk_gaikftnrate		numeric(3,0) DEFAULT 0,
kzk_gaikkotgak		numeric(6,0) DEFAULT 0,
kzk_gaikjgngak		numeric(6,0) DEFAULT 0,
kzk_gaidjgngak		numeric(6,0) DEFAULT 0,
kzk_gaidjgncnt		numeric(2,0) DEFAULT 0,
kzk_gaimnaijgngak	numeric(6,0) DEFAULT 0,
kzk_gaimgaijgngak	numeric(6,0) DEFAULT 0,
kzk_gaimjgncnt		numeric(2,0) DEFAULT 0,
kzk_gaiykzftnkbn	character(1),
kzk_nyuftnkbn		character(1),
kzk_nyukftnrate		numeric(3,0) DEFAULT 0,
kzk_nyukkotgak		numeric(6,0) DEFAULT 0,
kzk_nyukjgngak		numeric(6,0) DEFAULT 0,
kzk_nyudjgngak		numeric(6,0) DEFAULT 0,
kzk_nyudjgncnt		numeric(2,0) DEFAULT 0,
kzk_nyumjgngak		numeric(6,0) DEFAULT 0,
kzk_nyumjgncnt		numeric(2,0) DEFAULT 0,
kzk_nyuskjryykbn	character(1),
kzk_nyuskjjyogak	numeric(6,0) DEFAULT 0,
tsy_gaiftnkbn		character(1),
tsy_gaikftnrate		numeric(3,0) DEFAULT 0,
tsy_gaikkotgak		numeric(6,0) DEFAULT 0,
tsy_gaikjgngak		numeric(6,0) DEFAULT 0,
tsy_gaidjgngak		numeric(6,0) DEFAULT 0,
tsy_gaidjgncnt		numeric(2,0) DEFAULT 0,
tsy_gaimnaijgngak	numeric(6,0) DEFAULT 0,
tsy_gaimgaijgngak	numeric(6,0) DEFAULT 0,
tsy_gaimjgncnt		numeric(2,0) DEFAULT 0,
tsy_gaiykzftnkbn	character(1),
tsy_nyuftnkbn		character(1),
tsy_nyukftnrate		numeric(3,0) DEFAULT 0,
tsy_nyukkotgak		numeric(6,0) DEFAULT 0,
tsy_nyukjgngak		numeric(6,0) DEFAULT 0,
tsy_nyudjgngak		numeric(6,0) DEFAULT 0,
tsy_nyudjgncnt		numeric(2,0) DEFAULT 0,
tsy_nyumjgngak		numeric(6,0) DEFAULT 0,
tsy_nyumjgncnt		numeric(2,0) DEFAULT 0,
tsy_nyuskjryykbn	character(1),
tsy_nyuskjjyogak	numeric(6,0) DEFAULT 0,
tnk_gaiftnkbn		character(1),
tnk_gaikftnrate		numeric(3,0) DEFAULT 0,
tnk_gaikkotgak		numeric(6,0) DEFAULT 0,
tnk_gaikjgngak		numeric(6,0) DEFAULT 0,
tnk_gaidjgngak		numeric(6,0) DEFAULT 0,
tnk_gaidjgncnt		numeric(2,0) DEFAULT 0,
tnk_gaimnaijgngak	numeric(6,0) DEFAULT 0,
tnk_gaimgaijgngak	numeric(6,0) DEFAULT 0,
tnk_gaimjgncnt		numeric(2,0) DEFAULT 0,
tnk_gaiykzftnkbn	character(1),
tnk_nyuftnkbn		character(1),
tnk_nyukftnrate		numeric(3,0) DEFAULT 0,
tnk_nyukkotgak		numeric(6,0) DEFAULT 0,
tnk_nyukjgngak		numeric(6,0) DEFAULT 0,
tnk_nyudjgngak		numeric(6,0) DEFAULT 0,
tnk_nyudjgncnt		numeric(2,0) DEFAULT 0,
tnk_nyumjgngak		numeric(6,0) DEFAULT 0,
tnk_nyumjgncnt		numeric(2,0) DEFAULT 0,
tnk_nyuskjryykbn	character(1),
tnk_nyuskjjyogak	numeric(6,0) DEFAULT 0,
termid			character varying(16),
opid			character varying(16),
creymd			character(8),
upymd			character(8),
uphms			character(6),
Constraint "tbl_hknnum_primary_key" Primary Key ("hospid", "hknnum", "tekstymd", "paykbn")
);

-- TBL_HKNNUMWK �� TBL_HKNNUM --

insert into TBL_HKNNUM
(hospid,
hknnum,
tekstymd,
paykbn,
tekedymd,
hbtnum,
seidoname,
tanseidoname,
hknkohsbtkbn,
hbtnumchkkbn,
kensnumchkkbn,
jkyskensnumchkkbn,
hknkoukbn,
genhknnum1,
genhknnum2,
genhknnum3,
joken_styear,
joken_edyear,
tentnk,
receskykbn,
receftndspkbn,
--recejgndspkbn--
hon_gaiftnkbn,
hon_gaikftnrate,
hon_gaikkotgak,
hon_gaikjgngak,
hon_gaidjgngak,
hon_gaidjgncnt,
hon_gaimnaijgngak,
hon_gaimgaijgngak,
hon_gaimjgncnt,
hon_gaiykzftnkbn,
hon_nyuftnkbn,
hon_nyukftnrate,
hon_nyukkotgak,
hon_nyukjgngak,
hon_nyudjgngak,
hon_nyudjgncnt,
hon_nyumjgngak,
hon_nyumjgncnt,
hon_nyuskjryykbn,
--hon_nyuskjjyogak--
kzk_gaiftnkbn,
kzk_gaikftnrate,
kzk_gaikkotgak,
kzk_gaikjgngak,
kzk_gaidjgngak,
kzk_gaidjgncnt,
kzk_gaimnaijgngak,
kzk_gaimgaijgngak,
kzk_gaimjgncnt,
kzk_gaiykzftnkbn,
kzk_nyuftnkbn,
kzk_nyukftnrate,
kzk_nyukkotgak,
kzk_nyukjgngak,
kzk_nyudjgngak,
kzk_nyudjgncnt,
kzk_nyumjgngak,
kzk_nyumjgncnt,
kzk_nyuskjryykbn,
--kzk_nyuskjjyogak--
tsy_gaiftnkbn,
tsy_gaikftnrate,
tsy_gaikkotgak,
tsy_gaikjgngak,
tsy_gaidjgngak,
tsy_gaidjgncnt,
tsy_gaimnaijgngak,
tsy_gaimgaijgngak,
tsy_gaimjgncnt,
tsy_gaiykzftnkbn,
tsy_nyuftnkbn,
tsy_nyukftnrate,
tsy_nyukkotgak,
tsy_nyukjgngak,
tsy_nyudjgngak,
tsy_nyudjgncnt,
tsy_nyumjgngak,
tsy_nyumjgncnt,
tsy_nyuskjryykbn,
--tsy_nyuskjjyogak--
tnk_gaiftnkbn,
tnk_gaikftnrate,
tnk_gaikkotgak,
tnk_gaikjgngak,
tnk_gaidjgngak,
tnk_gaidjgncnt,
tnk_gaimnaijgngak,
tnk_gaimgaijgngak,
tnk_gaimjgncnt,
tnk_gaiykzftnkbn,
tnk_nyuftnkbn,
tnk_nyukftnrate,
tnk_nyukkotgak,
tnk_nyukjgngak,
tnk_nyudjgngak,
tnk_nyudjgncnt,
tnk_nyumjgngak,
tnk_nyumjgncnt,
tnk_nyuskjryykbn,
--tnk_nyuskjjyogak--
termid,
opid,
creymd,
upymd,
uphms)
select
hospid,
hknnum,
tekstymd,
paykbn,
tekedymd,
hbtnum,
seidoname,
tanseidoname,
hknkohsbtkbn,
hbtnumchkkbn,
kensnumchkkbn,
jkyskensnumchkkbn,
hknkoukbn,
genhknnum1,
genhknnum2,
genhknnum3,
joken_styear,
joken_edyear,
tentnk,
receskykbn,
receftndspkbn,
hon_gaiftnkbn,
hon_gaikftnrate,
hon_gaikkotgak,
hon_gaikjgngak,
hon_gaidjgngak,
hon_gaidjgncnt,
hon_gaimnaijgngak,
hon_gaimgaijgngak,
hon_gaimjgncnt,
hon_gaiykzftnkbn,
hon_nyuftnkbn,
hon_nyukftnrate,
hon_nyukkotgak,
hon_nyukjgngak,
hon_nyudjgngak,
hon_nyudjgncnt,
hon_nyumjgngak,
hon_nyumjgncnt,
hon_nyuskjryykbn,
kzk_gaiftnkbn,
kzk_gaikftnrate,
kzk_gaikkotgak,
kzk_gaikjgngak,
kzk_gaidjgngak,
kzk_gaidjgncnt,
kzk_gaimnaijgngak,
kzk_gaimgaijgngak,
kzk_gaimjgncnt,
kzk_gaiykzftnkbn,
kzk_nyuftnkbn,
kzk_nyukftnrate,
kzk_nyukkotgak,
kzk_nyukjgngak,
kzk_nyudjgngak,
kzk_nyudjgncnt,
kzk_nyumjgngak,
kzk_nyumjgncnt,
kzk_nyuskjryykbn,
tsy_gaiftnkbn,
tsy_gaikftnrate,
tsy_gaikkotgak,
tsy_gaikjgngak,
tsy_gaidjgngak,
tsy_gaidjgncnt,
tsy_gaimnaijgngak,
tsy_gaimgaijgngak,
tsy_gaimjgncnt,
tsy_gaiykzftnkbn,
tsy_nyuftnkbn,
tsy_nyukftnrate,
tsy_nyukkotgak,
tsy_nyukjgngak,
tsy_nyudjgngak,
tsy_nyudjgncnt,
tsy_nyumjgngak,
tsy_nyumjgncnt,
tsy_nyuskjryykbn,
tnk_gaiftnkbn,
tnk_gaikftnrate,
tnk_gaikkotgak,
tnk_gaikjgngak,
tnk_gaidjgngak,
tnk_gaidjgncnt,
tnk_gaimnaijgngak,
tnk_gaimgaijgngak,
tnk_gaimjgncnt,
tnk_gaiykzftnkbn,
tnk_nyuftnkbn,
tnk_nyukftnrate,
tnk_nyukkotgak,
tnk_nyukjgngak,
tnk_nyudjgngak,
tnk_nyudjgncnt,
tnk_nyumjgngak,
tnk_nyumjgncnt,
tnk_nyuskjryykbn,
termid,
opid,
creymd,
upymd,
uphms
from TBL_HKNNUMWK;

-- TBL_HKNNUMWK ��� --

drop table TBL_HKNNUMWK;
