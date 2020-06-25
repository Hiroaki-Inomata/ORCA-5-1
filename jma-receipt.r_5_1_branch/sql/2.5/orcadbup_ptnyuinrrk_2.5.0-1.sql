--
-- 患者入院履歴テーブル構造変更
--
-- 退院事由(TAIINCD)追加
-- 
-- Create Date : 2004/10/13           --
--

-- (1) tbl_ptnyuinrrk_wk 作成
CREATE TABLE "tbl_ptnyuinrrk_wk" AS
	SELECT
	*
	FROM tbl_ptnyuinrrk
;

-- (2) tbl_ptnyuinrrk 削除
DROP TABLE tbl_ptnyuinrrk;

-- (3) tbl_ptnyuinrrk 作成
CREATE TABLE "tbl_ptnyuinrrk" (
	"hospid" character(24) NOT NULL,
	"ptid" numeric(10,0) NOT NULL,
	"rrknum" numeric(3,0) NOT NULL,
	"rrkedanum" numeric(3,0) NOT NULL,
	"btunum" character(2),
	"btuname" character varying(20),
	"brmnum" character(8),
	"nyuinka" character(2),
	"hkncombinum" character(4),
	"nyuinymd" character(8),
	"taiinymd" character(8),
	"taiincd" character(2),
	"tokutei_kbn" character(1),
	"tokutei_nyuin" character(2),
	"rm_sagaku" character(2),
	"teiki_seikyukbn" character(1),
	"kensaku_dispkbn" character(1),
	"shokaikbn" character(1),
	"shonum" numeric(3,0),
	"drcd1" character(5),
	"drcd2" character(5),
	"drcd3" character(5),
	"jtikbn" character(1),
	"santei_btunum" character(2),
	"santei_brmnum" character(8),
	"tennyuymd" character(8),
	"tenstuymd" character(8),
	"taisyonissu" numeric(5,0),
	"tusannissu1" numeric(5,0),
	"tusannissu2" numeric(5,0),
	"nyuinchukbn" character(1),
	"termid" character varying(16),
	"opid" character varying(16),
	"creymd" character(8),
	"upymd" character(8),
	"uphms" character(6),
	Constraint "tbl_ptnyuinrrk_primary_key" Primary Key ("hospid", "ptid", "rrknum", "rrkedanum")
);

-- (4) tbl_ptnyuinrrk_wk ->  tbl_ptnyuinrrk
INSERT INTO tbl_ptnyuinrrk (
hospid,
ptid,
rrknum,
rrkedanum,
btunum,
btuname,
brmnum,
nyuinka,
hkncombinum,
nyuinymd,
taiinymd,
taiincd,
tokutei_kbn,
tokutei_nyuin,
rm_sagaku,
teiki_seikyukbn,
kensaku_dispkbn,
shokaikbn,
shonum,
drcd1,
drcd2,
drcd3,
jtikbn,
santei_btunum,
santei_brmnum,
tennyuymd,
tenstuymd,
taisyonissu,
tusannissu1,
tusannissu2,
nyuinchukbn,
termid,
opid,
creymd,
upymd,
uphms
)
SELECT 
hospid,
ptid,
rrknum,
rrkedanum,
btunum,
btuname,
brmnum,
nyuinka,
hkncombinum,
nyuinymd,
taiinymd,
'  ',
tokutei_kbn,
tokutei_nyuin,
rm_sagaku,
teiki_seikyukbn,
kensaku_dispkbn,
shokaikbn,
shonum,
drcd1,
drcd2,
drcd3,
jtikbn,
santei_btunum,
santei_brmnum,
tennyuymd,
tenstuymd,
taisyonissu,
tusannissu1,
tusannissu2,
nyuinchukbn,
termid,
opid,
creymd,
upymd,
uphms
FROM tbl_ptnyuinrrk_wk ;


-- (5) tbl_ptnyuinrrk_wk 削除
DROP TABLE tbl_ptnyuinrrk_wk;
