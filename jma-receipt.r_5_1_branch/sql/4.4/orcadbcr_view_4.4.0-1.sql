--                                              --
-- ¥Ó¥å¡¼ºÆºîÀ®                                 --
-- view_q002   ¾È²ñ£²                           --
-- view_q003   ¾È²ñ£³                           --
-- view_bd001  ¼ýÇ¼£±                           --
-- view_bd002  ¼ýÇ¼£²                           --
-- view_bd003  ¼ýÇ¼£³                           --
-- view_q001   ¾È²ñ£±                           --
--                                              --
-- Create Date : 2009/01/15                     --
--                                              --
\set ON_ERROR_STOP

CREATE VIEW view_q002 AS
    SELECT a.hospnum,
           a.ptid,
           a.nyugaikbn,
           a.sryka,
           a.srykbn,
           a.srysyukbn,
           b.sryymd,
           a.srycd1,
           a.srycd2,
           a.srycd3,
           a.srycd4,
           a.srycd5 
      FROM tbl_sryact a,
           tbl_jyurrk b 
     WHERE (((((a.hospnum = b.hospnum) 
       AND (a.ptid = b.ptid)) 
       AND (a.nyugaikbn = b.nyugaikbn)) 
       AND (a.sryka = b.sryka)) 
       AND (((((((((((((((a.zainum = b.zainum1) 
        OR (a.zainum = b.zainum2)) 
        OR (a.zainum = b.zainum3)) 
        OR (a.zainum = b.zainum4)) 
        OR (a.zainum = b.zainum5)) 
        OR (a.zainum = b.zainum6)) 
        OR (a.zainum = b.zainum7)) 
        OR (a.zainum = b.zainum8)) 
        OR (a.zainum = b.zainum9)) 
        OR (a.zainum = b.zainum10)) 
        OR (a.zainum = b.zainum11)) 
        OR (a.zainum = b.zainum12)) 
        OR (a.zainum = b.zainum13)) 
        OR (a.zainum = b.zainum14)) 
        OR (a.zainum = b.zainum15)))
union all
	select	a.hospnum,a.ptid,'1' as nyugaikbn ,b.sryka,b.srykbn,'',a.sryymd,b.srycd as srycd1 ,'' as srycd2 , '' as srycd3, '' as srycd4,'' as srycd5     from tbl_nrrksrh a , tbl_nsrysrh b where a.hospnum = b.hospnum and a.ptid = b.ptid and a.zainum = b.zainum 
;
COMMENT 
        ON VIEW view_q002 IS '´µ¼Ô¾È²ñ¥Ó¥å¡¼2';



create  view view_q003 as
	select	hospnum,ptid,nyugaikbn,sryka,srykbn,srysyukbn,srycd1,srycd2,srycd3,srycd4,srycd5 from tbl_sryact
union all
	select	hospnum,ptid,'1' as nyugaikbn ,sryka,srykbn,'',srycd as srycd1 ,'' as srycd2 , '' as srycd3, '' as srycd4,'' as srycd5     from tbl_nsrysrh
;

COMMENT 
        ON VIEW view_q003 IS '´µ¼Ô¾È²ñ¥Ó¥å¡¼3';


CREATE VIEW view_bd001 AS
    SELECT a.hospnum, a.ptid, b.ptnum, c.name, c.sex, c.birthday, a.sryka, a.denpprtymd, a.skymoney, a.nyukin_total FROM tbl_syunou_main a, tbl_ptnum b, tbl_ptinf c WHERE ((((((((a.hospnum = b.hospnum) AND (a.ptid = b.ptid)) AND (a.hospnum = c.hospnum)) AND (a.ptid = c.ptid)) AND (a.denpjtikbn <> '3')) AND (a.denpjtikbn <> '7')) AND (a.createkbn <> '3')) AND (c.tstptnumkbn <> '1'));

COMMENT ON VIEW view_bd001 IS '¼ýÇ¼¥Ó¥å¡¼';

CREATE view view_bd002 as
select
c.ptnum,
a.nyugaikbn,
a.ptid,
a.denpnum,
a.denplastnum,
a.sryka,
a.sryymd,
a.denpjtikbn,
a.createkbn,
a.denpprtymd,
a.skystymd,
a.skyedymd,
a.hkncombinum,
a.syuhknnum,
a.syuhknftnmoney,
a.syucompftn,
a.syucompftn_entani,
a.syucompykzftn,
a.syucomptotal,
a.syuykzftnkbn,
a.koh1hknnum,
a.koh1hknftnmoney,
a.koh1compftn,
a.koh1compftn_entani,
a.koh1compykzftn,
a.koh1comptotal,
a.koh1ykzftnkbn,
a.koh2hknnum,
a.koh2hknftnmoney,
a.koh2compftn,
a.koh2compftn_entani,
a.koh2compykzftn,
a.koh2comptotal,
a.koh2ykzftnkbn,
a.koh3hknnum,
a.koh3hknftnmoney,
a.koh3compftn,
a.koh3compftn_entani,
a.koh3compykzftn,
a.koh3comptotal,
a.koh3ykzftnkbn,
a.koh4hknnum,
a.koh4hknftnmoney,
a.koh4compftn,
a.koh4compftn_entani,
a.koh4compykzftn,
a.koh4comptotal,
a.koh4ykzftnkbn,
a.ptftnrate,
a.skykukbn,
a.tax_taishou,
a.tax_money,
a.skygk,
a.ssu_hknten,
a.ssu_money,
a.ssu_tgmoney,
a.ssu_tgmoney_tax,
a.sdo_hknten,
a.sdo_money,
a.sdo_tgmoney,
a.sdo_tgmoney_tax,
a.ztk_hknten,
a.ztk_money,
a.ztk_tgmoney,
a.ztk_tgmoney_tax,
a.tyk_hknten,
a.tyk_money,
a.tyk_tgmoney,
a.tyk_tgmoney_tax,
a.csy_hknten,
a.csy_money,
a.csy_tgmoney,
a.csy_tgmoney_tax,
a.syc_hknten,
a.syc_money,
a.syc_tgmoney,
a.syc_tgmoney_tax,
a.sjt_hknten,
a.sjt_money,
a.sjt_tgmoney,
a.sjt_tgmoney_tax,
a.kns_hknten,
a.kns_money,
a.kns_tgmoney,
a.kns_tgmoney_tax,
a.gzu_hknten,
a.gzu_money,
a.gzu_tgmoney,
a.gzu_tgmoney_tax,
a.etc_hknten,
a.etc_money,
a.etc_tgmoney,
a.etc_tgmoney_tax,
a.nyn_hknten,
a.nyn_money,
a.nyn_tgmoney,
a.nyn_tgmoney_tax,
a.total_hknten,
a.total_money,
a.total_tgmoney,
a.total_tgmoney_tax,
a.tgmoney_tax_sai,
a.jihi_1,
a.jihi_1_tax,
a.jihi_2,
a.jihi_2_tax,
a.jihi_3,
a.jihi_3_tax,
a.jihi_4,
a.jihi_4_tax,
a.jihi_5,
a.jihi_5_tax,
a.jihi_6,
a.jihi_6_tax,
a.jihi_7,
a.jihi_7_tax,
a.jihi_8,
a.jihi_8_tax,
a.jihi_9,
a.jihi_9_tax,
a.jihi_10,
a.jihi_10_tax,
a.jihi_total,
a.jihi_total_tax,
a.jihi_tax,
a.rjn_ftn,
a.koh_ftn,
a.koh_ftn_entani,
a.ykz_ftn,
a.rese_ykz_ftn,
a.kohtaiykzkbn,
a.chosei,
a.grp_denpnum,
a.grp_rennum,
a.grp_sgkmoney,
a.secmoney,
a.hkntekmoney,
a.discount_kbn,
a.discount_body,
a.discount_ratekbn,
a.discount_teiritu,
a.discount_rate,
a.discount_money,
a.rsishoshin_money,
a.rsisaishin_money,
a.rsisdo_money,
a.rsietc_money,
a.rsi_tax_sai,
a.rsi_total,
a.rsijibai_skymoney,
a.skymoney_tax_sai,
a.skymoney,
a.nyukin_total,
a.nyukin_kaisu,
a.misyuriyu,
a.drcd,
a.nynshokaisu,
a.ykzkennum,
a.skj_ftngaku1,
a.skj_ftnday1,
a.skj_ftngaku2,
a.skj_ftnday2,
a.skj_ftngaku3,
a.skj_ftnday3,
a.saikeisankbn,
a.ingaishohokbn,
a.douji_denpnum,
a.contkbn,
a.fuku_denpnum,
a.fuku_kbn,
a.zaitaku,
a.kyufugai_shoshin_ten,
a.kyufugai_sidou_ten,
a.kyufugai_osin1_ten,
a.kyufugai_osin2_ten,
a.kyufugai_osin3_ten,
a.kyufugai_gokei_ten,
a.dayinfflg,
a.acct_updkbn,
a.jyo_hknftnmoney,
a.jyo_compftn,
a.jyo_compftn_entani,
a.termid,
a.opid,
a.creymd,
a.upymd,
a.uphms,
a.msi_hknten,
a.msi_money,
a.msi_tgmoney,
a.msi_tgmoney_tax,
a.ssn_hknten,
a.ssn_money,
a.ssn_tgmoney,
a.ssn_tgmoney_tax,
a.hou_hknten,
a.hou_money,
a.hou_tgmoney,
a.hou_tgmoney_tax,
a.ryo_hknten,
a.ryo_money,
a.ryo_tgmoney,
a.ryo_tgmoney_tax,
a.shohou_sai,
a.grp_hakhouflg,
a.hospnum,
a.byr_hknten,
a.byr_money,
a.byr_tgmoney,
a.byr_tgmoney_tax,
    coalesce(b.roomnum ,'') as roomnum ,
    coalesce(b.byotonum ,'') as byotonum ,
    coalesce(b.rmsagaku ,0) as rmsagaku ,
    coalesce(b.rmsagaku_tax_sai ,0) as rmsagaku_tax_sai ,
    coalesce(b.shokuji1_nissu ,0) as shokuji1_nissu ,
    coalesce(b.shokuji1 ,0) as shokuji1 ,
    coalesce(b.shokuji2_nissu ,0) as shokuji2_nissu ,
    coalesce(b.shokuji2 ,0) as shokuji2 ,
    coalesce(b.shokuji3_nissu ,0) as shokuji3_nissu ,
    coalesce(b.shokuji3 ,0) as shokuji3 ,
    coalesce(b.shokuji4_nissu ,0) as shokuji4_nissu ,
    coalesce(b.shokuji4 ,0) as shokuji4 ,
    coalesce(b.shokuji5_nissu ,0) as shokuji5_nissu ,
    coalesce(b.shokuji5 ,0) as shokuji5 ,
    coalesce(b.shokuji6_nissu ,0) as shokuji6_nissu ,
    coalesce(b.shokuji6 ,0) as shokuji6 ,
    coalesce(b.shokuji7_nissu ,0) as shokuji7_nissu ,
    coalesce(b.shokuji7 ,0) as shokuji7 ,
    coalesce(b.syuskj_nissu ,0) as syuskj_nissu ,
    coalesce(b.syuskj_ryoyohi ,0) as syuskj_ryoyohi ,
    coalesce(b.syuskj_ftn ,0) as syuskj_ftn ,
    coalesce(b.syuskj_ftn_rece ,0) as syuskj_ftn_rece ,
    coalesce(b.syuskj_ftnkbn ,'') as syuskj_ftnkbn ,
    coalesce(b.koh1skj_nissu ,0) as koh1skj_nissu ,
    coalesce(b.koh1skj_ryoyohi ,0) as koh1skj_ryoyohi ,
    coalesce(b.koh1skj_ftn ,0) as koh1skj_ftn ,
    coalesce(b.koh1skj_ftn_rece ,0) as koh1skj_ftn_rece ,
    coalesce(b.koh1skj_ftnkbn ,'') as koh1skj_ftnkbn ,
    coalesce(b.koh2skj_nissu ,0) as koh2skj_nissu ,
    coalesce(b.koh2skj_ryoyohi ,0) as koh2skj_ryoyohi ,
    coalesce(b.koh2skj_ftn ,0) as koh2skj_ftn ,
    coalesce(b.koh2skj_ftn_rece ,0) as koh2skj_ftn_rece ,
    coalesce(b.koh2skj_ftnkbn ,'') as koh2skj_ftnkbn ,
    coalesce(b.koh3skj_nissu ,0) as koh3skj_nissu ,
    coalesce(b.koh3skj_ryoyohi ,0) as koh3skj_ryoyohi ,
    coalesce(b.koh3skj_ftn ,0) as koh3skj_ftn ,
    coalesce(b.koh3skj_ftn_rece ,0) as koh3skj_ftn_rece ,
    coalesce(b.koh3skj_ftnkbn ,'') as koh3skj_ftnkbn ,
    coalesce(b.koh4skj_nissu ,0) as koh4skj_nissu ,
    coalesce(b.koh4skj_ryoyohi ,0) as koh4skj_ryoyohi ,
    coalesce(b.koh4skj_ftn ,0) as koh4skj_ftn ,
    coalesce(b.koh4skj_ftn_rece ,0) as koh4skj_ftn_rece ,
    coalesce(b.koh4skj_ftnkbn ,'') as koh4skj_ftnkbn ,
    coalesce(b.ryoyohi_skj ,0) as ryoyohi_skj ,
    coalesce(b.skymoney_skj ,0) as skymoney_skj ,
    coalesce(b.skymoney_skj_tax ,0) as skymoney_skj_tax ,
    coalesce(b.skymoney_skj_kei ,0) as skymoney_skj_kei ,
    coalesce(b.ryoyohi_skj_jihi ,0) as ryoyohi_skj_jihi ,
    coalesce(b.skymoney_skj_jihi ,0) as skymoney_skj_jihi ,
    coalesce(b.skymoney_skj_jihi_tax ,0) as skymoney_skj_jihi_tax ,
    coalesce(b.skymoney_skj_jihi_kei ,0) as skymoney_skj_jihi_kei ,
    coalesce(b.day_1 ,'') as day_1 ,
    coalesce(b.day_2 ,'') as day_2 ,
    coalesce(b.day_3 ,'') as day_3 ,
    coalesce(b.day_4 ,'') as day_4 ,
    coalesce(b.day_5 ,'') as day_5 ,
    coalesce(b.day_6 ,'') as day_6 ,
    coalesce(b.day_7 ,'') as day_7 ,
    coalesce(b.day_8 ,'') as day_8 ,
    coalesce(b.day_9 ,'') as day_9 ,
    coalesce(b.day_10 ,'') as day_10 ,
    coalesce(b.day_11 ,'') as day_11 ,
    coalesce(b.day_12 ,'') as day_12 ,
    coalesce(b.day_13 ,'') as day_13 ,
    coalesce(b.day_14 ,'') as day_14 ,
    coalesce(b.day_15 ,'') as day_15 ,
    coalesce(b.day_16 ,'') as day_16 ,
    coalesce(b.day_17 ,'') as day_17 ,
    coalesce(b.day_18 ,'') as day_18 ,
    coalesce(b.day_19 ,'') as day_19 ,
    coalesce(b.day_20 ,'') as day_20 ,
    coalesce(b.day_21 ,'') as day_21 ,
    coalesce(b.day_22 ,'') as day_22 ,
    coalesce(b.day_23 ,'') as day_23 ,
    coalesce(b.day_24 ,'') as day_24 ,
    coalesce(b.day_25 ,'') as day_25 ,
    coalesce(b.day_26 ,'') as day_26 ,
    coalesce(b.day_27 ,'') as day_27 ,
    coalesce(b.day_28 ,'') as day_28 ,
    coalesce(b.day_29 ,'') as day_29 ,
    coalesce(b.day_30 ,'') as day_30 ,
    coalesce(b.day_31 ,'') as day_31 ,
    coalesce(b.nyuin_rrknum ,0) as nyuin_rrknum ,
    coalesce(b.shokuji8_nissu ,0) as shokuji8_nissu ,
    coalesce(b.shokuji8 ,0) as shokuji8 ,
    coalesce(b.shokuji9_nissu ,0) as shokuji9_nissu ,
    coalesce(b.shokuji9 ,0) as shokuji9 ,
    coalesce(b.shokuji10_nissu ,0) as shokuji10_nissu ,
    coalesce(b.shokuji10 ,0) as shokuji10 ,
    coalesce(b.ryoyohi_life ,0) as ryoyohi_life ,
    coalesce(b.skymoney_life ,0) as skymoney_life ,
    coalesce(b.skymoney_life_tax ,0) as skymoney_life_tax ,
    coalesce(b.skymoney_life_kei ,0) as skymoney_life_kei ,
    coalesce(b.ryoyohi_life_jihi ,0) as ryoyohi_life_jihi ,
    coalesce(b.skymoney_life_jihi ,0) as skymoney_life_jihi ,
    coalesce(b.skymoney_life_jihi_tax ,0) as skymoney_life_jihi_tax ,
    coalesce(b.skymoney_life_jihi_kei ,0) as skymoney_life_jihi_kei
from tbl_syunou_main a left join tbl_syunou_nyuin b using (hospnum,nyugaikbn,ptid,denpnum),tbl_ptnum c,tbl_ptinf d
WHERE a.hospnum = c.hospnum
AND   a.ptid    = c.ptid
AND   a.hospnum = d.hospnum
AND   a.ptid    = d.ptid
AND   a.denpjtikbn <> '3'
AND   a.createkbn <> '3'
AND   d.tstptnumkbn <> '1'
;

COMMENT ON VIEW view_bd002 IS '¼ýÇ¼¥Ó¥å¡¼2';

CREATE VIEW view_bd003 AS
    SELECT a.hospnum, a.nyugaikbn, a.ptid, a.denpnum, a.denpedanum, a.skymoney, a.nyuhen_money, a.nyuhen_ymd, b.sryymd FROM (tbl_syumei a JOIN tbl_syunou_main b USING (hospnum, nyugaikbn, ptid, denpnum));


COMMENT ON VIEW view_bd003 IS '¼ýÇ¼¥Ó¥å¡¼3';


CREATE VIEW view_q001 AS
    SELECT tbl_hkncombi.hospnum,
           tbl_hkncombi.ptid,
           tbl_hkncombi.hkncombinum,
           view_q004.nyugaikbn,
           view_q004.sryka,
           view_q004.sryymd,
           tbl_pthkninf.hknid,
           tbl_pthkninf.hknnum,
           tbl_pthkninf.hknjanum,
           tbl_pthkninf.honkzkkbn,
           tbl_pthkninf.kigo,
           tbl_pthkninf.num,
           tbl_ptkohinf.kohid,
           tbl_ptkohinf.kohnum,
           tbl_ptkohinf.paykbn,
           tbl_ptkohinf.ftnjanum 
      FROM (((tbl_hkncombi JOIN view_q004 
        ON ((((tbl_hkncombi.hospnum = view_q004.hospnum) 
       AND (tbl_hkncombi.ptid = view_q004.ptid)) 
       AND (tbl_hkncombi.hkncombinum = to_number((view_q004.hkncombinum),
           '9999'))))) 
      LEFT JOIN tbl_pthkninf 
        ON (((((tbl_hkncombi.hospnum = tbl_pthkninf.hospnum) 
       AND (tbl_hkncombi.ptid = tbl_pthkninf.ptid)) 
       AND (tbl_hkncombi.hknid = tbl_pthkninf.hknid)) 
       AND (tbl_hkncombi.hknnum = tbl_pthkninf.hknnum)))) 
      LEFT JOIN tbl_ptkohinf 
        ON ((((tbl_hkncombi.hospnum = tbl_ptkohinf.hospnum) 
       AND (tbl_hkncombi.ptid = tbl_ptkohinf.ptid)) 
       AND (((((tbl_hkncombi.koh1id = tbl_ptkohinf.kohid) 
       AND (tbl_hkncombi.koh1hknnum = tbl_ptkohinf.kohnum)) 
        OR ((tbl_hkncombi.koh2id = tbl_ptkohinf.kohid) 
       AND (tbl_hkncombi.koh2hknnum = tbl_ptkohinf.kohnum))) 
        OR ((tbl_hkncombi.koh3id = tbl_ptkohinf.kohid) 
       AND (tbl_hkncombi.koh3hknnum = tbl_ptkohinf.kohnum))) 
        OR ((tbl_hkncombi.koh4id = tbl_ptkohinf.kohid) 
       AND (tbl_hkncombi.koh4hknnum = tbl_ptkohinf.kohnum)))))) UNION SELECT tbl_hkncombi.hospnum,
           tbl_hkncombi.ptid,
           tbl_hkncombi.hkncombinum,
           view_q004.nyugaikbn,
           view_q004.sryka,
           view_q004.sryymd,
           tbl_pthkninf.hknid,
           tbl_pthkninf.hknnum,
           tbl_pthkninf.hknjanum,
           tbl_pthkninf.honkzkkbn,
           tbl_pthkninf.kigo,
           tbl_pthkninf.num,
           tbl_ptkohinf.kohid,
           tbl_ptkohinf.kohnum,
           tbl_ptkohinf.paykbn,
           tbl_ptkohinf.ftnjanum 
      FROM ((view_q004 JOIN tbl_hkncombi 
        ON ((((view_q004.hospnum = tbl_hkncombi.hospnum) 
       AND (view_q004.ptid = tbl_hkncombi.ptid)) 
       AND (to_number((view_q004.hkncombinum),
           '9999') = tbl_hkncombi.hkncombinum)))) 
      LEFT JOIN tbl_pthkninf 
        ON (((((tbl_hkncombi.hospnum = tbl_pthkninf.hospnum) 
       AND (tbl_hkncombi.ptid = tbl_pthkninf.ptid)) 
       AND (tbl_hkncombi.hknid = tbl_pthkninf.hknid)) 
       AND (tbl_hkncombi.hknnum = tbl_pthkninf.hknnum)))),
           tbl_ptkohinf 
     WHERE (((((view_q004.hospnum = tbl_ptkohinf.hospnum) 
       AND (view_q004.ptid = tbl_ptkohinf.ptid)) 
       AND (((((((((((((tbl_ptkohinf.kohnum = '960') 
        OR (tbl_ptkohinf.kohnum = '961')) 
        OR (tbl_ptkohinf.kohnum = '962')) 
        OR (tbl_ptkohinf.kohnum = '963')) 
        OR (tbl_ptkohinf.kohnum = '964')) 
        OR (tbl_ptkohinf.kohnum = '965')) 
        OR (tbl_ptkohinf.kohnum = '966')) 
        OR (tbl_ptkohinf.kohnum = '967')) 
        OR (tbl_ptkohinf.kohnum = '968')) 
        OR (tbl_ptkohinf.kohnum = '969')) 
        OR (tbl_ptkohinf.kohnum = '977')) 
        OR (tbl_ptkohinf.kohnum = '978')) 
        OR (tbl_ptkohinf.kohnum = '979'))) 
       AND (view_q004.sryymd >= tbl_ptkohinf.tekstymd)) 
       AND (view_q004.sryymd <= tbl_ptkohinf.tekedymd));

COMMENT 
        ON VIEW view_q001 IS '´µ¼Ô¾È²ñ¥Ó¥å¡¼1';

