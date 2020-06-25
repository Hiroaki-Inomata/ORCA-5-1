CREATE VIEW view_bd001 AS
    SELECT a.hospnum,
           a.ptid,
           b.ptnum,
           c.name,
           c.sex,
           c.birthday,
           a.sryka,
           a.denpprtymd,
           a.skymoney,
           a.nyukin_total 
      FROM tbl_syunou_main a,
           tbl_ptnum b,
           tbl_ptinf c 
     WHERE (a.hospnum = b.hospnum) 
       AND (a.ptid = b.ptid) 
       AND (a.hospnum = c.hospnum) 
       AND (a.ptid    = c.ptid) 
       AND (a.denpjtikbn  <> '3') 
       AND (a.denpjtikbn  <> '7') 
       AND (a.createkbn   <> '3') 
       AND (c.tstptnumkbn <> '1');


COMMENT ON VIEW view_bd001 IS '収納ビュー';

CREATE VIEW view_bd002 AS
    SELECT c.ptnum,
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
           a.chosei1,
           a.chosei2,
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
           COALESCE(b.roomnum,
           ''::bpchar) AS roomnum,
           COALESCE(b.byotonum,
           ''::bpchar) AS byotonum,
           COALESCE(b.rmsagaku,
           0) AS rmsagaku,
           COALESCE(b.rmsagaku_tax_sai,
           0) AS rmsagaku_tax_sai,
           COALESCE((b.shokuji1_nissu)::integer,
           0) AS shokuji1_nissu,
           COALESCE(b.shokuji1,
           0) AS shokuji1,
           COALESCE((b.shokuji2_nissu)::integer,
           0) AS shokuji2_nissu,
           COALESCE(b.shokuji2,
           0) AS shokuji2,
           COALESCE((b.shokuji3_nissu)::integer,
           0) AS shokuji3_nissu,
           COALESCE(b.shokuji3,
           0) AS shokuji3,
           COALESCE((b.shokuji4_nissu)::integer,
           0) AS shokuji4_nissu,
           COALESCE(b.shokuji4,
           0) AS shokuji4,
           COALESCE((b.shokuji5_nissu)::integer,
           0) AS shokuji5_nissu,
           COALESCE(b.shokuji5,
           0) AS shokuji5,
           COALESCE((b.shokuji6_nissu)::integer,
           0) AS shokuji6_nissu,
           COALESCE(b.shokuji6,
           0) AS shokuji6,
           COALESCE((b.shokuji7_nissu)::integer,
           0) AS shokuji7_nissu,
           COALESCE(b.shokuji7,
           0) AS shokuji7,
           COALESCE((b.syuskj_nissu)::integer,
           0) AS syuskj_nissu,
           COALESCE(b.syuskj_ryoyohi,
           0) AS syuskj_ryoyohi,
           COALESCE(b.syuskj_ftn,
           0) AS syuskj_ftn,
           COALESCE(b.syuskj_ftn_rece,
           0) AS syuskj_ftn_rece,
           COALESCE(b.syuskj_ftnkbn,
           ''::bpchar) AS syuskj_ftnkbn,
           COALESCE((b.koh1skj_nissu)::integer,
           0) AS koh1skj_nissu,
           COALESCE(b.koh1skj_ryoyohi,
           0) AS koh1skj_ryoyohi,
           COALESCE(b.koh1skj_ftn,
           0) AS koh1skj_ftn,
           COALESCE(b.koh1skj_ftn_rece,
           0) AS koh1skj_ftn_rece,
           COALESCE(b.koh1skj_ftnkbn,
           ''::bpchar) AS koh1skj_ftnkbn,
           COALESCE((b.koh2skj_nissu)::integer,
           0) AS koh2skj_nissu,
           COALESCE(b.koh2skj_ryoyohi,
           0) AS koh2skj_ryoyohi,
           COALESCE(b.koh2skj_ftn,
           0) AS koh2skj_ftn,
           COALESCE(b.koh2skj_ftn_rece,
           0) AS koh2skj_ftn_rece,
           COALESCE(b.koh2skj_ftnkbn,
           ''::bpchar) AS koh2skj_ftnkbn,
           COALESCE((b.koh3skj_nissu)::integer,
           0) AS koh3skj_nissu,
           COALESCE(b.koh3skj_ryoyohi,
           0) AS koh3skj_ryoyohi,
           COALESCE(b.koh3skj_ftn,
           0) AS koh3skj_ftn,
           COALESCE(b.koh3skj_ftn_rece,
           0) AS koh3skj_ftn_rece,
           COALESCE(b.koh3skj_ftnkbn,
           ''::bpchar) AS koh3skj_ftnkbn,
           COALESCE((b.koh4skj_nissu)::integer,
           0) AS koh4skj_nissu,
           COALESCE(b.koh4skj_ryoyohi,
           0) AS koh4skj_ryoyohi,
           COALESCE(b.koh4skj_ftn,
           0) AS koh4skj_ftn,
           COALESCE(b.koh4skj_ftn_rece,
           0) AS koh4skj_ftn_rece,
           COALESCE(b.koh4skj_ftnkbn,
           ''::bpchar) AS koh4skj_ftnkbn,
           COALESCE(b.ryoyohi_skj,
           0) AS ryoyohi_skj,
           COALESCE(b.skymoney_skj,
           0) AS skymoney_skj,
           COALESCE(b.skymoney_skj_tax,
           0) AS skymoney_skj_tax,
           COALESCE(b.skymoney_skj_kei,
           0) AS skymoney_skj_kei,
           COALESCE(b.ryoyohi_skj_jihi,
           0) AS ryoyohi_skj_jihi,
           COALESCE(b.skymoney_skj_jihi,
           0) AS skymoney_skj_jihi,
           COALESCE(b.skymoney_skj_jihi_tax,
           0) AS skymoney_skj_jihi_tax,
           COALESCE(b.skymoney_skj_jihi_kei,
           0) AS skymoney_skj_jihi_kei,
           COALESCE(b.day_1,
           ''::bpchar) AS day_1,
           COALESCE(b.day_2,
           ''::bpchar) AS day_2,
           COALESCE(b.day_3,
           ''::bpchar) AS day_3,
           COALESCE(b.day_4,
           ''::bpchar) AS day_4,
           COALESCE(b.day_5,
           ''::bpchar) AS day_5,
           COALESCE(b.day_6,
           ''::bpchar) AS day_6,
           COALESCE(b.day_7,
           ''::bpchar) AS day_7,
           COALESCE(b.day_8,
           ''::bpchar) AS day_8,
           COALESCE(b.day_9,
           ''::bpchar) AS day_9,
           COALESCE(b.day_10,
           ''::bpchar) AS day_10,
           COALESCE(b.day_11,
           ''::bpchar) AS day_11,
           COALESCE(b.day_12,
           ''::bpchar) AS day_12,
           COALESCE(b.day_13,
           ''::bpchar) AS day_13,
           COALESCE(b.day_14,
           ''::bpchar) AS day_14,
           COALESCE(b.day_15,
           ''::bpchar) AS day_15,
           COALESCE(b.day_16,
           ''::bpchar) AS day_16,
           COALESCE(b.day_17,
           ''::bpchar) AS day_17,
           COALESCE(b.day_18,
           ''::bpchar) AS day_18,
           COALESCE(b.day_19,
           ''::bpchar) AS day_19,
           COALESCE(b.day_20,
           ''::bpchar) AS day_20,
           COALESCE(b.day_21,
           ''::bpchar) AS day_21,
           COALESCE(b.day_22,
           ''::bpchar) AS day_22,
           COALESCE(b.day_23,
           ''::bpchar) AS day_23,
           COALESCE(b.day_24,
           ''::bpchar) AS day_24,
           COALESCE(b.day_25,
           ''::bpchar) AS day_25,
           COALESCE(b.day_26,
           ''::bpchar) AS day_26,
           COALESCE(b.day_27,
           ''::bpchar) AS day_27,
           COALESCE(b.day_28,
           ''::bpchar) AS day_28,
           COALESCE(b.day_29,
           ''::bpchar) AS day_29,
           COALESCE(b.day_30,
           ''::bpchar) AS day_30,
           COALESCE(b.day_31,
           ''::bpchar) AS day_31,
           COALESCE((b.nyuin_rrknum)::integer,
           0) AS nyuin_rrknum,
           COALESCE((b.shokuji8_nissu)::integer,
           0) AS shokuji8_nissu,
           COALESCE(b.shokuji8,
           0) AS shokuji8,
           COALESCE((b.shokuji9_nissu)::integer,
           0) AS shokuji9_nissu,
           COALESCE(b.shokuji9,
           0) AS shokuji9,
           COALESCE((b.shokuji10_nissu)::integer,
           0) AS shokuji10_nissu,
           COALESCE(b.shokuji10,
           0) AS shokuji10,
           COALESCE(b.ryoyohi_life,
           0) AS ryoyohi_life,
           COALESCE(b.skymoney_life,
           0) AS skymoney_life,
           COALESCE(b.skymoney_life_tax,
           0) AS skymoney_life_tax,
           COALESCE(b.skymoney_life_kei,
           0) AS skymoney_life_kei,
           COALESCE(b.ryoyohi_life_jihi,
           0) AS ryoyohi_life_jihi,
           COALESCE(b.skymoney_life_jihi,
           0) AS skymoney_life_jihi,
           COALESCE(b.skymoney_life_jihi_tax,
           0) AS skymoney_life_jihi_tax,
           COALESCE(b.skymoney_life_jihi_kei,
           0) AS skymoney_life_jihi_kei 
      FROM (tbl_syunou_main a 
      LEFT JOIN tbl_syunou_nyuin b 
     USING (hospnum,
           nyugaikbn,
           ptid,
           denpnum)),
           tbl_ptnum c,
           tbl_ptinf d 
     WHERE (a.hospnum  = c.hospnum ) 
       AND (a.ptid     = c.ptid    ) 
       AND (a.hospnum  = d.hospnum ) 
       AND (a.ptid     = d.ptid ) 
       AND (a.denpjtikbn  <> '3')
       AND (a.createkbn   <> '3') 
       AND (d.tstptnumkbn <> '1');


COMMENT ON VIEW view_bd002 IS '収納ビュー2';

CREATE VIEW view_bd003 AS
    SELECT a.hospnum,
           a.nyugaikbn,
           a.ptid,
           a.denpnum,
           a.denpedanum,
           a.skymoney,
           a.nyuhen_money,
           a.nyuhen_ymd,
           b.sryymd 
      FROM (tbl_syumei a JOIN tbl_syunou_main b 
     USING (hospnum,
           nyugaikbn,
           ptid,
           denpnum));


COMMENT ON VIEW view_bd003 IS '収納ビュー3';

CREATE VIEW view_i001 AS
    SELECT a.hospnum,
           a.ptid,
           b.rrknum,
           b.rrkedanum,
           e.rrkedanum_max,
           c.ptnum,
           a.kananame,
           a.name,
           a.sex,
           a.birthday,
           a.home_post,
           a.home_adrs,
           a.home_banti,
           a.deathkbn,
           b.btunum,
           b.brmnum,
           b.nyuinka,
           b.nyuinymd,
           b.taiinymd,
           b.taiincd,
           b.tennyuymd,
           b.tenstuymd,
           a.tstptnumkbn 
      FROM tbl_ptnyuinrrk b,
           tbl_ptnyuinrrk d,
           
   (SELECT tbl_ptnyuinrrk.hospnum,
           tbl_ptnyuinrrk.ptid,
           tbl_ptnyuinrrk.rrknum,
           max(tbl_ptnyuinrrk.rrkedanum) AS rrkedanum_max 
      FROM tbl_ptnyuinrrk 
     GROUP BY tbl_ptnyuinrrk.hospnum,
           tbl_ptnyuinrrk.ptid,
           tbl_ptnyuinrrk.rrknum) e,
           tbl_ptinf a,
           tbl_ptnum c 
     WHERE (b.hospnum = a.hospnum) 
       AND (b.ptid = a.ptid) 
       AND (b.hospnum = c.hospnum ) 
       AND (b.ptid = c.ptid ) 
       AND (b.hospnum = d.hospnum) 
       AND (b.ptid = d.ptid) 
       AND (a.rrknum <> 0 ) 
       AND (a.rrkedanum <> 0 ) 
       AND (a.rrknum = d.rrknum ) 
       AND (a.rrkedanum = d.rrkedanum ) 
       AND (d.kensaku_dispkbn = '1'::bpchar) 
       AND (b.jtikbn <> '5'::bpchar) 
       AND (b.jtikbn <> '6'::bpchar) 
       AND (b.hospnum = e.hospnum) 
       AND (b.ptid = e.ptid) 
       AND (b.rrknum = e.rrknum);


COMMENT ON VIEW view_i001 IS '入院患者照会ビュー';

CREATE VIEW view_q004 AS
    SELECT tbl_jyurrk.hospnum,
           tbl_jyurrk.ptid,
           tbl_jyurrk.nyugaikbn,
           tbl_jyurrk.sryka,
           tbl_jyurrk.hkncombinum,
           tbl_jyurrk.sryymd 
      FROM tbl_jyurrk 
     UNION ALL SELECT a.hospnum,
           a.ptid,
           '1' AS nyugaikbn,
           b.sryka,
           b.hkncombinum,
           a.sryymd 
      FROM tbl_nrrksrh a,
           tbl_nsrysrh b 
     WHERE (a.hospnum = b.hospnum) 
       AND (a.ptid = b.ptid) 
       AND (a.zainum = b.zainum);


COMMENT ON VIEW view_q004 IS '患者照会ビュー4';

CREATE VIEW view_q003 AS
    SELECT tbl_sryact.hospnum,
           tbl_sryact.ptid,
           tbl_sryact.nyugaikbn,
           tbl_sryact.sryka,
           tbl_sryact.srykbn,
           tbl_sryact.srysyukbn,
           tbl_sryact.srycd1,
           tbl_sryact.srycd2,
           tbl_sryact.srycd3,
           tbl_sryact.srycd4,
           tbl_sryact.srycd5 
      FROM tbl_sryact 
     UNION ALL SELECT tbl_nsrysrh.hospnum,
           tbl_nsrysrh.ptid,
           '1' AS nyugaikbn,
           tbl_nsrysrh.sryka,
           tbl_nsrysrh.srykbn,
           '   '::bpchar AS srysyukbn,
           tbl_nsrysrh.srycd AS srycd1,
           tbl_nsrysrh.srycd AS srycd2,
           tbl_nsrysrh.srycd AS srycd3,
           tbl_nsrysrh.srycd AS srycd4,
           tbl_nsrysrh.srycd AS srycd5 
      FROM tbl_nsrysrh;


COMMENT ON VIEW view_q003 IS '患者照会ビュー3';

CREATE VIEW view_q002 AS
    SELECT b.hospnum,
           b.ptid,
           b.nyugaikbn,
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
     WHERE (a.hospnum = b.hospnum) 
       AND (a.ptid    = b.ptid) 
       AND (a.nyugaikbn = b.nyugaikbn) 
       AND (a.sryka = b.sryka) 
       AND ((a.zainum = b.zainum1) 
        OR  (a.zainum = b.zainum2) 
        OR  (a.zainum = b.zainum3) 
        OR  (a.zainum = b.zainum4) 
        OR  (a.zainum = b.zainum5) 
        OR  (a.zainum = b.zainum6) 
        OR  (a.zainum = b.zainum7) 
        OR  (a.zainum = b.zainum8) 
        OR  (a.zainum = b.zainum9) 
        OR  (a.zainum = b.zainum10) 
        OR  (a.zainum = b.zainum11) 
        OR  (a.zainum = b.zainum12) 
        OR  (a.zainum = b.zainum13) 
        OR  (a.zainum = b.zainum14) 
        OR  (a.zainum = b.zainum15)) 
     UNION ALL SELECT a.hospnum,
           a.ptid,
           '1' AS nyugaikbn,
           b.sryka,
           b.srykbn,
           '   '::bpchar AS srysyukbn,
           a.sryymd,
           b.srycd AS srycd1,
           b.srycd AS srycd2,
           b.srycd AS srycd3,
           b.srycd AS srycd4,
           b.srycd AS srycd5 
      FROM tbl_nrrksrh a,
           tbl_nsrysrh b 
     WHERE (((a.hospnum = b.hospnum) 
       AND (a.ptid = b.ptid)) 
       AND (a.zainum = b.zainum));


COMMENT ON VIEW view_q002 IS '患者照会ビュー2';

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
           tbl_ptkohinf.ftnjanum,
           tbl_ptkohinf.jkysnum,
           tbl_ptkohinf.tekstymd 
      FROM (((tbl_hkncombi JOIN view_q004 
        ON ((((tbl_hkncombi.hospnum = view_q004.hospnum) 
       AND (tbl_hkncombi.ptid = view_q004.ptid)) 
       AND (tbl_hkncombi.hkncombinum = to_number((view_q004.hkncombinum)::text,
           '9999'::text))))) 
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
       AND (tbl_hkncombi.koh4hknnum = tbl_ptkohinf.kohnum)))))) 
     UNION SELECT tbl_hkncombi.hospnum,
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
           tbl_ptkohinf.ftnjanum,
           tbl_ptkohinf.jkysnum,
           tbl_ptkohinf.tekstymd 
      FROM ((view_q004 JOIN tbl_hkncombi 
        ON ((((view_q004.hospnum = tbl_hkncombi.hospnum) 
       AND (view_q004.ptid = tbl_hkncombi.ptid)) 
       AND (to_number((view_q004.hkncombinum)::text,
           '9999'::text) = tbl_hkncombi.hkncombinum)))) 
      LEFT JOIN tbl_pthkninf 
        ON (((((tbl_hkncombi.hospnum = tbl_pthkninf.hospnum) 
       AND (tbl_hkncombi.ptid = tbl_pthkninf.ptid)) 
       AND (tbl_hkncombi.hknid = tbl_pthkninf.hknid)) 
       AND (tbl_hkncombi.hknnum = tbl_pthkninf.hknnum)))),
           tbl_ptkohinf 
     WHERE (((((view_q004.hospnum = tbl_ptkohinf.hospnum) 
       AND (view_q004.ptid = tbl_ptkohinf.ptid)) 
       AND ((((((((((((((((((tbl_ptkohinf.kohnum = '956'::bpchar) 
        OR (tbl_ptkohinf.kohnum = '957'::bpchar)) 
        OR (tbl_ptkohinf.kohnum = '958'::bpchar)) 
        OR (tbl_ptkohinf.kohnum = '959'::bpchar)) 
        OR (tbl_ptkohinf.kohnum = '960'::bpchar)) 
        OR (tbl_ptkohinf.kohnum = '961'::bpchar)) 
        OR (tbl_ptkohinf.kohnum = '962'::bpchar)) 
        OR (tbl_ptkohinf.kohnum = '963'::bpchar)) 
        OR (tbl_ptkohinf.kohnum = '964'::bpchar)) 
        OR (tbl_ptkohinf.kohnum = '965'::bpchar)) 
        OR (tbl_ptkohinf.kohnum = '966'::bpchar)) 
        OR (tbl_ptkohinf.kohnum = '967'::bpchar)) 
        OR (tbl_ptkohinf.kohnum = '968'::bpchar)) 
        OR (tbl_ptkohinf.kohnum = '969'::bpchar)) 
        OR (tbl_ptkohinf.kohnum = '976'::bpchar)) 
        OR (tbl_ptkohinf.kohnum = '977'::bpchar)) 
        OR (tbl_ptkohinf.kohnum = '978'::bpchar)) 
        OR (tbl_ptkohinf.kohnum = '979'::bpchar))) 
       AND (view_q004.sryymd >= tbl_ptkohinf.tekstymd)) 
       AND (view_q004.sryymd <= tbl_ptkohinf.tekedymd));


COMMENT ON VIEW view_q001 IS '患者照会ビュー1';

