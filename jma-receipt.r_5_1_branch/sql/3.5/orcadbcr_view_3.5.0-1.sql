\set ON_ERROR_STOP

CREATE VIEW "view_bd001" as SELECT a.hospnum,
           a.ptid,
           b.ptnum,
           c.name,
           c.sex,
           c.birthday,
           a.sryka,
           a.denpprtymd,
           a.skymoney,
           a.nyukin_total 
      FROM tbl_syunou a,
           tbl_ptnum b,
           tbl_ptinf c 
     WHERE ((((((((a.hospnum = b.hospnum) 
       AND (a.ptid = b.ptid)) 
       AND (a.hospnum = c.hospnum)) 
       AND (a.ptid = c.ptid)) 
       AND (a.denpjtikbn <> '3'::bpchar)) 
       AND (a.denpjtikbn <> '7'::bpchar)) 
       AND (a.createkbn <> '3'::bpchar)) 
       AND (c.tstptnumkbn <> '1'::bpchar));

COMMENT ON VIEW "view_bd001" IS '収納ビュー';

CREATE VIEW "view_bd002" as SELECT a.hospnum,
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
           a.roomnum,
           a.byotonum,
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
           a.msi_hknten,
           a.msi_money,
           a.msi_tgmoney,
           a.msi_tgmoney_tax,
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
           a.nyn_hknten,
           a.nyn_money,
           a.nyn_tgmoney,
           a.nyn_tgmoney_tax,
           a.total_hknten,
           a.total_money,
           a.total_tgmoney,
           a.total_tgmoney_tax,
           a.shohou_sai,
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
           a.rmsagaku,
           a.rmsagaku_tax_sai,
           a.shokuji1_nissu,
           a.shokuji1,
           a.shokuji2_nissu,
           a.shokuji2,
           a.shokuji3_nissu,
           a.shokuji3,
           a.shokuji4_nissu,
           a.shokuji4,
           a.shokuji5_nissu,
           a.shokuji5,
           a.shokuji6_nissu,
           a.shokuji6,
           a.shokuji7_nissu,
           a.shokuji7,
           a.syuskj_nissu,
           a.syuskj_ryoyohi,
           a.syuskj_ftn,
           a.syuskj_ftn_rece,
           a.syuskj_ftnkbn,
           a.koh1skj_nissu,
           a.koh1skj_ryoyohi,
           a.koh1skj_ftn,
           a.koh1skj_ftn_rece,
           a.koh1skj_ftnkbn,
           a.koh2skj_nissu,
           a.koh2skj_ryoyohi,
           a.koh2skj_ftn,
           a.koh2skj_ftn_rece,
           a.koh2skj_ftnkbn,
           a.koh3skj_nissu,
           a.koh3skj_ryoyohi,
           a.koh3skj_ftn,
           a.koh3skj_ftn_rece,
           a.koh3skj_ftnkbn,
           a.koh4skj_nissu,
           a.koh4skj_ryoyohi,
           a.koh4skj_ftn,
           a.koh4skj_ftn_rece,
           a.koh4skj_ftnkbn,
           a.ryoyohi_skj,
           a.skymoney_skj,
           a.skymoney_skj_tax,
           a.skymoney_skj_kei,
           a.ryoyohi_skj_jihi,
           a.skymoney_skj_jihi,
           a.skymoney_skj_jihi_tax,
           a.skymoney_skj_jihi_kei,
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
           a.day_1,
           a.day_2,
           a.day_3,
           a.day_4,
           a.day_5,
           a.day_6,
           a.day_7,
           a.day_8,
           a.day_9,
           a.day_10,
           a.day_11,
           a.day_12,
           a.day_13,
           a.day_14,
           a.day_15,
           a.day_16,
           a.day_17,
           a.day_18,
           a.day_19,
           a.day_20,
           a.day_21,
           a.day_22,
           a.day_23,
           a.day_24,
           a.day_25,
           a.day_26,
           a.day_27,
           a.day_28,
           a.day_29,
           a.day_30,
           a.day_31,
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
           a.nyuin_rrknum,
           a.acct_updkbn,
           a.jyo_hknftnmoney,
           a.jyo_compftn,
           a.jyo_compftn_entani,
           a.termid,
           a.opid,
           a.creymd,
           a.upymd,
           a.uphms,
           b.ptnum 
      FROM tbl_syunou a,
           tbl_ptnum b,
           tbl_ptinf c 
     WHERE (((((((b.hospnum = a.hospnum) 
       AND (b.ptid = a.ptid)) 
       AND (b.hospnum = c.hospnum)) 
       AND (b.ptid = c.ptid)) 
       AND (a.denpjtikbn <> '3'::bpchar)) 
       AND (a.createkbn <> '3'::bpchar)) 
       AND (c.tstptnumkbn <> '1'::bpchar));

COMMENT ON VIEW "view_bd002" IS '収納ビュー2';

CREATE VIEW "view_bd003" as SELECT a.hospnum,
           a.nyugaikbn,
           a.ptid,
           a.denpnum,
           a.denpedanum,
           a.skymoney,
           a.nyuhen_money,
           a.nyuhen_ymd,
           b.sryymd 
      FROM (tbl_syumei a JOIN tbl_syunou b 
     USING (hospnum,
           nyugaikbn,
           ptid,
           denpnum));

COMMENT ON VIEW "view_bd003" IS '収納ビュー3';

CREATE VIEW "view_i001" as SELECT a.hospnum,
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
     WHERE ((((((((((((((((b.hospnum = a.hospnum) 
       AND (b.ptid = a.ptid)) 
       AND (b.hospnum = c.hospnum)) 
       AND (b.ptid = c.ptid)) 
       AND (b.hospnum = d.hospnum)) 
       AND (b.ptid = d.ptid)) 
       AND (a.rrknum <> '0'::"numeric")) 
       AND (a.rrkedanum <> '0'::"numeric")) 
       AND (a.rrknum = d.rrknum)) 
       AND (a.rrkedanum = d.rrkedanum)) 
       AND (d.kensaku_dispkbn = '1'::bpchar)) 
       AND (b.jtikbn <> '5'::bpchar)) 
       AND (b.jtikbn <> '6'::bpchar)) 
       AND (b.hospnum = e.hospnum)) 
       AND (b.ptid = e.ptid)) 
       AND (b.rrknum = e.rrknum));

COMMENT 
        ON VIEW "view_i001" IS '入院患者照会ビュー';

CREATE VIEW "view_q001" as SELECT tbl_hkncombi.hospnum,
           tbl_hkncombi.ptid,
           tbl_hkncombi.hkncombinum,
           tbl_jyurrk.nyugaikbn,
           tbl_jyurrk.sryka,
           tbl_jyurrk.sryymd,
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
      FROM (((tbl_hkncombi JOIN tbl_jyurrk 
        ON ((((tbl_hkncombi.hospnum = tbl_jyurrk.hospnum) 
       AND (tbl_hkncombi.ptid = tbl_jyurrk.ptid)) 
       AND (tbl_hkncombi.hkncombinum = to_number((tbl_jyurrk.hkncombinum)::text,
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
       AND (tbl_hkncombi.koh4hknnum = tbl_ptkohinf.kohnum))))));


COMMENT ON VIEW "view_q001" IS '患者照会ビュー';

CREATE VIEW "view_q002" as SELECT a.hospnum,
           a.ptid,
           a.nyugaikbn,
           a.sryka,
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
        OR (a.zainum = b.zainum15)));

COMMENT ON VIEW "view_q002" IS '患者照会ビュー2';

