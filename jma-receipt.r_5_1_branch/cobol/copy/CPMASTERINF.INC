      *******************************************************************
      * Project code name "ORCA"
      * 日医標準レセプトソフト（JMA standard receipt software）
      * Copyright(C) 2002 JMA (Japan Medical Association)
      *
      * This program is part of "JMA standard receipt software".
      *
      *     This program is distributed in the hope that it will be useful
      * for further advancement in medical care, according to JMA Open
      * Source License, but WITHOUT ANY WARRANTY.
      *     Everyone is granted permission to use, copy, modify and
      * redistribute this program, but only under the conditions described
      * in the JMA Open Source License. You should have received a copy of
      * this license along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
      *    マスタ取得対象
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  MASTERINF-AREA.
           03  MASTERINF-VAL.
      *
               05 PIC X(50) VALUE  "tbl_adrs".
               05 PIC X(50) VALUE  "住所".
               05 PIC X(01) VALUE  "0".
      *
               05 PIC X(50) VALUE  "tbl_byomei".
               05 PIC X(50) VALUE  "病名".
               05 PIC X(01) VALUE  "1".
      *
               05 PIC X(50) VALUE  "tbl_chk".
               05 PIC X(50) VALUE  "チェック".
               05 PIC X(01) VALUE  "1".
      *
               05 PIC X(50) VALUE  "tbl_chksnd".
               05 PIC X(50) VALUE  "チェック２".
               05 PIC X(01) VALUE  "1".
      *
               05 PIC X(50) VALUE  "tbl_etensu_1".
               05 PIC X(50) VALUE  "電子点数表１".
               05 PIC X(01) VALUE  "0".
      *
               05 PIC X(50) VALUE  "tbl_etensu_2_jma".
               05 PIC X(50) VALUE  "電子点数表２追加".
               05 PIC X(01) VALUE  "0".
      *
               05 PIC X(50) VALUE  "tbl_etensu_2_sample".
               05 PIC X(50) VALUE  "電子点数表２検体".
               05 PIC X(01) VALUE  "0".
      *
               05 PIC X(50) VALUE  "tbl_etensu_2".
               05 PIC X(50) VALUE  "電子点数表２".
               05 PIC X(01) VALUE  "0".
      *
               05 PIC X(50) VALUE  "tbl_etensu_3_1".
               05 PIC X(50) VALUE  "電子点数表３−１".
               05 PIC X(01) VALUE  "0".
      *
               05 PIC X(50) VALUE  "tbl_etensu_3_2".
               05 PIC X(50) VALUE  "電子点数表３−２".
               05 PIC X(01) VALUE  "0".
      *
               05 PIC X(50) VALUE  "tbl_etensu_3_3".
               05 PIC X(50) VALUE  "電子点数表３−３".
               05 PIC X(01) VALUE  "0".
      *
               05 PIC X(50) VALUE  "tbl_etensu_3_4".
               05 PIC X(50) VALUE  "電子点数表３−４".
               05 PIC X(01) VALUE  "0".
      *
               05 PIC X(50) VALUE  "tbl_etensu_4".
               05 PIC X(50) VALUE  "電子点数表４".
               05 PIC X(01) VALUE  "0".
      *
               05 PIC X(50) VALUE  "tbl_etensu_5".
               05 PIC X(50) VALUE  "電子点数表５".
               05 PIC X(01) VALUE  "0".
      *
               05 PIC X(50) VALUE  "tbl_example".
               05 PIC X(50) VALUE  "雛形".
               05 PIC X(01) VALUE  "1".
      *
               05 PIC X(50) VALUE  "tbl_generic_class".
               05 PIC X(50) VALUE  "医薬品分類".
               05 PIC X(01) VALUE  "1".
      *
               05 PIC X(50) VALUE  "tbl_generic_price".
               05 PIC X(50) VALUE  "最低薬価".
               05 PIC X(01) VALUE  "1".
      *
               05 PIC X(50) VALUE  "tbl_genericname".
               05 PIC X(50) VALUE  "一般名".
               05 PIC X(01) VALUE  "1".
      *
               05 PIC X(50) VALUE  "tbl_hknjainf_care".
               05 PIC X(50) VALUE  "介護保険者情報".
               05 PIC X(01) VALUE  "1".
      *
               05 PIC X(50) VALUE  "tbl_hknjainf".
               05 PIC X(50) VALUE  "保険者情報".
               05 PIC X(01) VALUE  "1".
      *
               05 PIC X(50) VALUE  "tbl_hknnum".
               05 PIC X(50) VALUE  "保険番号".
               05 PIC X(01) VALUE  "1".
      *
               05 PIC X(50) VALUE  "tbl_hktsantei".
               05 PIC X(50) VALUE  "包括チェック".
               05 PIC X(01) VALUE  "1".
      *
               05 PIC X(50) VALUE  "tbl_hktsrycd".
               05 PIC X(50) VALUE  "包括診療コード".
               05 PIC X(01) VALUE  "1".
      *
               05 PIC X(50) VALUE  "tbl_infection_judgment".
               05 PIC X(50) VALUE  "感染症判定".
               05 PIC X(01) VALUE  "1".
      *
               05 PIC X(50) VALUE  "tbl_interact".
               05 PIC X(50) VALUE  "相互作用".
               05 PIC X(01) VALUE  "1".
      *
               05 PIC X(50) VALUE  "tbl_kensasort".
               05 PIC X(50) VALUE  "検査分類".
               05 PIC X(01) VALUE  "1".
      *
               05 PIC X(50) VALUE  "tbl_labor_sio".
               05 PIC X(50) VALUE  "労働基準監督署".
               05 PIC X(01) VALUE  "1".
      *
               05 PIC X(50) VALUE  "tbl_material_h".
               05 PIC X(50) VALUE  "特定器材品目".
               05 PIC X(01) VALUE  "1".
      *
               05 PIC X(50) VALUE  "tbl_material_k".
               05 PIC X(50) VALUE  "特材品目算定関連手技".
               05 PIC X(01) VALUE  "1".
      *
               05 PIC X(50) VALUE  "tbl_material_s".
               05 PIC X(50) VALUE  "特材品目算定適応疾患".
               05 PIC X(01) VALUE  "1".
      *
               05 PIC X(50) VALUE  "tbl_nyuincdchg".
               05 PIC X(50) VALUE  "入院料コード置換".
               05 PIC X(01) VALUE  "1".
      *
               05 PIC X(50) VALUE  "tbl_nyuinkhn".
               05 PIC X(50) VALUE  "入院基本料".
               05 PIC X(01) VALUE  "1".
      *
               05 PIC X(50) VALUE  "tbl_nyuinryakusho".
               05 PIC X(50) VALUE  "入院レセプト記載略称".
               05 PIC X(01) VALUE  "1".
      *
               05 PIC X(50) VALUE  "tbl_nyuksnchk".
               05 PIC X(50) VALUE  "入院料加算チェック".
               05 PIC X(01) VALUE  "1".
      *
               05 PIC X(50) VALUE  "tbl_prgoption".
               05 PIC X(50) VALUE  "プログラムオプション".
               05 PIC X(01) VALUE  "1".
      *
               05 PIC X(50) VALUE  "tbl_psychotropic".
               05 PIC X(50) VALUE  "向精神薬成分コード".
               05 PIC X(01) VALUE  "1".
      *
               05 PIC X(50) VALUE  "tbl_recekisai".
               05 PIC X(50) VALUE  "レセプト記載事項".
               05 PIC X(01) VALUE  "1".
      *
               05 PIC X(50) VALUE  "tbl_senteicdchg".
               05 PIC X(50) VALUE  "選定療養費・一般コード振替".
               05 PIC X(01) VALUE  "1".
      *
               05 PIC X(50) VALUE  "tbl_srycdchg".
               05 PIC X(50) VALUE  "一般老人置換".
               05 PIC X(01) VALUE  "1".
      *
               05 PIC X(50) VALUE  "tbl_sskijyo".
               05 PIC X(50) VALUE  "症状措置機序".
               05 PIC X(01) VALUE  "1".
      *
               05 PIC X(50) VALUE  "tbl_synonym".
               05 PIC X(50) VALUE  "同義語".
               05 PIC X(01) VALUE  "1".
      *
               05 PIC X(50) VALUE  "tbl_syskanri".
               05 PIC X(50) VALUE  "システム管理".
               05 PIC X(01) VALUE  "1".
      *
               05 PIC X(50) VALUE  "tbl_tekiou_disease".
               05 PIC X(50) VALUE  "医薬品傷病名ＤＢ適応傷病名リンク".
               05 PIC X(01) VALUE  "1".
      *
               05 PIC X(50) VALUE  "tbl_tekiou_medicine".
               05 PIC X(50) VALUE  "医薬品傷病名ＤＢ医薬品".
               05 PIC X(01) VALUE  "1".
      *
               05 PIC X(50) VALUE  "tbl_tekioubyomei".
               05 PIC X(50) VALUE  "適応病名".
               05 PIC X(01) VALUE  "1".
      *
               05 PIC X(50) VALUE  "tbl_tensuplus".
               05 PIC X(50) VALUE  "点数付加".
               05 PIC X(01) VALUE  "1".
      *
               05 PIC X(50) VALUE  "tbl_tensuplus2".
               05 PIC X(50) VALUE  "点数付加２".
               05 PIC X(01) VALUE  "1".
      *
               05 PIC X(50) VALUE  "tbl_tensu".
               05 PIC X(50) VALUE  "点数".
               05 PIC X(01) VALUE  "1".
      *
               05 PIC X(50) VALUE  "tbl_toukeimemo".
               05 PIC X(50) VALUE  "統計覚書".
               05 PIC X(01) VALUE  "1".
      *
               05 PIC X(50) VALUE  "tbl_yakujyo".
               05 PIC X(50) VALUE  "薬剤情報".
               05 PIC X(01) VALUE  "1".
      *
           03  MASTERINF-R  REDEFINES  MASTERINF-VAL.
               05  MASTERINF-OCC   OCCURS    50.
                   07  MASTERINF-ID    PIC X(50).
                   07  MASTERINF-NAME  PIC X(50).
                   07  MASTERINF-EXISTS-UPYMD
                                       PIC X(1).
           03  MASTERINF-MAX       PIC 9(03)
                                   VALUE     50.
