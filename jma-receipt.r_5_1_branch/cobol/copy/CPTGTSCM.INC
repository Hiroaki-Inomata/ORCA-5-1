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
      *****************************************************************
      *    データ出力対象スキーマ一覧  COPY (CPTGTSCM.INC)            *
      *****************************************************************
      *****************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
       01  TGTSCM-AREA.
           03  TGTSCM-VAL.
               05  PIC X(23)   VALUE "001tbl_syskanri".
               05  PIC X(23)   VALUE "002tbl_byomei".
               05  PIC X(23)   VALUE "003tbl_chk".
               05  PIC X(23)   VALUE "004tbl_inputcd".
               05  PIC X(23)   VALUE "005tbl_hkncombi".
               05  PIC X(23)   VALUE "006tbl_hknnum".
               05  PIC X(23)   VALUE "007tbl_inputset".
               05  PIC X(23)   VALUE "008tbl_adrs".
               05  PIC X(23)   VALUE "009tbl_jyurrk".
               05  PIC X(23)   VALUE "010tbl_srykarrk".
               05  PIC X(23)   VALUE "011tbl_ptbyomei".
               05  PIC X(23)   VALUE "012tbl_ptcom".
               05  PIC X(23)   VALUE "013tbl_ptnum".
               05  PIC X(23)   VALUE "014tbl_pthkninf".
               05  PIC X(23)   VALUE "015tbl_ptkohinf".
               05  PIC X(23)   VALUE "016tbl_ptinf".
               05  PIC X(23)   VALUE "017tbl_kyuseirrk".
               05  PIC X(23)   VALUE "018tbl_sryacct".
               05  PIC X(23)   VALUE "019tbl_sryact".
               05  PIC X(23)   VALUE "020tbl_syumei".
               05  PIC X(23)   VALUE "021tbl_syunou".
               05  PIC X(23)   VALUE "022tbl_tsyrrk".
               05  PIC X(23)   VALUE "023tbl_tensu".
               05  PIC X(23)   VALUE "024tbl_uketuke".
               05  PIC X(23)   VALUE "025tbl_yyk".
               05  PIC X(23)   VALUE "026tbl_sykrrk".
               05  PIC X(23)   VALUE "027tbl_tnkrrk".
               05  PIC X(23)   VALUE "092tbl_wksryact".
               05  PIC X(23)   VALUE "028tbl_santei".
               05  PIC X(23)   VALUE "093tbl_para".
               05  PIC X(23)   VALUE "029tbl_receprt".
               05  PIC X(23)   VALUE "033tbl_srycdchg ".
               05  PIC X(23)   VALUE "031tbl_tensuplus".
               05  PIC X(23)   VALUE "032tbl_hknjainf".
               05  PIC X(23)   VALUE "030tbl_seikyu".
               05  PIC X(23)   VALUE "000view_bd001".
               05  PIC X(23)   VALUE "000view_bd002".
               05  PIC X(23)   VALUE "034tbl_userbyomei".
               05  PIC X(23)   VALUE "035tbl_jobkanri".
               05  PIC X(23)   VALUE "037tbl_rececom".
               05  PIC X(23)   VALUE "036tbl_example".
               05  PIC X(23)   VALUE "038tbl_dbkanri".
               05  PIC X(23)   VALUE "039tbl_recekanri".
               05  PIC X(23)   VALUE "040tbl_ptrsiinf".
               05  PIC X(23)   VALUE "041tbl_prtkanri".
               05  PIC X(23)   VALUE "042tbl_prtdata".
               05  PIC X(23)   VALUE "043tbl_interact".
               05  PIC X(23)   VALUE "044tbl_sskijyo".
               05  PIC X(23)   VALUE "045tbl_jinmei".
               05  PIC X(23)   VALUE "046tbl_monthlynum".
               05  PIC X(23)   VALUE "000view_Q001".
               05  PIC X(23)   VALUE "052tbl_yakujyo".
               05  PIC X(23)   VALUE "055tbl_nyuinkhn".
               05  PIC X(23)   VALUE "058tbl_ptnyuinrrk".
               05  PIC X(23)   VALUE "059tbl_pttainfuka".
               05  PIC X(23)   VALUE "053tbl_nyuinacct".
               05  PIC X(23)   VALUE "054tbl_nyuinact".
               05  PIC X(23)   VALUE "000view_I001".
               05  PIC X(23)   VALUE "057tbl_nyuksnchk".
               05  PIC X(23)   VALUE "056tbl_nyuinryakusho".
               05  PIC X(23)   VALUE "062tbl_teikikanri".
               05  PIC X(23)   VALUE "063tbl_teikiprt".
               05  PIC X(23)   VALUE "064tbl_teikirrk".
               05  PIC X(23)   VALUE "060tbl_ptteikirrk".
               05  PIC X(23)   VALUE "048tbl_kohsky".
               05  PIC X(23)   VALUE "049tbl_ldmap".
               05  PIC X(23)   VALUE "050tbl_lock".
               05  PIC X(23)   VALUE "047tbl_kensasort".
               05  PIC X(23)   VALUE "061tbl_senteicdchg".
               05  PIC X(23)   VALUE "051tbl_ptkohftn".
               05  PIC X(23)   VALUE "000view_Q002".
               05  PIC X(23)   VALUE "065tbl_receden".
               05  PIC X(23)   VALUE "066tbl_kohplus".
               05  PIC X(23)   VALUE "067tbl_prvkanri".
               05  PIC X(23)   VALUE "000view_bd003".
               05  PIC X(23)   VALUE "068tbl_syuday".
               05  PIC X(23)   VALUE "069tbl_pgkanri".
               05  PIC X(23)   VALUE "070tbl_opecom".
               05  PIC X(23)   VALUE "071tbl_syutotal".
               05  PIC X(23)   VALUE "072tbl_toukeimemo".
               05  PIC X(23)   VALUE "073tbl_btpara".
               05  PIC X(23)   VALUE "074tbl_order".
               05  PIC X(23)   VALUE "075tbl_help".
               05  PIC X(23)   VALUE "076tbl_chksnd".
               05  PIC X(23)   VALUE "077tbl_receupd".
               05  PIC X(23)   VALUE "078tbl_orderprt".
               05  PIC X(23)   VALUE "080tbl_sryacctplus".
               05  PIC X(23)   VALUE "081tbl_jyurrk_prv".
               05  PIC X(23)   VALUE "082tbl_sryacct_prv".
               05  PIC X(23)   VALUE "083tbl_syumei_prv".
               05  PIC X(23)   VALUE "084tbl_ptcom_prv".
               05  PIC X(23)   VALUE "085tbl_sryacctplus_prv".
               05  PIC X(23)   VALUE "086tbl_syunou_prv".
               05  PIC X(23)   VALUE "087tbl_ptinf_prv".
               05  PIC X(23)   VALUE "088tbl_sryact_prv".
               05  PIC X(23)   VALUE "089tbl_syutotal_prv".
               05  PIC X(23)   VALUE "090tbl_santei_prv".
               05  PIC X(23)   VALUE "091tbl_srykarrk_prv".
               05  PIC X(23)   VALUE "094tbl_hours".
               05  PIC X(23)   VALUE "095tbl_tekioubyomei".
               05  PIC X(23)   VALUE "096tbl_chktrd".
               05  PIC X(23)   VALUE "097tbl_syuka".
               05  PIC X(23)   VALUE "098tbl_chk004".
               05  PIC X(23)   VALUE "099tbl_odritem".
               05  PIC X(23)   VALUE "100tbl_ryocom".
               05  PIC X(23)   VALUE "101tbl_joblog".
               05  PIC X(23)   VALUE "102tbl_chk005".
               05  PIC X(23)   VALUE "103tbl_genericname".
               05  PIC X(23)   VALUE "104tbl_hktsantei".
               05  PIC X(23)   VALUE "105tbl_hktsrycd".
               05  PIC X(23)   VALUE "106tbl_qrcdhkn".
               05  PIC X(23)   VALUE "107tbl_rece_putb".
               05  PIC X(23)   VALUE "108tbl_rece_puth".
               05  PIC X(23)   VALUE "109tbl_syurrk".
               05  PIC X(23)   VALUE "110tbl_tensuplus2".
               05  PIC X(23)   VALUE "111tbl_clm_uid".
               05  PIC X(23)   VALUE "112tbl_csv_info".
               05  PIC X(23)   VALUE "113tbl_file_info".
               05  PIC X(23)   VALUE "114tbl_mstkanri".
               05  PIC X(23)   VALUE "115tbl_ptmemoinf".
               05  PIC X(23)   VALUE "116tbl_shoukaidata".
               05  PIC X(23)   VALUE "117tbl_shoukaikanri".
               05  PIC X(23)   VALUE "118tbl_sysbase".
               05  PIC X(23)   VALUE "119tbl_sysuser".
               05  PIC X(23)   VALUE "120tbl_yakujyo_temp".
           03  TGTSCM-R     REDEFINES TGTSCM-VAL.
               05  TGTSCM-OCC  OCCURS  125.
                   07  TGTSCM-NO     PIC 9(03).
                   07  TGTSCM-ID     PIC X(20).
           03  TGTSCM-MAX      PIC 9(05)   VALUE 125.
