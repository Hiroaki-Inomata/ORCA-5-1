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
       01  SC07.
      *医療機関コード
              03  SC07-HOSPCDN         PIC  X(20).
      *診療年月
              03  SC07-PRTYM           PIC  X(16).
      *請求年月日
              03  SC07-SEIYMD          PIC  X(22).
      *点数区分表：１医科
              03  SC07-TENHYOKBN1      PIC  X(02).
      *点数区分表：３歯科
              03  SC07-TENHYOKBN2      PIC  X(02).
      *点数区分表：４調剤
              03  SC07-TENHYOKBN3      PIC  X(02).
      *点数区分表：６訪問
              03  SC07-TENHYOKBN4      PIC  X(02).
      *住所
              03  SC07-ADRS            PIC  X(100).
      *医療機関名
              03  SC07-HOSPNAME        PIC  X(100).
      *電話番号
              03  SC07-TEL             PIC  X(15).
      *管理者名
              03  SC07-KAISETUNAME     PIC  X(40).
      *負担者番号
              03  SC07-FTNJANUM1-TBL   OCCURS  10.
                  05  SC07-FTNJANUM1   PIC  X(2).
              03  SC07-FTNJANUM2-TBL   OCCURS  10.
                  05  SC07-FTNJANUM2   PIC  X(8).
      *受給者番号
              03  SC07-JKYSNUM-TBL   OCCURS  10.
                  05  SC07-JKYSNUM     PIC  X(14).
      *保険者番号
              03  SC07-HKNJANUM-TBL   OCCURS  10.
                  05  SC07-HKNJANUM    PIC  X(16).
      *受給者氏名
              03  SC07-KANANAME-TBL  OCCURS  10.
                  05  SC07-KANANAME    PIC  X(20).
              03  SC07-NAME-TBL      OCCURS  10.
                  05  SC07-NAME        PIC  X(20).
      *給付割合
              03  SC07-KYURATE1-TBL OCCURS  10.
                  05  SC07-KYURATE1  PIC  X(2).
              03  SC07-KYURATE2-TBL  OCCURS  10.
                  05  SC07-KYURATE2  PIC  X(2).
              03  SC07-KYURATE3-TBL  OCCURS  10.
                  05  SC07-KYURATE3  PIC  X(2).
      *入外区分
              03  SC07-NYU-TBL OCCURS  10.
                  05  SC07-NYUGAIKBN1  PIC  X(2).
              03  SC07-GAI-TBL  OCCURS  10.
                  05  SC07-NYUGAIKBN2  PIC  X(2).
      *診療日数
              03  SC07-NISSU-TBL  OCCURS  10.
                  05  SC07-NISSU       PIC  X(2).
      *請求額
              03  SC07-SEIKYU-TBL   OCCURS  10.
                  05  SC07-SEIKYU	PIC  X(7).
      *食事療養費薬剤負担分
              03  SC07-SYKYKZFTN-TBL     OCCURS  10.
                  05  SC07-SYKYKZFTN   PIC  X(6).
      *総点数
              03  SC07-TENSU-TBL     OCCURS  10.
                  05  SC07-TENSU       PIC  X(7).
      *公費負担金
              03  SC07-ITBFTN-TBL    OCCURS   10.
                  05  SC07-ITBFTN	PIC  X(6).
      *合計件数
              03  SC07-SKENSU		PIC  X(2).
      *合計請求額
              03  SC07-SSEIKYU		PIC  X(7).
      *食事療養・薬剤判別記号
              03  SC07-SYKYKZ-TBL OCCURS 10.
                  05  SC07-SYKYKZ       PIC  X(02).
      *備考テーブル
              03  SC07-BIKO-TBL.
      *備考文字エリア
                  05  SC07-MOJI-TBL OCCURS 10.
      *備考欄文字上段
                      07  SC07-MOJI1    PIC  X(04).
      *備考欄文字下段
                      07  SC07-MOJI2    PIC  X(04).
      *備考◯エリア
                 05  SC07-MARU-TBL OCCURS 10.
      *備考◯上段
                      07  SC07-MARU1    PIC  X(02).
      *備考◯下段
                      07  SC07-MARU2    PIC  X(02).
