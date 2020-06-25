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
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者  日付        内容
      *  01.00.01    門間    2019/05/21  改元対応
      ********************************************************************
        01  SC09.
      *印刷年月
            03  SC09-PRTGG          PIC X(04).
            03  SC09-PRTYY          PIC X(04).
            03  SC09-PRTMM          PIC X(04).
      *医療機関コード
            03  SC09-HOSPCD         PIC X(14).
      *市町村番号
            03  SC09-CITYNUM        PIC X(12).
      *市町村名
            03  SC09-CITYNAME       PIC X(20).
      *医療機関住所
            03  SC09-ADRS           PIC X(100).
      *医療機関名称
            03  SC09-HOSPNAME       PIC X(100).
      *電話番号
            03  SC09-TEL            PIC X(15).
      *開設者名
            03  SC09-KAISETU        PIC X(40).
      *ページ数
            03  SC09-PAGE           PIC X(10).
      *受給者番号
            03  SC09-JKYSNUM-TBL        OCCURS  7 TIMES.
                05  SC09-JKYSNUM    PIC X(20).
      *性別
            03  SC09-SEX-TBL            OCCURS  7 TIMES.
                05  SC09-SEX        PIC X(02).
      *誕生年月日
            03  SC09-TANJOYMD-TBL       OCCURS  7 TIMES.
                05  SC09-TANJOG     PIC X(2).
                05  SC09-TANJOYMD   PIC X(12).
      *診療年月
            03  SC09-SRYYY-TBL          OCCURS  7 TIMES.
                05  SC09-SRYYY      PIC X(04).
            03  SC09-SRYMM-TBL          OCCURS  7 TIMES.
                05  SC09-SRYMM      PIC X(04).
      *カナ氏名
            03  SC09-KANANAME-TBL       OCCURS  7 TIMES.
                05  SC09-KANANAME   PIC X(30).
      *保険者番号
            03  SC09-HKNJANUM-TBL       OCCURS  7 TIMES.
                05  SC09-HKNJANUM   PIC X(16).
      *診療科
            03  SC09-SRYKA-TBL          OCCURS  7 TIMES.
                05  SC09-SRYKA      PIC X(04).
      *入外区分
            03  SC09-NYUGAIKBN-TBL      OCCURS  7 TIMES.
                05  SC09-NYUGAIKBN  PIC X(02).
      *法別番号
            03  SC09-HBTNUM-TBL         OCCURS  7 TIMES.
                05  SC09-HBTNUM     PIC X(04).
      *長期
            03  SC09-TYOKI-TBL          OCCURS  7 TIMES.
                05  SC09-TYOKI      PIC X(02).
      *日数
            03  SC09-NISSU-TBL          OCCURS  7 TIMES.
                05  SC09-NISSU      PIC X(04).
      *点数
            03  SC09-TENSU-TBL          OCCURS  7 TIMES.
                05  SC09-TENSU      PIC X(14).
      *一部負担金
            03  SC09-SEIKYUGK-TBL       OCCURS  7 TIMES.
                05  SC09-SEIKYUGK   PIC X(14).
      *食事日数
            03  SC09-SNISSU-TBL         OCCURS  7 TIMES.
                05  SC09-SNISSU     PIC X(04).
      *標準負担額
            03  SC09-FTNGK-TBL          OCCURS  7 TIMES.
                05  SC09-FTNGK      PIC X(14).
      *備考
            03  SC09-BIKO-TBL           OCCURS  7 TIMES.
                05  SC09-BIKO       PIC X(40).
      *小計
            03  SC09-GTENSU         PIC X(14).
            03  SC09-GSEIKYUGK      PIC X(14).
            03  SC09-GFTNGK         PIC X(14).
