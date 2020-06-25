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
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    吉川         11/05/13  整理番号を追加
      *  01.00.02    萩野         12/07/31  就学区分別用の文字列を追加
      *  01.00.03    萩野         18/10/10  修正履歴コメント削除
      ********************************************************************
        01  SC091.
      *印刷年月日
            03  SC091-SEIYMD              PIC X(22).
      *医療機関コード
            03  SC091-HOSPCD              PIC X(14).
      *就学区分
            03  SC091-SYUGAKU             PIC X(06).
            03  SC091-SYUGAKU2            PIC X(10).

      *受給者番号
            03  SC091-JKYSNUM-TBL             OCCURS  8 TIMES.
              05  SC091-JKYSNUM           PIC X(14).
      *誕生年月日
            03  SC091-TANJOYMD-TBL            OCCURS  8 TIMES.
              05  SC091-TANJOG            PIC X(2).
              05  SC091-TANJOYMD          PIC X(12).
      *診療年月
            03  SC091-SRYYM-TBL               OCCURS  8 TIMES.
              05  SC091-SRYG              PIC X(2).
              05  SC091-SRYYM             PIC X(8).
      *氏名
            03  SC091-NAME-TBL                OCCURS  8 TIMES.
              05  SC091-NAME              PIC X(20).
      *保険者番号
            03  SC091-HKNJANUM-TBL            OCCURS  8 TIMES.
              05  SC091-HKNJANUM          PIC X(16).
      *診療科
            03  SC091-SRYKA-TBL               OCCURS  8 TIMES.
              05  SC091-SRYKA             PIC X(4).
      *入外区分
            03  SC091-NYUGAIKBN-TBL           OCCURS  8 TIMES.
              05  SC091-NYUGAIKBN         PIC X(2).
      *法別番号(公費負担の別)
            03  SC091-HBTNUM-TBL              OCCURS  8 TIMES.
              05  SC091-HBTNUM            PIC X(4).
      *長期(特定疾病の別)
            03  SC091-TYOKI-TBL               OCCURS  8 TIMES.
              05  SC091-TYOKI             PIC X(2).
      *点数
            03  SC091-TENSU-TBL               OCCURS  8 TIMES.
              05  SC091-TENSU             PIC X(14).
      *一部負担金
            03  SC091-SEIKYUGK-TBL            OCCURS  8 TIMES.
              05  SC091-SEIKYUGK          PIC X(14).
      *食事日数
            03  SC091-SNISSU-TBL              OCCURS  8 TIMES.
              05  SC091-SNISSU            PIC X(4).
      *標準負担額
            03  SC091-FTNGK-TBL               OCCURS  8 TIMES.
              05  SC091-FTNGK             PIC X(12).
      *備考
            03  SC091-BIKO-TBL                OCCURS  8 TIMES.
              05  SC091-BIKO              PIC X(40).
      *整理番号
            03  SC091-SEIRINUM-TBL            OCCURS  8 TIMES.
              05  SC091-SEIRINUM          PIC X(4).
