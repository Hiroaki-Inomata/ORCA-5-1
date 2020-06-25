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
      *  01.00.01    吉川      2010/11/02   特別療養費国保対応
      *  01.00.02    吉川      2014/11/12   県外保険者名のバイト数増加
      *  01.00.03    萩野      2019/03/08   修正履歴コメント削除
      ********************************************************************
        01  SKT2800.
            02  SKT2800-SRYYM   PIC X(16).
            02  SKT2800-MARU1   PIC X(2).
            02  SKT2800-MARU2   PIC X(2).
            02  SKT2800-SRYKAMEI
                    PIC X(34)   OCCURS  3 TIMES.
            02  SKT2800-HOSPCD  PIC X(20).
            02  SKT2800-POST    PIC X(16).
            02  SKT2800-ADRS    PIC X(100).
            02  SKT2800-HOSPNAME    PIC X(100).
            02  SKT2800-KAISETUNAME PIC X(40).
            02  SKT2800-TEL PIC X(15).
            02  SKT2800-HOSPKBN
                    PIC X(2)    OCCURS  4 TIMES.
            02  SKT2800-KENSU-TBL       OCCURS  10 TIMES.
              03  SKT2800-KENSU   PIC X(12).
            02  SKT2800-TENSU-TBL       OCCURS  10 TIMES.
              03  SKT2800-TENSU   PIC X(18).
            02  SKT2800-SEIKYUSYO-TBL           OCCURS  6 TIMES.
              03  SKT2800-SEIKYUSYO   PIC X(6).
            02  SKT2800-SYAHOSEIKYU PIC X(6).
            02  SKT2800-KGIHKNJANAME-TBL        OCCURS  12 TIMES.
              03  SKT2800-KGIHKNJANAME    PIC X(24).
            02  SKT2800-KGIKOUKINAME-TBL        OCCURS  12 TIMES.
              03  SKT2800-KGIKOUKINAME    PIC X(24).
            02  SKT2800-TOKUBETU     PIC  X(10).
