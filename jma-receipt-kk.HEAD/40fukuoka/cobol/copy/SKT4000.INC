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
      *  01.00.01    吉川    2011/02/09  特別療養費国保対応
      *  01.00.02    吉川    2014/11/10  標榜科名欄の文字数と行数追加
      *  01.00.03    萩野    2015/09/14  総合計欄の点数の桁数を修正
      *  01.00.04    萩野    2015/11/13  点数欄の点数の桁数を修正
      *  01.00.05    門間    2019/01/10  修正履歴コメント削除
      ********************************************************************
        01  SKT4000.
            02  SKT4000-SRYM    PIC X(6).
            02  SKT4000-SRYKA
                    PIC X(22)   OCCURS  4 TIMES.
            02  SKT4000-POST    PIC X(7).
            02  SKT4000-ADRS    PIC X(100).
            02  SKT4000-TEL PIC X(15).
            02  SKT4000-HOSPNAME    PIC X(100).
            02  SKT4000-KANRINAME   PIC X(40).
            02  SKT4000-HOSPCD  PIC X(20).
            02  SKT4000-TOKUBETU    PIC X(10).
            02  SKT4000-SEIKYU          OCCURS  8 TIMES.
              03  SKT4000-HKNJANUM    PIC X(12).
              03  SKT4000-KO-HKNJANUM PIC X(16).
              03  SKT4000-NYUGAIKBN       OCCURS  2 TIMES.
                04  SKT4000-KKH-KENSU   PIC X(5).
                04  SKT4000-KKH-TENSU   PIC X(10).
                04  SKT4000-TAK-KENSU   PIC X(5).
                04  SKT4000-TAK-TENSU   PIC X(10).
                04  SKT4000-RJN-KENSU   PIC X(5).
                04  SKT4000-RJN-TENSU   PIC X(10).
                04  SKT4000-KO-KENSU    PIC X(5).
                04  SKT4000-KO-TENSU    PIC X(10).
            02  SKT4000-SSEIKYU         OCCURS  2 TIMES.
              03  SKT4000-KKH-SKENSU  PIC X(5).
              03  SKT4000-KKH-STENSU  PIC X(10).
              03  SKT4000-TAK-SKENSU  PIC X(5).
              03  SKT4000-TAK-STENSU  PIC X(10).
              03  SKT4000-RJN-SKENSU  PIC X(5).
              03  SKT4000-RJN-STENSU  PIC X(10).
              03  SKT4000-KO-SKENSU   PIC X(5).
              03  SKT4000-KO-STENSU   PIC X(10).
      *
            02  SKT4000-SUM.
              03  SKT4000-N-GKENSU    PIC X(5).
              03  SKT4000-N-GTENSU    PIC X(10).
              03  SKT4000-N-AVG   PIC X(9).
              03  SKT4000-G-GKENSU    PIC X(5).
              03  SKT4000-G-GTENSU    PIC X(10).
              03  SKT4000-G-AVG   PIC X(9).
              03  SKT4000-N-SGKENSU   PIC X(5).
              03  SKT4000-N-SGTENSU   PIC X(10).
              03  SKT4000-G-SGKENSU   PIC X(5).
              03  SKT4000-G-SGTENSU   PIC X(10).
              03  SKT4000-KOH-GKENSU  PIC X(5).
              03  SKT4000-KOH-GTENSU  PIC X(10).
