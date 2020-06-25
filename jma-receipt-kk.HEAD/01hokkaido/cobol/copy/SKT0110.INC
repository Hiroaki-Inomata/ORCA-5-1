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
      *  01.00.02    萩野    2018/09/10  修正履歴コメント削除
      ********************************************************************
        01  SKT0110.
            03  SKT0110-NO              PIC X(04).
            03  SKT0110-SRYYM           PIC X(16).
            03  SKT0110-HKNJANUM        PIC X(12).
            03  SKT0110-HKNJANAME       PIC X(30).
            03  SKT0110-HKNJANAME2      PIC X(12).
            03  SKT0110-SEIYMD          PIC X(22).
            03  SKT0110-HOSPCD          PIC X(07).
            03  SKT0110-ADRS            PIC X(100).
            03  SKT0110-HOSPNAME        PIC X(100).
            03  SKT0110-KAISETUNAME     PIC X(40).
            03  SKT0110-TEL             PIC X(15).
            03  SKT0110-ZAI1            PIC X(02).
            03  SKT0110-ZAI2            PIC X(02).
            03  SKT0110-TOKUBETU        PIC X(10).
            03  SKT0110-SEIKYU          OCCURS  10 TIMES.
                05  SKT0110-NYUGAI-TBL  OCCURS  2 TIMES.
                    07  SKT0110-KENSU   PIC X(05).
                    07  SKT0110-NISSU   PIC X(05).
                    07  SKT0110-TENSU   PIC X(09).
                    07  SKT0110-ITBFTN  PIC X(09).
                05  SKT0110-SKENSU      PIC X(05).
                05  SKT0110-SNISSU      PIC X(05).
                05  SKT0110-SKINGK      PIC X(09).
                05  SKT0110-SFTNGK      PIC X(09).
            03  SKT0110-KYURATE-TBL     OCCURS  2 TIMES.
                05  SKT0110-KYURATE     PIC X(02).
