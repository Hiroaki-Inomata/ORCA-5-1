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
      *  01.00.01    吉川    2010/04/02  特別療養費国保対応
      *  01.00.02    門間    2018/11/01  修正履歴コメント削除
      ********************************************************************
        01  SKT1220.
            03  SKT1220-SRYYM                        PIC X(16).
            03  SKT1220-SEIYMD                       PIC X(22).
            03  SKT1220-ADRS                         PIC X(100).
            03  SKT1220-HOSPNAME                     PIC X(100).
            03  SKT1220-TEL                          PIC X(15).
            03  SKT1220-KAISETUNAME                  PIC X(40).
            03  SKT1220-HKNJANUM                     PIC X(12).
            03  SKT1220-HOSPCD                       PIC X(14).
            03  SKT1220-RECEKA                       PIC X(20).
            03  SKT1220-KYURATE-TBL                OCCURS  3 TIMES.
              05  SKT1220-KYURATE                    PIC X(02).
            03  SKT1220-ZAI1                         PIC X(02).
            03  SKT1220-ZAI2                         PIC X(02).
            03  SKT1220-NO                           PIC X(04).
            03  SKT1220-SEIKYU-TBL                 OCCURS  11 TIMES.
              05  SKT1220-KENSU                      PIC X(05).
              05  SKT1220-NISSU                      PIC X(05).
              05  SKT1220-TENSU                      PIC X(09).
              05  SKT1220-ITBFTN                     PIC X(09).
            03  SKT1220-KOH-TBL                    OCCURS  5 TIMES.
              05  SKT1220-KOHNAME                    PIC X(08).
              05  SKT1220-K-KBN                      PIC X(04).
              05  SKT1220-K-KENSU                    PIC X(05).
              05  SKT1220-K-NISSU                    PIC X(05).
              05  SKT1220-K-TENSU                    PIC X(09).
              05  SKT1220-K-ITBFTN                   PIC X(09).
              05  SKT1220-K-KOHTENSU                 PIC X(09).
            03  SKT1220-TOKUBETU                     PIC X(10).
