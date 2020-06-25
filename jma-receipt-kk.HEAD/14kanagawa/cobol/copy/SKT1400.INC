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
      *  01.00.01    吉川    2010/08/04  特別療養費国保対応
      ********************************************************************
        01  SKT1400.
            02  SKT1400-SEIYM   PIC X(16).
            02  SKT1400-HOSPNAME    PIC X(100).
            02  SKT1400-HOSPCD  PIC X(20).
            02  SKT1400-HKNJASU PIC X(5).
            02  SKT1400-SEIKYU          OCCURS  7 TIMES.
              03  SKT1400-KENSU   PIC X(7).
              03  SKT1400-TENSU   PIC X(14).
            02  SKT1400-UKETUKE-SU  PIC X(7).
      *=== 特別療養費国保対応 by Yoshikawa(2010/08/04) start
            02  SKT1400-TOKUBETU  PIC X(10).
      *=== 特別療養費国保対応 by Yoshikawa(2010/08/04) end
