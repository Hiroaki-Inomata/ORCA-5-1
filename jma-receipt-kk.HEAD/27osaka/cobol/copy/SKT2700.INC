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
      * Maj/Min/Rev  修正者    日付         内容
      *  01.00.01    吉川      2010/11/02   特別療養費国保対応
      *  01.00.02    門間      2018/12/03   修正履歴コメント削除
      ********************************************************************
        01  SKT2700.
            02  SKT2700-SRYYM   PIC X(16).
            02  SKT2700-HKNJANAME   PIC X(20).
            02  SKT2700-HKNJANUM    PIC X(6).
            02  SKT2700-SEIYMD  PIC X(22).
            02  SKT2700-HOSPCDN PIC X(20).
            02  SKT2700-ADRS    PIC X(100).
            02  SKT2700-HOSPNAME    PIC X(100).
            02  SKT2700-KAISETUNAME PIC X(40).
            02  SKT2700-TEL PIC X(15).
            02  SKT2700-TOKUBETU    PIC  X(10).
            02  SKT2700-KYURATE1    PIC X(2).
            02  SKT2700-KYURATE2    PIC X(2).
            02  SKT2700-KYURATE3    PIC X(2).
            02  SKT2700-KYURATE4    PIC X(2).
            02  SKT2700-SEIKYU          OCCURS  31 TIMES.
              03  SKT2700-KENSU   PIC X(5).
              03  SKT2700-TENSU   PIC X(9).
              03  SKT2700-ITBFTN  PIC X(9).
              03  SKT2700-S-KENSU PIC X(5).
              03  SKT2700-S-KINGAKU   PIC X(9).
              03  SKT2700-S-FTNGAKU   PIC X(9).
              03  SKT2700-BIKO    PIC X(2).
            02  SKT2700-GOKEI.
              03  SKT2700-GKENSU  PIC X(5).
              03  SKT2700-GTENSU  PIC X(9).
              03  SKT2700-GITBFTN PIC X(9).
              03  SKT2700-S-GKENSU    PIC X(5).
              03  SKT2700-S-GKINGAKU  PIC X(9).
              03  SKT2700-S-GFTNGAKU  PIC X(9).
              03  SKT2700-GBIKO   PIC X(3).
            02  SKT2700-MENJO   PIC X(4).
            02  SKT2700-TAKOH   PIC X(4).
