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
      *  01.00.01    吉川      2010/11/01   特別療養費国保対応
      *  01.00.02    萩野      2020/02/12   修正履歴コメント削除
      ********************************************************************
       01  SKT2300.
           03  SKT2300-SEIYM1       PIC  X(16).
           03  SKT2300-TOKUBETU     PIC  X(10).
           03  SKT2300-HKNJAINF     OCCURS  8.
             05  SKT2300-HKNJANAME  PIC  X(100).
             05  SKT2300-HKNKBN     OCCURS  3.
               07  SKT2300-NYUGAIKBN  OCCURS  2.
                 09  SKT2300-KENSU  PIC  X(05).
                 09  SKT2300-NISSU  PIC  X(05).
                 09  SKT2300-TENSU  PIC  X(10).
             05  SKT2300-SKJ-AREA OCCURS 2.
               07  SKT2300-SKJ    PIC  X(10).
           03  SKT2300-KOHKENSU     PIC  X(11).
           03  SKT2300-FUKUKENSU    PIC  X(11).
      *
           03  SKT2300-SEIYM2       PIC  X(16).
           03  SKT2300-HOSPCDN      PIC  X(20).
           03  SKT2300-ADRS         PIC  X(100).
           03  SKT2300-HOSPNAME       PIC  X(100).
           03  SKT2300-KAISETUNAME    PIC  X(40).
           03  SKT2300-PRTYMD         PIC  X(22).
           03  SKT2300-GHKNKBN  OCCURS  4.
             05  SKT2300-GNYUGAIKBN  OCCURS  2.
               07  SKT2300-GKENSU  PIC  X(07).
               07  SKT2300-GNISSU  PIC  X(07).
               07  SKT2300-GTENSU  PIC  X(11).
               07  SKT2300-GSKJ    PIC  X(09).
           03  SKT2300-GKOHKENSU   PIC  X(11).
           03  SKT2300-GFUKUKENSU  PIC  X(11).
