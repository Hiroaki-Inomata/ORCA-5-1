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
      ************************************************************
      *    診療報酬請求書     COPY (CPSKT0600TMP.INC)            *
      *
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者  日付      内容
      *  01.00.01    吉川    08/10/08  シス管「2007」公費まとめ対応
      *  01.00.02    吉川    12/04/17  県外分は都道府県ごとに集計する
      ************************************************************
       01  SKT1100TMP-REC.
           03      SKT1100TMP-KEY.
             05    SKT1100TMP-KENKBN                PIC  9(01).
             05    SKT1100TMP-KBN1                  PIC  9(01).
             05    SKT1100TMP-KBN2                  PIC  9(01).
             05    SKT1100TMP-HKNJANUM              PIC  X(06).
             05    SKT1100TMP-KYURATE               PIC  9(02).
      *=== シス管「2007」対応 by Yoshikawa(2008/10/08) start
           03      SKT1100TMP-KOHNAME               PIC  X(10).
      *=== シス管「2007」対応 by Yoshikawa(2008/10/08) end
      *=== 県外は都道府県ごとに集計 by Yoshikawa(2012/04/17) start
           03      SKT1100TMP-HKNJANAME             PIC  X(20).
      *=== 県外は都道府県ごとに集計 by Yoshikawa(2012/04/17) end
           03      SKT1100TMP-DATA.
             05    SKT1100TMP-GAIRAI.
               07  SKT1100TMP-KENSU               PIC  9(05).
               07  SKT1100TMP-NISSU               PIC  9(05).
               07  SKT1100TMP-TENSU               PIC  9(09).
               07  SKT1100TMP-SEIKYUGK            PIC  9(09).
