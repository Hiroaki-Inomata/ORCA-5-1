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
      *****************************************************************
      *    データチェック ＡＰＩ共通ＳＰＡ
      *****************************************************************
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
       01  SPA-API-AREA.
           03  SPA-API-PTINF-AREA.
               05  SPA-API-PTINF-MAX           PIC 9(05).
               05  SPA-API-PTINF-OCC           OCCURS  100.
                   07  SPA-API-PTINF-PTID      PIC 9(10).
           03  SPA-API-EPTINF-AREA.
               05  SPA-API-EPTINF-MAX          PIC 9(05).
               05  SPA-API-EPTINF-OCC          OCCURS  100.
                   07  SPA-API-EPTINF-PTNUM    PIC X(20).
                   07  SPA-API-EPTINF-ERRCD    PIC X(04).
