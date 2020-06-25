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
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     @@@@-門間    10/12/07  コメント履歴削除
      ********************************************************************
       01  HC901.
               03  HC901-SYSYMD        PIC X(22).
               03  HC901-PAGE          PIC X(3).
               03  HC901-SRYYMD        PIC X(22).
               03  HC901-MIDASI        PIC X(10).
               03  HC901-NYUGAIKBN     PIC X(4).
               03  HC901-TSRYKA        PIC X(20).
               03  HC901-YOSO-G                    OCCURS  11 TIMES.
                   05  HC901-YOSO-OCC              OCCURS  5 TIMES.
                       07  HC901-YOSO  PIC X(12).
