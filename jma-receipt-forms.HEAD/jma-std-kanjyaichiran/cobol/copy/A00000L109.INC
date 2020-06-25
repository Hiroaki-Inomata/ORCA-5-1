      *****************************************************************
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
      *****************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     @@@@-門間    10/12/07  コメント履歴削除
      *****************************************************************
       01  L109.
           03  L109-MIDASI            PIC X(10).
           03  L109-ERRMSG            PIC X(50).
           03  L109-PAGE              PIC X(03).
           03  L109-SYSYMD            PIC X(22).
           03  L109-YMD1              PIC X(22).
           03  L109-YMD2              PIC X(22).
           03  L109-SRYCD             PIC X(09).
           03  L109-SRYMEI            PIC X(100).
           03  L109-DRNAME            PIC X(30).
           03  L109-NUM-G.
             05  L109-NUM-TBL         OCCURS 15.
               07  L109-NUM           PIC X(05).
           03  L109-PTNUM-G.
             05  L109-PTNUM-TBL       OCCURS 15.
               07  L109-PTNUM         PIC X(10).
           03  L109-KANANAME-G.
             05  L109-KANANAME-TBL    OCCURS 15.
               07  L109-KANANAME      PIC X(40).
           03  L109-NAME-G.
             05  L109-NAME-TBL        OCCURS 15.
               07  L109-NAME          PIC X(26).
           03  L109-SEX-G.
             05  L109-SEX-TBL         OCCURS 15.
               07  L109-SEX           PIC X(02).
           03  L109-BIRTHDAY-G.
             05  L109-BIRTHDAY-TBL    OCCURS 15.
               07  L109-BIRTHDAY      PIC X(10).
           03  L109-AGE-G.
             05  L109-AGE-TBL         OCCURS 15.
               07  L109-AGE           PIC X(03).
           03  L109-HKNJANUM-G.
             05  L109-HKNJANUM-TBL    OCCURS 15.
               07  L109-HKNJANUM      PIC X(08).
           03  L109-HONKAZKKBN-G.
             05  L109-HONKAZKKBN-TBL  OCCURS 15.
               07  L109-HONKAZKKBN    PIC X(02).
           03  L109-KIGONUM-G.
             05  L109-KIGONUM-TBL     OCCURS 15.
               07  L109-KIGONUM       PIC X(52).
               07  L109-KIGO          PIC X(52).
               07  L109-BANGO         PIC X(52).
           03  L109-KOH1FTNNUM-G.
             05  L109-KOH1FTNNUM-TBL  OCCURS 15.
               07  L109-KOH1FTNNUM    PIC X(08).
           03  L109-NYUGAIKBN-G.
             05  L109-NYUGAIKBN-TBL   OCCURS 15.
               07  L109-NYUGAIKBN     PIC X(02).
           03  L109-SRYKA-G.
             05  L109-SRYKA-TBL       OCCURS 15.
               07  L109-SRYKA         PIC X(10).
           03  L109-SRYYMD-G.
             05  L109-SRYYMD-TBL      OCCURS 15.
               07  L109-SRYYMD        PIC X(10).
           03  L109-LASTYMD-G.
             05  L109-LASTYMD-TBL     OCCURS 15.
               07  L109-LASTYMD       PIC X(10).
           03  L109-TENKI-G.
             05  L109-TENKI-TBL       OCCURS 15.
               07  L109-TENKI         PIC X(04).
           03  L109-TENKIYMD-G.
             05  L109-TENKIYMD-TBL    OCCURS 15.
               07  L109-TENKIYMD      PIC X(10).
           03  L109-CNTTIT1           PIC X(20).
           03  L109-CNTTIT2           PIC X(20).
           03  L109-CNTTIT3           PIC X(20).
           03  L109-CNT1              PIC X(10).
           03  L109-CNT2              PIC X(10).
           03  L109-CNT3              PIC X(10).
