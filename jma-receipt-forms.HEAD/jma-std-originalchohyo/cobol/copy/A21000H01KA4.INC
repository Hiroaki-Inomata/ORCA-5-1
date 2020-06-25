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
      * 01.00.01     @@@@-門間    11/01/07  コメント履歴削除
      * 02.00.01     @@@@-門間    19/04/17  新元号対応
      ********************************************************************
       01  H01KA4.
           03  H01KA4-PTNUM              PIC X(20).
           03  H01KA4-FK.
               05  H01KA4-FK-NAME        PIC X(04).
               05  H01KA4-FK-JKYSNO      PIC X(20).
           03  H01KA4-FTN-JKY-TBL        OCCURS 2.
               05  H01KA4-FTNSNO         PIC X(16).
               05  H01KA4-JKYSNO         PIC X(14).
           03  H01KA4-KANANAME           PIC X(40).
           03  H01KA4-NAME               PIC X(20).
           03  H01KA4-NAME2              PIC X(40).
           03  H01KA4-OTOKO              PIC X(02).
           03  H01KA4-ONNA               PIC X(02).
           03  H01KA4-ONNA2              PIC X(02).
           03  H01KA4-MEIJI              PIC X(02).
           03  H01KA4-TAISYOU            PIC X(02).
           03  H01KA4-SYOUWA             PIC X(02).
           03  H01KA4-HEISEI             PIC X(02).
           03  H01KA4-TANJYOY            PIC X(02).
           03  H01KA4-TANJYOM            PIC X(02).
           03  H01KA4-TANJYOD            PIC X(02).
           03  H01KA4-ADRSPOST           PIC X(08).
           03  H01KA4-ADRS               PIC X(136).
           03  H01KA4-ADRSTEL            PIC X(20).
           03  H01KA4-WORK               PIC X(16).
           03  H01KA4-TUZUKI             PIC X(8).
           03  H01KA4-HPTNUM             PIC X(16).
           03  H01KA4-KIGONUM            PIC X(32).
           03  H01KA4-KIGO               PIC X(32).
           03  H01KA4-NUM                PIC X(32).
           03  H01KA4-YUKOY              PIC X(02).
           03  H01KA4-YUKOM              PIC X(02).
           03  H01KA4-YUKOD              PIC X(02).
           03  H01KA4-HIHKNJNAME         PIC X(24).
           03  H01KA4-SKKSYOWA           PIC X(02).
           03  H01KA4-SKKHEISEI          PIC X(02).
           03  H01KA4-SIKAKUY            PIC X(02).
           03  H01KA4-SIKAKUM            PIC X(02).
           03  H01KA4-SIKAKUD            PIC X(02).
           03  H01KA4-HKNJANAME          PIC X(48).
           03  H01KA4-HKNJAADR           PIC X(60).
           03  H01KA4-FTNRITU            PIC X(04).
           03  H01KA4-HKNKZKKBN-G        OCCURS 3.
               05  H01KA4-HKNKZKKBN      PIC X(06).
           03  H01KA4-SINSATU-TBL        OCCURS 17.
               05  H01KA4-BYOMEI         PIC X(84).
               05  H01KA4-TENKI          PIC X(06).
               05  H01KA4-STRYMDG.
                   07  H01KA4-STRYMDY    PIC X(04).
                   07  H01KA4-STRYMDM    PIC X(02).
                   07  H01KA4-STRYMDD    PIC X(02).
               05  H01KA4-ENDYMDG.
                   07  H01KA4-ENDYMDY    PIC X(04).
                   07  H01KA4-ENDYMDM    PIC X(02).
                   07  H01KA4-ENDYMDD    PIC X(02).
               05  H01KA4-MANRYOG.
                   07  H01KA4-MANRYOY    PIC X(04).
                   07  H01KA4-MANRYOM    PIC X(02).
                   07  H01KA4-MANRYOD    PIC X(02).
           03  H01KA4-FTN-JKY-N-TBL      OCCURS 2.
               05  H01KA4-FTNSNON        PIC X(20).
               05  H01KA4-JKYSNON        PIC X(20).
           03  H01KA4-BIKO               PIC X(40).
           03  H01KA4-TANJYOG            PIC X(04).
           03  H01KA4-YUKOG              PIC X(04).
           03  H01KA4-SIKAKUG            PIC X(04).
