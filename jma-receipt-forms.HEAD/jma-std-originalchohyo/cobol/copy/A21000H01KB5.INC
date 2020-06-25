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
       01  H01KB5.
           03  H01KB5-WORK                PIC X(16).
           03  H01KB5-TUZUKI              PIC X(08).
           03  H01KB5-HPTNUM              PIC X(16).
           03  H01KB5-KIGONUM             PIC X(32).
           03  H01KB5-KIGO                PIC X(32).
           03  H01KB5-NUM                 PIC X(32).
           03  H01KB5-KANANAME            PIC X(40).
           03  H01KB5-NAME                PIC X(24).
           03  H01KB5-PTNUM               PIC X(20).
           03  H01KB5-FTN-JKY-TBL         OCCURS 3. 
               05  H01KB5-JKYSNO          PIC X(14).
               05  H01KB5-FTNSNO          PIC X(16).
           03  H01KB5-FK.
               05  H01KB5-FK-NAME         PIC X(04).
               05  H01KB5-FK-JKYSNO       PIC X(20).
           03  H01KB5-OTOKO               PIC X(02).
           03  H01KB5-ONNA                PIC X(02).
           03  H01KB5-SIKAKUG             PIC X(04).
           03  H01KB5-SIKAKUY             PIC X(02).
           03  H01KB5-SIKAKUM             PIC X(02).
           03  H01KB5-SIKAKUD             PIC X(02).
           03  H01KB5-ADRS                PIC X(64).
           03  H01KB5-ADRSTEL             PIC X(20).
           03  H01KB5-HIHKNJNAME          PIC X(24).
           03  H01KB5-HKNJANAME           PIC X(64).
           03  H01KB5-HKNJAADR            PIC X(64).
           03  H01KB5-FTNRITU             PIC X(04).
           03  H01KB5-KUMIAIIN            PIC X(06).
           03  H01KB5-JUGYOIN             PIC X(06).
           03  H01KB5-KAZOKU              PIC X(06).
           03  H01KB5-TANJYOY             PIC X(02).
           03  H01KB5-TANJYOM             PIC X(02).
           03  H01KB5-TANJYOD             PIC X(02).
           03  H01KB5-TAISYOU             PIC X(02).
           03  H01KB5-SYOUWA              PIC X(02).
           03  H01KB5-MEIJI               PIC X(02).
           03  H01KB5-HEISEI              PIC X(02).
           03  H01KB5-YUKOG               PIC X(04).
           03  H01KB5-YUKOY               PIC X(02).
           03  H01KB5-YUKOM               PIC X(02).
           03  H01KB5-YUKOD               PIC X(02).
           03  H01KB5-SINSATU-TBL         OCCURS 15.
               05  H01KB5-BYOMEI          PIC X(84).
               05  H01KB5-TENKI           PIC X(06).
               05  H01KB5-SYOKUNAI        PIC X(02).
               05  H01KB5-SYOKUGAI        PIC X(02).
               05  H01KB5-STRYMDG.
                   07  H01KB5-STRYMDY     PIC X(04).
                   07  H01KB5-STRYMDM     PIC X(02).
                   07  H01KB5-STRYMDD     PIC X(02).
               05  H01KB5-ENDYMDG.
                   07  H01KB5-ENDYMDY     PIC X(04).
                   07  H01KB5-ENDYMDM     PIC X(02).
                   07  H01KB5-ENDYMDD     PIC X(02).
               05  H01KB5-MANRYOG.
                   07  H01KB5-MANRYOY     PIC X(04).
                   07  H01KB5-MANRYOM     PIC X(02).
                   07  H01KB5-MANRYOD     PIC X(02).
           03  H01KB5-FTN-JKY-N-TBL       OCCURS 3. 
               05  H01KB5-JKYSNON         PIC X(20).
               05  H01KB5-FTNSNON         PIC X(20).
           03  H01KB5-BIKO               PIC X(40).
           03  H01KB5-TANJYOG            PIC X(04).
