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
      * 01.00.02     @@@@-門間    12/06/19  保険者名称が12文字を超えた場合、
      *                                     2行表示（24文字）するように修正
      * 02.00.01     @@@@-門間    19/04/17  新元号対応
      ********************************************************************
       01  H01SA4.
           03  H01SA4-PTNUM              PIC X(20).
           03  H01SA4-FK.
               05  H01SA4-FK-NAME        PIC X(04).
               05  H01SA4-FK-JKYSNO      PIC X(20).
           03  H01SA4-FTN-JKY-TBL        OCCURS 2.
               05  H01SA4-FTNSNO         PIC X(16).
               05  H01SA4-JKYSNO         PIC X(14).
           03  H01SA4-KANANAME           PIC X(40).
           03  H01SA4-NAME               PIC X(20).
           03  H01SA4-NAME2              PIC X(40).
           03  H01SA4-OTOKO              PIC X(02).
           03  H01SA4-ONNA               PIC X(02).
           03  H01SA4-MEIJI              PIC X(02).
           03  H01SA4-TAISYOU            PIC X(02).
           03  H01SA4-SYOUWA             PIC X(02).
           03  H01SA4-HEISEI             PIC X(02).
           03  H01SA4-TANJYOY            PIC X(02).
           03  H01SA4-TANJYOM            PIC X(02).
           03  H01SA4-TANJYOD            PIC X(02).
           03  H01SA4-ADRSPOST           PIC X(08).
           03  H01SA4-ADRS               PIC X(136).
           03  H01SA4-ADRSTEL            PIC X(20).
           03  H01SA4-WORK               PIC X(16).
           03  H01SA4-TUZUKI             PIC X(08).
           03  H01SA4-FTNRITU            PIC X(04).
           03  H01SA4-HPTNUM             PIC X(16).
           03  H01SA4-KIGONUM            PIC X(32).
           03  H01SA4-KIGO               PIC X(32).
           03  H01SA4-NUM                PIC X(32).
           03  H01SA4-YUKOY              PIC X(02).
           03  H01SA4-YUKOM              PIC X(02).
           03  H01SA4-YUKOD              PIC X(02).
           03  H01SA4-HIHKNJNAME         PIC X(24).
           03  H01SA4-SKKSYOWA           PIC X(02).
           03  H01SA4-SKKHEISEI          PIC X(02).
           03  H01SA4-SIKAKUY            PIC X(02).
           03  H01SA4-SIKAKUM            PIC X(02).
           03  H01SA4-SIKAKUD            PIC X(02).
           03  H01SA4-KINADRS            PIC X(68).
           03  H01SA4-KINADRSTEL         PIC X(20).
           03  H01SA4-KINNAME            PIC X(48).
           03  H01SA4-HKNADRS            PIC X(68).
           03  H01SA4-HKNADRSTEL         PIC X(20).
           03  H01SA4-HKNNAME            PIC X(24).
           03  H01SA4-HKNNAME2           PIC X(48).
           03  H01SA4-SINSATU-TBL        OCCURS 17.
               05  H01SA4-BYOMEI         PIC X(84).
               05  H01SA4-TENKI          PIC X(06).
               05  H01SA4-SYOKUNAI       PIC X(02).
               05  H01SA4-SYOKUGAI       PIC X(02).
               05  H01SA4-STRYMDG.
                   07  H01SA4-STRYMDY    PIC X(04).
                   07  H01SA4-STRYMDM    PIC X(02).
                   07  H01SA4-STRYMDD    PIC X(02).
               05  H01SA4-ENDYMDG.
                   07  H01SA4-ENDYMDY    PIC X(04).
                   07  H01SA4-ENDYMDM    PIC X(02).
                   07  H01SA4-ENDYMDD    PIC X(02).
               05  H01SA4-MANRYOG.
                   07  H01SA4-MANRYOY    PIC X(04).
                   07  H01SA4-MANRYOM    PIC X(02).
                   07  H01SA4-MANRYOD    PIC X(02).
           03  H01SA4-FTN-JKY-N-TBL      OCCURS 2.
               05  H01SA4-FTNSNON        PIC X(20).
               05  H01SA4-JKYSNON        PIC X(20).
           03  H01SA4-BIKO               PIC X(40).
           03  H01SA4-TANJYOG            PIC X(04).
           03  H01SA4-YUKOG              PIC X(04).
           03  H01SA4-SIKAKUG            PIC X(04).
