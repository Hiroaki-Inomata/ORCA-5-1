      *******************************************************************
      * Project code name "ORCA"
      * Æü°åÉ¸½à¥ì¥»¥×¥È¥½¥Õ¥È¡ÊJMA standard receipt software¡Ë
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
      *    ¼ýÇ¼ £Á£Ð£É¶¦ÄÌ£Ó£Ð£Á
      *****************************************************************
      *****************************************************************
      *  ¥×¥í¥°¥é¥à½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      *****************************************************************
       01  SPA-API-AREA.
      *
           03  SPA-REQ-NYUGAIKBN               PIC X(1).
           03  SPA-REQ-DENPNUM                 PIC X(07).
           03  SPA-REQ-DENPEDANUM              PIC X(02).
           03  SPA-REQ-NYUHENKBN               PIC X(1).
           03  SPA-REQ-NYUHENYMD               PIC X(10).
           03  SPA-REQ-NYUHENTIME              PIC X(05).
           03  SPA-REQ-NYUKIN-HOHO             PIC X(02).
           03  SPA-REQ-CHOSEI1                 PIC X(10).
           03  SPA-REQ-CHOSEI2                 PIC X(10).
           03  SPA-REQ-NYUKIN                  PIC X(10).
           03  SPA-REQ-FORCE                   PIC X(05).
      *
           03  SPA-API-HAKKOKBN                PIC X(01).
           03  SPA-API-NYUGAIKBN               PIC X(01).
           03  SPA-API-DENPNUM                 PIC 9(07).
           03  SPA-API-SRYYMD                  PIC X(08).
           03  SPA-API-SRYKA                   PIC X(02).
           03  SPA-API-HKNCOMBI                PIC X(04).
           03  SPA-API-DENPEDANUM              PIC 9(02).
           03  SPA-API-NYUHENKBN               PIC X(1).
           03  SPA-API-NYUKIN-HOHO             PIC X(02).
           03  SPA-API-NYUHENYMD               PIC X(08).
           03  SPA-API-NYUHENHMS               PIC X(06).
           03  SPA-API-CHOSEI1                 PIC S9(10).
           03  SPA-API-CHOSEI2                 PIC S9(10).
           03  SPA-API-NYUKIN                  PIC S9(10).
           03  SPA-API-MISYU                   PIC S9(10).
           03  SPA-API-FORCE                   PIC X(01).
           03  SPA-API-PROC-FLG                PIC 9(01).
           03  SPA-API-MINUS                   PIC X(01).
      *
           03  SPA-API-SMEI-JOTAIKBN           PIC X(01).
           03  SPA-API-BASEYMD                 PIC X(08).
           03  SPA-API-STSRYYMD                PIC X(08).
           03  SPA-API-EDSRYYMD                PIC X(08).
           03  SPA-API-SRYYM                   PIC X(06).
           03  SPA-API-KARTE-UID               PIC X(36).
           03  SPA-API-ORCA-UID                PIC X(36).
           03  SPA-API-OUTKBN                  PIC X(01).
           03  SPA-API-SORT                    PIC X(1).
           03  SPA-API-ORDER                   PIC X(1).
           03  SPA-API-OFFSET                  PIC 9(05).
           03  SPA-API-LIMIT                   PIC 9(05).
           03  SPA-API-FROM                    PIC 9(05).
           03  SPA-API-TO                      PIC 9(05).
           03  SPA-API-SYU-COUNT               PIC 9(05).
           03  SPA-API-RESNUM                  PIC X(02).
           03  SPA-API-ERRFLG                  PIC 9(01).
           03  SPA-API-SYUNOU-MAX              PIC 9(05).
           03  SPA-API-SYUNOU-AREA.
               05  SPA-API-SYUNOU-OCC          OCCURS  500.
                   07  SPA-API-TNYUHENKBN      PIC X(01).
                   07  SPA-API-TDENPNUM        PIC 9(07).
                   07  SPA-API-TSRYYMD         PIC X(08).
                   07  SPA-API-TSRYKA          PIC X(02).
                   07  SPA-API-THKNCOMBI       PIC X(04).
                   07  SPA-API-THKNKBN         PIC X(01).
                   07  SPA-API-TRSI-HKNKBN     PIC X(01).
                   07  SPA-API-TNYUGAIKBN      PIC X(01).
                   07  SPA-API-TNYUKIN-HOHO    PIC X(02).
                   07  SPA-API-TNYUKIN         PIC S9(10).
                   07  SPA-API-TMISYU          PIC S9(10).
                   07  SPA-API-TNYUHENYMD      PIC X(08).
                   07  SPA-API-TNYUHENHMS      PIC X(06).
                   07  SPA-API-TFORCE          PIC X(01).
                   07  SPA-API-TERRCD          PIC X(04).
                   07  SPA-API-TERRMSG         PIC X(100).
           03  SPA-API-TUKISUMKBN              PIC X(01).
           03  SPA-API-KIKANKBN                PIC X(01).
           03  SPA-API-STYMD.
               05  SPA-API-STYM                PIC X(06).
               05  SPA-API-STDAY               PIC 9(02).
           03  SPA-API-EDYMD.
               05  SPA-API-EDYM                PIC X(06).
               05  SPA-API-EDDAY               PIC 9(02).
           03  SPA-API-PRINT-MODE              PIC X(10).
           03  SPA-API-SKYPRT                  PIC X(01).
           03  SPA-API-SKYGRP                  PIC X(01).
           03  SPA-API-SRYPRT                  PIC X(01).

