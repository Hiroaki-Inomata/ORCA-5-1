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
      ******************************************************************
      *    HAORIリクエスト収納情報
      *    COPY (CPINCOMEV3REQ01.INC)
      ******************************************************************
        01  INCOMEREQ01-AREA.
            03  INCOMEREQ01-CONTEXT        PIC S9(9)   BINARY.
            03  INCOMEREQ01-OBJECT         PIC X(36).
            03  INCOMEREQ01-MODE           PIC S9(9)   BINARY.
            03  INCOMEREQ01-RECORDNAME     PIC X(128).
      *
            03  INCOMEREQ01-DATA-AREA.
                05  INCOMEREQ01-REQNUM         PIC X(02).
                05  INCOMEREQ01-REQMODE        PIC X(02).
                05  INCOMEREQ01-KARTE-UID      PIC X(36).
                05  INCOMEREQ01-ORCA-UID       PIC X(36).
                05  INCOMEREQ01-PTNUM          PIC X(20).
                05  INCOMEREQ01-OUTKBN         PIC X(1).
                05  INCOMEREQ01-SRYYM          PIC X(7).
                05  INCOMEREQ01-STDAY          PIC X(2).
                05  INCOMEREQ01-EDDAY          PIC X(2).
                05  INCOMEREQ01-STSRYYMD       PIC X(10).
                05  INCOMEREQ01-EDSRYYMD       PIC X(10).
                05  INCOMEREQ01-STSRYYM        PIC X(07).
                05  INCOMEREQ01-EDSRYYM        PIC X(07).
                05  INCOMEREQ01-SORT           PIC X(1).
                05  INCOMEREQ01-ORDER          PIC X(1).
                05  INCOMEREQ01-RANGE-FROM     PIC X(05).
                05  INCOMEREQ01-RANGE-TO       PIC X(05).
                05  INCOMEREQ01-SYUNOU-AREA.
                    07  INCOMEREQ01-SYUNOU-OCC         OCCURS 500.
                        09  INCOMEREQ01-TNYUGAIKBN     PIC X(1).
                        09  INCOMEREQ01-TDENPNUM       PIC X(07).
                        09  INCOMEREQ01-TNYUHENKBN     PIC X(1).
                        09  INCOMEREQ01-TNYUHENYMD     PIC X(10).
                        09  INCOMEREQ01-TNYUHENTIME    PIC X(05).
                        09  INCOMEREQ01-TNYUKIN-HOHO   PIC X(02).
                        09  INCOMEREQ01-TNYUKIN        PIC X(10).
                        09  INCOMEREQ01-TFORCE         PIC X(05).
                05  INCOMEREQ01-NYUGAIKBN      PIC X(1).
                05  INCOMEREQ01-DENPNUM        PIC X(07).
      *         内部処理用(NYUHENKBN)仕様書には記載しない
                05  INCOMEREQ01-NYUHENKBN      PIC X(1).
                05  INCOMEREQ01-DENPEDANUM     PIC X(2).
                05  INCOMEREQ01-NYUHENYMD      PIC X(10).
                05  INCOMEREQ01-NYUHENTIME     PIC X(5).
                05  INCOMEREQ01-CHOSEI1        PIC X(10).
                05  INCOMEREQ01-CHOSEI2        PIC X(10).
                05  INCOMEREQ01-NYUKIN         PIC X(10).
                05  INCOMEREQ01-NYUKIN-HOHO    PIC X(2).
                05  INCOMEREQ01-FORCE          PIC X(05).
                05  INCOMEREQ01-PRINT-MODE     PIC X(10).
                05  INCOMEREQ01-PRINT-AREA.
                    07  INCOMEREQ01-SKYPRT     PIC X(1).
                    07  INCOMEREQ01-SRYPRT     PIC X(1).
                05  INCOMEREQ01-PRINT-ORDER-AREA.
                    07  INCOMEREQ01-SKYGRP     PIC X(1).
