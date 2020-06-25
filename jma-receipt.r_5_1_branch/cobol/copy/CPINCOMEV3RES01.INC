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
      *    ＡＰＩレスポンス収納情報     COPY (CPINCOMEV3RES01.INC)
      ******************************************************************
        01  INCOMERES01-AREA.
            03  INCOMERES01-CONTEXT                  PIC S9(9) BINARY.
            03  INCOMERES01-OBJECT                   PIC X(36).
            03  INCOMERES01-MODE                     PIC S9(9) BINARY.
            03  INCOMERES01-RECORDNAME               PIC X(128).
            03  INCOMERES01-DATA-AREA.
              05  INCOMERES01-INFORMATION-DATE       PIC X(10).
              05  INCOMERES01-INFORMATION-TIME       PIC X(8).
              05  INCOMERES01-API-RESULT             PIC X(5).
              05  INCOMERES01-API-RESULT-MSG         PIC X(100).
              05  INCOMERES01-REQNUM                 PIC X(02).
              05  INCOMERES01-REQMODE                PIC X(02).
              05  INCOMERES01-RESNUM                 PIC X(02).
              05  INCOMERES01-KARTE-UID              PIC X(36).
              05  INCOMERES01-ORCA-UID               PIC X(36).
              05  INCOMERES01-PTNUM                  PIC X(20).
              05  INCOMERES01-SYUNOU-OVER            PIC X(05).
              05  INCOMERES01-SYUNOU-AREA.
                  07  INCOMERES01-SYUNOU-OCC         OCCURS  500.
                      09  INCOMERES01-TSRYYMD        PIC X(10).
                      09  INCOMERES01-TSKYEDYMD      PIC X(10).
                      09  INCOMERES01-TDENPPRTYMD    PIC X(10).
                      09  INCOMERES01-TNYUGAIKBN     PIC X(01).
                      09  INCOMERES01-TDENPNUM       PIC X(07).
                      09  INCOMERES01-TGRP-DENPNUM   PIC X(07).
                      09  INCOMERES01-THKNCOMBI      PIC X(04).
                      09  INCOMERES01-TFTNRATE       PIC X(03).
                      09  INCOMERES01-TSRYKA         PIC X(02).
                      09  INCOMERES01-TSEIKYU        PIC X(10).
                      09  INCOMERES01-TNYUKIN        PIC X(10).
                      09  INCOMERES01-TMISYU         PIC X(10).
                      09  INCOMERES01-TJOTAIKBN      PIC X(01).
                      09  INCOMERES01-TJOTAINAME     PIC X(10).
                      09  INCOMERES01-TTYPE          PIC X(01).
                      09  INCOMERES01-TTYPENAME      PIC X(10).
                      09  INCOMERES01-TKEIKOKU       PIC X(01).
                      09  INCOMERES01-TKEIKOKUNAME   PIC X(20).
                      09  INCOMERES01-TERRCD         PIC X(05).
                      09  INCOMERES01-TERRMSG        PIC X(100).
              05  INCOMERES01-MISYU-TTL-AREA.
                  07  INCOMERES01-TMISYU-TTL-N       PIC X(10).
                  07  INCOMERES01-TKANYU-TTL-N       PIC X(10).
                  07  INCOMERES01-TMISYU-TTL-G       PIC X(10).
                  07  INCOMERES01-TKANYU-TTL-G       PIC X(10).
              05  INCOMERES01-DATA-ID-AREA.
                  07 INCOMERES01-DATA-ID             PIC X(37).
