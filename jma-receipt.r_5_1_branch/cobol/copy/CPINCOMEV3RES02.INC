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
      *    ＡＰＩレスポンス収納情報     COPY (CPINCOMEV3RES02.INC)
      ******************************************************************
        01  INCOMERES02-AREA.
            03  INCOMERES02-CONTEXT                PIC S9(9) BINARY.
            03  INCOMERES02-OBJECT                 PIC X(36).
            03  INCOMERES02-MODE                   PIC S9(9) BINARY.
            03  INCOMERES02-RECORDNAME             PIC X(128).
            03  INCOMERES02-DATA-AREA.
              05  INCOMERES02-INFORMATION-DATE     PIC X(10).
              05  INCOMERES02-INFORMATION-TIME     PIC X(8).
              05  INCOMERES02-API-RESULT           PIC X(5).
              05  INCOMERES02-API-RESULT-MSG       PIC X(100).
              05  INCOMERES02-REQNUM               PIC X(2).
              05  INCOMERES02-REQMODE              PIC X(02).
              05  INCOMERES02-RESNUM               PIC X(2).
              05  INCOMERES02-KARTE-UID            PIC X(36).
              05  INCOMERES02-ORCA-UID             PIC X(36).
              05  INCOMERES02-PTNUM                PIC X(20).
              05  INCOMERES02-SYUNOU-AREA.
                07  INCOMERES02-SRYYMD             PIC X(10).
                07  INCOMERES02-SKYEDYMD           PIC X(10).
                07  INCOMERES02-DENPPRTYMD         PIC X(10).
                07  INCOMERES02-NYUGAIKBN          PIC X(1).
                07  INCOMERES02-DENPNUM            PIC X(7).
                07  INCOMERES02-HKNCOMBINUM        PIC X(4).
                07  INCOMERES02-FTNRATE            PIC X(3).
                07  INCOMERES02-SRYKA              PIC X(2).
                07  INCOMERES02-PTFTN-AREA.
                  09  INCOMERES02-SKYMONEY         PIC X(10).
                  09  INCOMERES02-SKYMONEY-TAX-SAI PIC X(10).
                  09  INCOMERES02-NYUKIN           PIC X(10).
                  09  INCOMERES02-HKNMONEY         PIC X(10).
                  09  INCOMERES02-JIHIMONEY        PIC X(10).
                  09  INCOMERES02-YKZMONEY         PIC X(10).
                  09  INCOMERES02-RJNMONEY         PIC X(10).
                  09  INCOMERES02-KOHMONEY         PIC X(10).
                  09  INCOMERES02-SKJMONEY         PIC X(10).
                  09  INCOMERES02-LIFEMONEY        PIC X(10).
                  09  INCOMERES02-RSI-TOTAL        PIC X(10).
                  09  INCOMERES02-GENMEN           PIC X(10).
                  09  INCOMERES02-CHOSEI1          PIC X(10).
                  09  INCOMERES02-CHOSEI2          PIC X(10).
                07  INCOMERES02-SRYITEM-AREA.
                  09  INCOMERES02-TOTAL-TEN        PIC X(10).
                  09  INCOMERES02-TOTAL-TGMONEY    PIC X(10).
                  09  INCOMERES02-TGMONEY-TAX-SAI  PIC X(10).
                  09  INCOMERES02-SRYITEM-OCC      OCCURS  16.
                    11  INCOMERES02-SRYITEM-KBN    PIC X(3).
                    11  INCOMERES02-SRYITEM-NAME   PIC X(30).
                    11  INCOMERES02-HKNTEN         PIC X(10).
                    11  INCOMERES02-TGMONEY        PIC X(10).
                07  INCOMERES02-ETCJIHI-AREA.
                  09  INCOMERES02-JIHI-TOTAL       PIC X(10).
                  09  INCOMERES02-JIHI-TOTAL-TAX   PIC X(10).
                  09  INCOMERES02-JIHI-TOTAL-TAX-SAI
                                                   PIC X(10).
                  09  INCOMERES02-ETCJIHI-OCC      OCCURS  10.
                    11  INCOMERES02-JIHI-NO        PIC X(2).
                    11  INCOMERES02-JIHI-NAME      PIC X(20).
                    11  INCOMERES02-JIHI-TAXNASI   PIC X(10).
                    11  INCOMERES02-JIHI-TAXARI    PIC X(10).
                07  INCOMERES02-RSI-AREA.
                  09  INCOMERES02-RSISHOSHIN-MONEY PIC X(10).
                  09  INCOMERES02-RSISAISHIN-MONEY PIC X(10).
                  09  INCOMERES02-RSISDO-MONEY     PIC X(10).
                  09  INCOMERES02-RSIETC-MONEY     PIC X(10).
                07  INCOMERES02-SKJRYOYO           PIC X(10).
                07  INCOMERES02-LIFERYOYO          PIC X(10).
                07  INCOMERES02-SKJRYOYO-JIHI      PIC X(10).
                07  INCOMERES02-SKJFTN-JIHI        PIC X(10).
                07  INCOMERES02-LIFERYOYO-JIHI     PIC X(10).
                07  INCOMERES02-LIFEFTN-JIHI       PIC X(10).
                07  INCOMERES02-RMSAGAKU           PIC X(10).
                07  INCOMERES02-RMSAGAKU-TAX-SAI   PIC X(10).
              05  INCOMERES02-SMEI-OCC           OCCURS  99.
                07  INCOMERES02-TDENPEDANUM      PIC X(2).
                07  INCOMERES02-TNYUHENYMD       PIC X(10).
                07  INCOMERES02-TNYUHENTIME      PIC X(5).
                07  INCOMERES02-TSKYMONEY        PIC X(10).
                07  INCOMERES02-TCHOSEI1         PIC X(10).
                07  INCOMERES02-TCHOSEI2         PIC X(10).
                07  INCOMERES02-TNYUKIN          PIC X(10).
                07  INCOMERES02-TJOTAI           PIC X(2).
                07  INCOMERES02-TJOTAI-NAME      PIC X(10).
                07  INCOMERES02-TNYUKIN-HOHO     PIC X(2).
                07  INCOMERES02-TNYUKIN-HOHO-NAME PIC X(20).
