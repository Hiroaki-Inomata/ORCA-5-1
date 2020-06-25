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
      *    ＡＰＩレスポンス収納情報     COPY (CPINCOMEINFV2RES.INC)
      ******************************************************************
        01  XML-INCOMEINFRES.
            03  ICINFRES-CONTEXT                       PIC S9(9) BINARY.
            03  ICINFRES-OBJECT                        PIC X(36).
            03  ICINFRES-MODE                          PIC S9(9) BINARY.
            03  ICINFRES-RECORDNAME                    PIC X(128).
            03  ICINFRES-PRIVATE-OBJECTS.
              05  ICINFRES-INFORMATION-DATE            PIC X(10).
              05  ICINFRES-INFORMATION-TIME            PIC X(8).
              05  ICINFRES-API-RESULT                  PIC X(4).
              05  ICINFRES-API-RESULT-MSG              PIC X(100).
              05  ICINFRES-PTINF-AREA.
                07  ICINFRES-PTNUM                     PIC X(20).
                07  ICINFRES-NAME                      PIC X(100).
                07  ICINFRES-KANANAME                  PIC X(100).
                07  ICINFRES-BIRTHDAY                  PIC X(10).
                07  ICINFRES-SEX                       PIC X(1).
              05  ICINFRES-INCONEINF-OVER              PIC X(05).
              05  ICINFRES-INCONEINF-OCC               OCCURS 200.
                07  ICINFRES-SRYYMD                    PIC X(10).
                07  ICINFRES-SKYEDYMD                  PIC X(10).
                07  ICINFRES-DENPPRTYMD                PIC X(10).
                07  ICINFRES-NYUGAIKBN                 PIC X(1).
                07  ICINFRES-DENPNUM                   PIC X(7).
                07  ICINFRES-HKNCOMBI                  PIC X(4).
                07  ICINFRES-FTNRATE                   PIC X(3).
                07  ICINFRES-SRYKA                     PIC X(2).
                07  ICINFRES-SRYKANAME                 PIC X(30).
                07  ICINFRES-PTFTN-AREA.
                  09  ICINFRES-SKYMONEY                PIC X(10).
                  09  ICINFRES-SKYMONEY-TAX-SAI        PIC X(10).
                  09  ICINFRES-NYUKIN                  PIC X(10).
                  09  ICINFRES-HKNMONEY                PIC X(10).
                  09  ICINFRES-JIHIMONEY               PIC X(10).
                  09  ICINFRES-YKZMONEY                PIC X(10).
                  09  ICINFRES-RJNMONEY                PIC X(10).
                  09  ICINFRES-KOHMONEY                PIC X(10).
                  09  ICINFRES-SLMONEY                 PIC X(10).
                  09  ICINFRES-SKJMONEY                PIC X(10).
                  09  ICINFRES-LIFEMONEY               PIC X(10).
                  09  ICINFRES-RSI-TOTAL               PIC X(10).
                  09  ICINFRES-RSIMONEY                PIC X(10).
                  09  ICINFRES-GENMEN                  PIC X(10).
                  09  ICINFRES-CHOSEI1                 PIC X(10).
                  09  ICINFRES-CHOSEI2                 PIC X(10).
                07  ICINFRES-AC-POINT-INFORMATION.
                  09  ICINFRES-TOTAL-TEN               PIC X(10).
                  09  ICINFRES-TOTAL-TGMONEY           PIC X(10).
                  09  ICINFRES-TGMONEY-TAX-SAI         PIC X(10).
                  09  ICINFRES-SRYITEM-OCC             OCCURS 16.
                    11  ICINFRES-SRYITEM-KBN           PIC X(03).
                    11  ICINFRES-SRYITEM-NAME          PIC X(30).
                    11  ICINFRES-SRYITEM-TENSU         PIC X(10).
                    11  ICINFRES-SRYITEM-TGMONEY       PIC X(10).
                07  ICINFRES-ETCJIHI-AREA.
                  09  ICINFRES-JIHI-TOTAL              PIC X(10).
                  09  ICINFRES-JIHI-TOTAL-TAX          PIC X(10).
                  09  ICINFRES-JIHI-TOTAL-TAX-SAI      PIC X(10).
                  09  ICINFRES-ETCJIHI-OCC             OCCURS  10.
                    11  ICINFRES-JIHI-NO               PIC X(2).
                    11  ICINFRES-JIHI-NAME             PIC X(20).
                    11  ICINFRES-JIHI-TAXNASI          PIC X(10).
                    11  ICINFRES-JIHI-TAXARI           PIC X(10).
                07  ICINFRES-RSI-AREA.
                  09  ICINFRES-RSISHOSHIN-MONEY        PIC X(10).
                  09  ICINFRES-RSISAISHIN-MONEY        PIC X(10).
                  09  ICINFRES-RSISDO-MONEY            PIC X(10).
                  09  ICINFRES-RSIETC-MONEY            PIC X(10).
                07  ICINFRES-SLRYOYO                   PIC X(10).
                07  ICINFRES-SKJRYOYO                  PIC X(10).
                07  ICINFRES-LIFERYOYO                 PIC X(10).
                07  ICINFRES-SKJRYOYO-JIHI             PIC X(10).
                07  ICINFRES-SKJFTN-JIHI               PIC X(10).
                07  ICINFRES-LIFERYOYO-JIHI            PIC X(10).
                07  ICINFRES-LIFEFTN-JIHI              PIC X(10).
                07  ICINFRES-RMSAGAKU                  PIC X(10).
                07  ICINFRES-RMSAGAKU-TAX-SAI          PIC X(10).
              05  ICINFRES-HKNCOMBI-INF-AREA           OCCURS  20.
                07  ICINFRES-HKNCOMBI-KEY              PIC X(04).
                07  ICINFRES-HKNNUM                    PIC X(03).
                07  ICINFRES-HKNJANUM                  PIC X(08).
                07  ICINFRES-HKNJANAME                 PIC X(20).
                07  ICINFRES-KIGO                      PIC X(80).
                07  ICINFRES-NUM                       PIC X(80).
                07  ICINFRES-KOHINF-AREA.
                  09  HSFDRES-KOHINF-OCC           OCCURS 4.
                    11  ICINFRES-KOHNUM           PIC X(03).
                    11  ICINFRES-KOHNAME          PIC X(20).
                    11  ICINFRES-FTNJANUM         PIC X(08).
                    11  ICINFRES-JKYSNUM          PIC X(20).
              05  ICINFRES-MISYU-TOTAL            PIC X(10).
              05  ICINFRES-MISYU-OVER             PIC X(05).
              05  ICINFRES-MISYU-AREA      OCCURS 50.
                07  ICINFRES-MISYU-SRYYMD         PIC X(10).
                07  ICINFRES-MISYU-NYUGAIKBN      PIC X(1).
                07  ICINFRES-MISYU-DENPNUM        PIC X(7).
                07  ICINFRES-MISYU                PIC X(10).
