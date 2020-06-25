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
      *    退院請求金額情報取得
      *    COPY (CPORCSNINF02.INC)
      ******************************************************************
       01  SNINF02-AREA.
           03  SNINF02-IN-SKYSTYMD                          PIC X(08).
           03  SNINF02-IN-SKYEDYMD                          PIC X(08).
      *
           03  SNINF02-SKYINF-AREA.
               05  SNINF02-SKYINF-MAX                       PIC 9(05).
               05  SNINF02-SKYINF-OCC               OCCURS  13.
                       07  SNINF02-SKYINF-KBN-G.
                           09  SNINF02-SKYINF-KBN-LBL       PIC X(20).
                           09  SNINF02-SKYINF-KBN-DATA      PIC X(01).
                           09  SNINF02-SKYINF-KBN-NAME      PIC X(20).
                       07  SNINF02-SRYYM                    PIC X(07).
                       07  SNINF02-DENPNUM                  PIC X(07).
                       07  SNINF02-SRYKA-G.
                           09  SNINF02-SRYKA                PIC X(02).
                           09  SNINF02-SRYKANAME            PIC X(30).
                       07  SNINF02-HKNINF-AREA.
                           09  SNINF02-HKNCOMBI             PIC X(04).
                           09  SNINF02-HKNNUM               PIC X(03).
                           09  SNINF02-HKNJANUM             PIC X(08).
                           09  SNINF02-HKNJANAME            PIC X(20).
                           09  SNINF02-HKIGO                PIC X(80).
                           09  SNINF02-HNUM                 PIC X(80).
                           09  SNINF02-KOHINF-AREA.
                               11  SNINF02-KOHINF-OCC       OCCURS 4.
                                   13  SNINF02-KOHNUM       PIC X(03).
                                   13  SNINF02-KOHNAME      PIC X(20).
                                   13  SNINF02-FTNJANUM     PIC X(08).
                                   13  SNINF02-JKYSNUM      PIC X(20).
                       07  SNINF02-PTFTN-AREA.
                           09  SNINF02-SKYMONEY             PIC X(10).
                           09  SNINF02-HKNMONEY             PIC X(10).
                           09  SNINF02-JIHIMONEY            PIC X(10).
                           09  SNINF02-RJNMONEY             PIC X(10).
                           09  SNINF02-KOHMONEY             PIC X(10).
                           09  SNINF02-SKJMONEY             PIC X(10).
                           09  SNINF02-RSIMONEY             PIC X(10).
                           09  SNINF02-GENMEN               PIC X(10).
                       07  SNINF02-TENSU-AREA.
                           09  SNINF02-TOTAL-TEN            PIC X(10).
                           09  SNINF02-SRYKBN-OCC           OCCURS 16.
                               11  SNINF02-SRYKBN-NAME      PIC X(30).
                               11  SNINF02-SRYKBN-TENSU     PIC X(10).
                       07  SNINF02-SKJRYOYO                 PIC X(10).
