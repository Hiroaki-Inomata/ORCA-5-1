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
      *    ＡＰＩレスポンス退院仮計算
      *    COPY (CPHSACSIMULATEV2RES.INC)
      ******************************************************************
       01  XML-HSACSIMULATERES.
           03  HSACSRES-CONTEXT                  PIC S9(9)   BINARY.
           03  HSACSRES-OBJECT                   PIC X(36).
           03  HSACSRES-MODE                     PIC S9(9)   BINARY.
           03  HSACSRES-RECORDNAME               PIC X(128).
      *
           03  HSACSRES-PRIVATE-OBJECTS.
               05  HSACSRES-INFORMATION-DATE         PIC X(10).
               05  HSACSRES-INFORMATION-TIME         PIC X(8).
               05  HSACSRES-API-RESULT               PIC X(2).
               05  HSACSRES-API-RESULT-MSG           PIC X(100).
               05  HSACSRES-SKYSTYMD                 PIC X(10).
               05  HSACSRES-SKYEDYMD                 PIC X(10).
               05  HSACSRES-PTINF-AREA.
                   07  HSACSRES-PTNUM                PIC X(20).
                   07  HSACSRES-NAME                 PIC X(100).
                   07  HSACSRES-KANANAME             PIC X(100).
                   07  HSACSRES-BIRTHDAY             PIC X(10).
                   07  HSACSRES-SEX                  PIC X(01).
               05  HSACSRES-NYUINYMD                 PIC X(10).
               05  HSACSRES-SKYINF-AREA.
                   07  HSACSRES-SKYINF-OCC           OCCURS  13.
                       09  HSACSRES-SKYINF-KBN-G.
                           11  HSACSRES-SKYINF-KBN-LBL   PIC X(20).
                           11  HSACSRES-SKYINF-KBN-DATA  PIC X(01).
                           11  HSACSRES-SKYINF-KBN-NAME  PIC X(20).
                       09  HSACSRES-SRYYM            PIC X(07).
                       09  HSACSRES-SRYKA-G.
                           11  HSACSRES-SRYKA        PIC X(02).
                           11  HSACSRES-SRYKANAME    PIC X(30).
                       09  HSACSRES-HKNINF-AREA.
                           11  HSACSRES-HKNCOMBI            PIC X(04).
                           11  HSACSRES-HKNNUM              PIC X(03).
                           11  HSACSRES-HKNJANUM            PIC X(08).
                           11  HSACSRES-HKNJANAME           PIC X(20).
                           11  HSACSRES-HKIGO               PIC X(80).
                           11  HSACSRES-HNUM                PIC X(80).
                           11  HSACSRES-KOHINF-AREA.
                               13  HSACSRES-KOHINF-OCC      OCCURS 4.
                                   15  HSACSRES-KOHNUM      PIC X(03).
                                   15  HSACSRES-KOHNAME     PIC X(20).
                                   15  HSACSRES-FTNJANUM    PIC X(08).
                                   15  HSACSRES-JKYSNUM     PIC X(20).
                       09  HSACSRES-PTFTN-AREA.
                           11  HSACSRES-SKYMONEY           PIC X(10).
                           11  HSACSRES-HKNMONEY           PIC X(10).
                           11  HSACSRES-JIHIMONEY          PIC X(10).
                           11  HSACSRES-RJNMONEY           PIC X(10).
                           11  HSACSRES-KOHMONEY           PIC X(10).
                           11  HSACSRES-SKJMONEY           PIC X(10).
                           11  HSACSRES-RSIMONEY           PIC X(10).
                           11  HSACSRES-GENMEN             PIC X(10).
                       09  HSACSRES-TENSU-AREA.
                           11  HSACSRES-TOTAL-TEN          PIC X(10).
                           11  HSACSRES-SRYKBN-OCC         OCCURS 16.
                               13  HSACSRES-SRYKBN-NAME    PIC X(30).
                               13  HSACSRES-SRYKBN-TENSU   PIC X(10).
                       09  HSACSRES-SKJRYOYO               PIC X(10).
