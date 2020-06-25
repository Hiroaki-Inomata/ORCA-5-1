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
      *    ＡＰＩレスポンス入院登録情報 
      *    COPY (CPHSOUTMODV2RES.INC)
      ******************************************************************
       01  XML-HSPTINFM02RES.
           03  HSPTINFM02RES-CONTEXT              PIC S9(9)   BINARY.
           03  HSPTINFM02RES-OBJECT               PIC X(36).
           03  HSPTINFM02RES-MODE                 PIC S9(9)   BINARY.
           03  HSPTINFM02RES-RECORDNAME           PIC X(128).
           03  HSPTINFM02RES-PRIVATE-OBJECTS.
               05  HSPTINFM02RES-INFORMATION-DATE     PIC X(10).
               05  HSPTINFM02RES-INFORMATION-TIME     PIC X(8).
               05  HSPTINFM02RES-API-RESULT-OCC       OCCURS  10.
                   07  HSPTINFM02RES-API-RESULT       PIC X(4).
                   07  HSPTINFM02RES-API-RESULT-MSG   PIC X(100).
               05  HSPTINFM02RES-REQNUM-G.
                   07  HSPTINFM02RES-REQNUM-LBL       PIC X(20).
                   07  HSPTINFM02RES-REQNUM-DATA      PIC X(02).
                   07  HSPTINFM02RES-REQNUM-NAME      PIC X(30).
               05  HSPTINFM02RES-SKYSTYMD                 PIC X(10).
               05  HSPTINFM02RES-SKYEDYMD                 PIC X(10).
               05  HSPTINFM02RES-UNSKYSTYMD             PIC X(10).
               05  HSPTINFM02RES-UNSKYEDYMD             PIC X(10).
               05  HSPTINFM02RES-PTINF-AREA.
                   07  HSPTINFM02RES-PTNUM                PIC X(20).
                   07  HSPTINFM02RES-NAME                 PIC X(100).
                   07  HSPTINFM02RES-KANANAME             PIC X(100).
                   07  HSPTINFM02RES-BIRTHDAY             PIC X(10).
                   07  HSPTINFM02RES-SEX                  PIC X(01).
               05  HSPTINFM02RES-RRKNUM                   PIC X(03).
               05  HSPTINFM02RES-NYUINYMD                 PIC X(10).
               05  HSPTINFM02RES-SKYINF-AREA.
                   07  HSPTINFM02RES-SKYINF-OCC           OCCURS  13.
                       09  HSPTINFM02RES-SKYINF-KBN-G.
                           11  HSPTINFM02RES-SKYINF-KBN-LBL   PIC X(20).
                           11  HSPTINFM02RES-SKYINF-KBN-DATA  PIC X(01).
                           11  HSPTINFM02RES-SKYINF-KBN-NAME  PIC X(20).
                       09  HSPTINFM02RES-SRYYM            PIC X(07).
                       09  HSPTINFM02RES-DENPNUM          PIC X(07).
                       09  HSPTINFM02RES-SRYKA-G.
                           11  HSPTINFM02RES-SRYKA        PIC X(02).
                           11  HSPTINFM02RES-SRYKANAME    PIC X(30).
                       09  HSPTINFM02RES-HKNINF-AREA.
                           11  HSPTINFM02RES-HKNCOMBI       PIC X(04).
                           11  HSPTINFM02RES-HKNNUM         PIC X(03).
                           11  HSPTINFM02RES-HKNJANUM       PIC X(08).
                           11  HSPTINFM02RES-HKNJANAME      PIC X(20).
                           11  HSPTINFM02RES-HKIGO          PIC X(80).
                           11  HSPTINFM02RES-HNUM           PIC X(80).
                           11  HSPTINFM02RES-KOHINF-AREA.
                               13  HSPTINFM02RES-KOHINF-OCC   OCCURS 4.
                                   15  HSPTINFM02RES-KOHNUM   PIC X(03).
                                   15  HSPTINFM02RES-KOHNAME  PIC X(20).
                                   15  HSPTINFM02RES-FTNJANUM PIC X(08).
                                   15  HSPTINFM02RES-JKYSNUM  PIC X(20).
                       09  HSPTINFM02RES-PTFTN-AREA.
                           11  HSPTINFM02RES-SKYMONEY      PIC X(10).
                           11  HSPTINFM02RES-HKNMONEY      PIC X(10).
                           11  HSPTINFM02RES-JIHIMONEY     PIC X(10).
                           11  HSPTINFM02RES-RJNMONEY      PIC X(10).
                           11  HSPTINFM02RES-KOHMONEY      PIC X(10).
                           11  HSPTINFM02RES-SKJMONEY      PIC X(10).
                           11  HSPTINFM02RES-RSIMONEY      PIC X(10).
                           11  HSPTINFM02RES-GENMEN        PIC X(10).
                       09  HSPTINFM02RES-TENSU-AREA.
                           11  HSPTINFM02RES-TOTAL-TEN        PIC X(10).
                           11  HSPTINFM02RES-SRYKBN-OCC       OCCURS 16.
                               13  HSPTINFM02RES-SRYKBN-NAME  PIC X(30).
                               13  HSPTINFM02RES-SRYKBN-TENSU PIC X(10).
                       09  HSPTINFM02RES-SKJRYOYO             PIC X(10).
