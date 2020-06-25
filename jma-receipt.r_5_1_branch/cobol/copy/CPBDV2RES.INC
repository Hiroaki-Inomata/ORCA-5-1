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
      *    ＡＰＩレスポンス出産育児一時金
      *    COPY (CPBDV2RES.INC)
      ******************************************************************
       01  XML-BDRES.
           03  BDRES-CONTEXT                      PIC S9(9)   BINARY.
           03  BDRES-OBJECT                       PIC X(36).
           03  BDRES-MODE                         PIC S9(9)   BINARY.
           03  BDRES-RECORDNAME                   PIC X(128).
           03  BDRES-PRIVATE-OBJECTS.
               05  BDRES-INFORMATION-DATE              PIC X(10).
               05  BDRES-INFORMATION-TIME              PIC X(8).
               05  BDRES-API-RESULT-OCC                OCCURS  10.
                   07  BDRES-API-RESULT                PIC X(4).
                   07  BDRES-API-RESULT-MSG            PIC X(100).
               05  BDRES-REQNUM-G.
                   07  BDRES-REQNUM-LBL                PIC X(20).
                   07  BDRES-REQNUM-DATA               PIC X(02).
                   07  BDRES-REQNUM-NAME               PIC X(30).
               05  BDRES-PTINF-AREA. 
                   07  BDRES-PTNUM                     PIC X(20).
                   07  BDRES-NAME                      PIC X(100).
                   07  BDRES-KANANAME                  PIC X(100).
                   07  BDRES-BIRTHDAY                  PIC X(10).
                   07  BDRES-SEX                       PIC X(01).
               05  BDRES-BUNBEN-INF-AREA.
                   07  BDRES-RRKNUM                    PIC X(03).
                   07  BDRES-NYUINYMD                  PIC X(10).
                   07  BDRES-TAIINYMD                  PIC X(10).
                   07  BDRES-RIYOUKBN-G.
                       09  BDRES-RIYOUKBN-LBL          PIC X(20).
                       09  BDRES-RIYOUKBN-DATA         PIC X(1).
                       09  BDRES-RIYOUKBN-NAME         PIC X(20).
                   07  BDRES-SKYYM-G.
                       09  BDRES-SKYYM-LBL             PIC X(20).
                       09  BDRES-SKYYM-DATA            PIC X(7).
                   07  BDRES-TEISYUTUBIKBN-G.
                       09  BDRES-TEISYUTUBIKBN-LBL     PIC X(20).
                       09  BDRES-TEISYUTUBIKBN-DATA    PIC X(1).
                       09  BDRES-TEISYUTUBIKBN-NAME    PIC X(20).
                   07  BDRES-SKYKBN-G.
                       09  BDRES-SKYKBN-LBL            PIC X(20).
                       09  BDRES-SKYKBN-DATA           PIC X(1).
                       09  BDRES-SKYKBN-NAME           PIC X(20).
                   07  BDRES-BUNBENKBN-G.
                       09  BDRES-BUNBENKBN-LBL         PIC X(20).
                       09  BDRES-BUNBENKBN-DATA        PIC X(1).
                       09  BDRES-BUNBENKBN-NAME        PIC X(20).
                   07  BDRES-TEISYUTUSAKI-G.
                       09  BDRES-TEISYUTUSAKI-LBL      PIC X(20).
                       09  BDRES-TEISYUTUSAKI-DATA     PIC X(1).
                       09  BDRES-TEISYUTUSAKI-NAME     PIC X(20).
                   07  BDRES-ZAIWEEK-G.
                       09  BDRES-ZAIWEEK-LBL           PIC X(20).
                       09  BDRES-ZAIWEEK-DATA          PIC X(2).
                       09  BDRES-ZAIWEEK-NAME          PIC X(10).
                   07  BDRES-BIRTHDAY-G.
                       09  BDRES-BIRTHDAY-LBL          PIC X(20).
                       09  BDRES-BIRTHDAY-DATA         PIC X(10).
                   07  BDRES-SIZANKBN-G.
                       09  BDRES-SIZANKBN-LBL          PIC X(20).
                       09  BDRES-SIZANKBN-DATA         PIC X(1).
                       09  BDRES-SIZANKBN-NAME         PIC X(10).
                   07  BDRES-BIRTHCNT-G.
                       09  BDRES-BIRTHCNT-LBL          PIC X(10).
                       09  BDRES-BIRTHCNT-DATA         PIC X(2).
                       09  BDRES-BIRTHCNT-NAME         PIC X(10).
                   07  BDRES-NYUIN-DAY-G.
                       09  BDRES-NYUIN-DAY-LBL         PIC X(10).
                       09  BDRES-NYUIN-DAY-DATA        PIC X(3).
                       09  BDRES-NYUIN-DAY-NAME        PIC X(10).
                   07  BDRES-SANKAKBN-G.
                       09  BDRES-SANKAKBN-LBL          PIC X(20).
                       09  BDRES-SANKAKBN-DATA         PIC X(1).
                       09  BDRES-SANKAKBN-NAME         PIC X(20).
                   07  BDRES-NYU-MONEY-G.
                       09  BDRES-NYU-MONEY-LBL         PIC X(20).
                       09  BDRES-NYU-MONEY-DATA        PIC X(7).
                       09  BDRES-NYU-MONEY-NAME        PIC X(20).
                   07  BDRES-RMSGK-MONEY-G.
                       09  BDRES-RMSGK-MONEY-LBL       PIC X(20).
                       09  BDRES-RMSGK-MONEY-DATA      PIC X(7).
                       09  BDRES-RMSGK-MONEY-NAME      PIC X(20).
                   07  BDRES-KAIJO-MONEY-G.
                       09  BDRES-KAIJO-MONEY-LBL       PIC X(20).
                       09  BDRES-KAIJO-MONEY-DATA      PIC X(7).
                       09  BDRES-KAIJO-MONEY-NAME      PIC X(20).
                   07  BDRES-BUNBEN-MONEY-G.
                       09  BDRES-BUNBEN-MONEY-LBL      PIC X(20).
                       09  BDRES-BUNBEN-MONEY-DATA     PIC X(7).
                       09  BDRES-BUNBEN-MONEY-NAME     PIC X(20).
                   07  BDRES-SHIN-MONEY-G.
                       09  BDRES-SHIN-MONEY-LBL        PIC X(20).
                       09  BDRES-SHIN-MONEY-DATA       PIC X(7).
                       09  BDRES-SHIN-MONEY-NAME       PIC X(20).
                   07  BDRES-KNS-MONEY-G.
                       09  BDRES-KNS-MONEY-LBL         PIC X(20).
                       09  BDRES-KNS-MONEY-DATA        PIC X(7).
                       09  BDRES-KNS-MONEY-NAME        PIC X(20).
                   07  BDRES-SYC-MONEY-G.
                       09  BDRES-SYC-MONEY-LBL         PIC X(20).
                       09  BDRES-SYC-MONEY-DATA        PIC X(7).
                       09  BDRES-SYC-MONEY-NAME        PIC X(20).
                   07  BDRES-SNK-MONEY-G.
                       09  BDRES-SNK-MONEY-LBL         PIC X(20).
                       09  BDRES-SNK-MONEY-DATA        PIC X(7).
                       09  BDRES-SNK-MONEY-NAME        PIC X(20).
                   07  BDRES-ETC-MONEY-G.
                       09  BDRES-ETC-MONEY-LBL         PIC X(20).
                       09  BDRES-ETC-MONEY-DATA        PIC X(7).
                       09  BDRES-ETC-MONEY-NAME        PIC X(20).
                   07  BDRES-ICBFTN-MONEY-G.
                       09  BDRES-ICBFTN-MONEY-LBL      PIC X(20).
                       09  BDRES-ICBFTN-MONEY-DATA     PIC X(7).
                       09  BDRES-ICBFTN-MONEY-NAME     PIC X(20).
                   07  BDRES-NINPUFTN-MONEY-G.
                       09  BDRES-NINPUFTN-MONEY-LBL      PIC X(20).
                       09  BDRES-NINPUFTN-MONEY-DATA     PIC X(7).
                       09  BDRES-NINPUFTN-MONEY-NAME     PIC X(20).
                   07  BDRES-DAIRI-MONEY-G.
                       09  BDRES-DAIRI-MONEY-LBL       PIC X(20).
                       09  BDRES-DAIRI-MONEY-DATA      PIC X(7).
                       09  BDRES-DAIRI-MONEY-NAME      PIC X(20).
                   07  BDRES-BIKOU-G.
                       09  BDRES-BIKOU-LBL             PIC X(10).
                       09  BDRES-BIKOU-DATA            PIC X(60).
                   07  BDRES-HKNJANUM                  PIC  X(08).
                   07  BDRES-HKNJANAME                 PIC  X(100).
                   07  BDRES-KIGO                      PIC  X(38).
                   07  BDRES-NUM                       PIC  X(38).
                   07  BDRES-HONKZKKBN                 PIC  X(01).
