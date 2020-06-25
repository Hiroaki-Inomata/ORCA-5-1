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
      *    ＡＰＩリクエスト出産育児一時金
      *    COPY (CPBDV2REQ.INC)
      ******************************************************************
       01  XML-BDREQ.
            03  BDREQ-CONTEXT             PIC S9(9)   BINARY.
            03  BDREQ-OBJECT              PIC X(36).
            03  BDREQ-MODE                PIC S9(9)   BINARY.
            03  BDREQ-RECORDNAME          PIC X(128).
            03  BDREQ-PRIVATE-OBJECTS.
              05  BDREQ-SAVEREQ           PIC X(1).
              05  BDREQ-REQNUM            PIC X(2).
              05  BDREQ-PTNUM             PIC X(20).
              05  BDREQ-NYUINYMD          PIC X(10).
              05  BDREQ-RIYOUKBN          PIC X(1).
              05  BDREQ-SKYYM             PIC X(07).
              05  BDREQ-TEISYUTUBIKBN     PIC X(01).
              05  BDREQ-SKYKBN            PIC  X(01).
              05  BDREQ-BUNBENKBN         PIC  X(01).
              05  BDREQ-TEISYUTUSAKI      PIC  X(01).
              05  BDREQ-ZAIWEEK           PIC  X(07).
              05  BDREQ-BIRTHDAY          PIC  X(10).
              05  BDREQ-SIZANKBN          PIC  X(01).
              05  BDREQ-BIRTHCNT          PIC  X(07).
              05  BDREQ-NYUIN-DAY         PIC  X(07).
              05  BDREQ-SANKAKBN          PIC  X(01).
              05  BDREQ-NYU-MONEY         PIC  X(07).
              05  BDREQ-RMSGK-MONEY       PIC  X(07).
              05  BDREQ-KAIJO-MONEY       PIC  X(07).
              05  BDREQ-BUNBEN-MONEY      PIC  X(07).
              05  BDREQ-SHIN-MONEY        PIC  X(07).
              05  BDREQ-KNS-MONEY         PIC  X(07).
              05  BDREQ-SYC-MONEY         PIC  X(07).
              05  BDREQ-SNK-MONEY         PIC  X(07).
              05  BDREQ-ETC-MONEY         PIC  X(07).
              05  BDREQ-ICBFTN-MONEY      PIC  X(07).
              05  BDREQ-BIKOU             PIC  X(60).
              05  BDREQ-HKNCOMBI          PIC  X(04).
              05  BDREQ-HKNJANUM          PIC  X(08).
              05  BDREQ-KIGO              PIC  X(38).
              05  BDREQ-NUM               PIC  X(38).
              05  BDREQ-HONKZKKBN         PIC  X(01).
