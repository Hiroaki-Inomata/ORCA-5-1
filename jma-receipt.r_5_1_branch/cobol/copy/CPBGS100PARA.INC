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
      ************************************************************
      *    新保険別請求チェック表パラメタファイル                *
      *    COPY  (CPBGS100PARA.INC)                              *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
           03  BGS100P-JOBID           PIC 9(07).
           03  BGS100P-SHELLID         PIC X(08).
           03  BGS100P-MEISAI          PIC 9(01).
           03  BGS100P-HOSPNUM         PIC 9(02).
           03  BGS100P-ERRFNAME        PIC X(1024).
           03  BGS100P-FORMKBN         PIC X(1).
           03  BGS100P-SRYYM           PIC X(6).
           03  BGS100P-SRYYMD          PIC X(8).
           03  BGS100P-NYUGAIKBN       PIC X(1).
           03  BGS100P-SKYFNAME        PIC X(1024).
           03  BGS100P-KOHFNAME        PIC X(1024).
           03  BGS100P-HKNJAFNAME      PIC X(1024).
           03  BGS100P-ETCFNAME        PIC X(1024).
           03  BGS100P-MEIFNAME        PIC X(1024).
           03  BGS100P-CSVDIR          PIC X(1024).
           03  BGS100P-CSVFNAME        PIC X(1024).
           03  BGS100P-SHELLMSG        PIC X(100).
           03  BGS100P-SYAHO           PIC 9(01).
           03  BGS100P-KOKUHO          PIC 9(01).
           03  BGS100P-KOUKI           PIC 9(01).
           03  BGS100P-TOKUYO          PIC 9(01).
           03  BGS100P-ROUSAI          PIC 9(01).
           03  BGS100P-JIBAI           PIC 9(01).
           03  BGS100P-DAISAN          PIC 9(01).
           03  BGS100P-KOUGAI          PIC 9(01).
           03  BGS100P-SOUKATU         PIC 9(01).
           03  BGS100P-HOKEN           PIC 9(02).
           03  BGS100P-EDITKBN         PIC X(01).
           03  BGS100P-ETCGETKBN       PIC X(01).
           03  BGS100P-SKYGET-PARA     PIC X(01).
           03  BGS100P-HKNKBN-G.
               05  BGS100P-HKNKBN-HKN  PIC X(01).
               05  BGS100P-HKNKBN      PIC X(02).
           03  BGS100P-HKNJANUM        PIC X(08).
           03  BGS100P-SYUKA           PIC 9(01).
           03  BGS100P-SYUKA-HKN-G.
               05  BGS100P-SYUKA-SYA   PIC 9(01).
               05  BGS100P-SYUKA-KOK   PIC 9(01).
               05  BGS100P-SYUKA-KOU   PIC 9(01).
           03  BGS100P-SYUKA-HKN-R     REDEFINES   BGS100P-SYUKA-HKN-G.
               05  BGS100P-SYUKA-HKN   PIC 9(01)
                                       OCCURS  3.
           03  BGS100P-RECEKA-G.
               05  BGS100P-RECEKA-MAX  PIC S9(03).
               05  BGS100P-RECEKA-OCC  OCCURS  100.
                   07  BGS100P-RECEKA  PIC X(02).
           03  BGS100P-PAGE            PIC 9(05).
           03  BGS100P-RC              PIC 9(02).
