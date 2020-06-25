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
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  05.01.01    門間         19/08/01  新規作成
      *  05.01.02    萩野         19/11/14  カナ名の桁数増加
      ******************************************************************
       01  SC07.
           03  SC07-DATAKBN                 PIC  X(01).
           03  SC07-SEIGYM.
               05  SC07-SEIG                PIC  X(01).
               05  SC07-SEIYY               PIC  X(02).
               05  SC07-SEIMM               PIC  X(02).
           03  SC07-SEIYMD                  PIC  X(22).
           03  SC07-HOSPCD                  PIC  X(07).
           03  SC07-HOSPNAME                PIC  X(100).
           03  SC07-TEL                     PIC  X(15).
           03  SC07-PAGE                    PIC  X(03).
           03  SC07-GPAGE                   PIC  X(03).
           03  SC07-MEISAI-TBL  OCCURS  13 TIMES.
               05  SC07-FTNJANUM            PIC  X(08).
               05  SC07-JKYSNUM             PIC  X(10).
               05  SC07-HKNKBN              PIC  X(01).
               05  SC07-HKNJANUM            PIC  X(08).
               05  SC07-KANANAME            PIC  X(30).
               05  SC07-KANANAME2           PIC  X(50).
               05  SC07-TANJOYMD.
                   07  SC07-TANJOG          PIC  X(01).
                   07  SC07-TANJOYY         PIC  X(02).
                   07  SC07-TANJOMM         PIC  X(02).
                   07  SC07-TANJODD         PIC  X(02).
               05  SC07-SEX                 PIC  X(01).
               05  SC07-NYUGAIKBN           PIC  X(01).
               05  SC07-KYURATE             PIC  X(01).
               05  SC07-SRYYM.
                   07  SC07-SRYG            PIC  X(01).
                   07  SC07-SRYYY           PIC  X(02).
                   07  SC07-SRYMM           PIC  X(02).
               05  SC07-NISSU               PIC  X(02).
               05  SC07-TENSU               PIC  X(07).
               05  SC07-ITBFTN              PIC  X(07).
               05  SC07-HBTNUM1             PIC  X(02).
               05  SC07-HBTNUM2             PIC  X(02).
               05  SC07-TOKKI1              PIC  X(02).
               05  SC07-TOKKI2              PIC  X(02).
