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
      * Maj/Min/Rev  修正者  日付        内容
      *  01.00.01    吉川    2013/10/08  新規作成
      *  01.00.02    吉川    2013/12/02  機関区分追加
      ******************************************************************
       01  SC11.
           03  SC11-PRTYM                   PIC  X(16).
           03  SC11-SEIYMD                  PIC  X(22).
           03  SC11-HOSPCD                  PIC  X(07).
           03  SC11-ADRS                    PIC  X(100).
           03  SC11-HOSPNAME                PIC  X(100).
           03  SC11-KAISETUNAME             PIC  X(40).
           03  SC11-TEL                     PIC  X(15).
           03  SC11-PAGE                    PIC  X(03).
           03  SC11-GPAGE                   PIC  X(03).
           03  SC11-MEISAI-TBL  OCCURS  10 TIMES.
               05  SC11-JIGYO               PIC  X(02).
               05  SC11-HBTNUM1             PIC  X(04).
               05  SC11-HBTNUM2             PIC  X(04).
               05  SC11-TOKKI1              PIC  X(04).
               05  SC11-TOKKI2              PIC  X(04).
               05  SC11-JKYSNUM             PIC  X(20).
               05  SC11-HKNKBN              PIC  X(02).
               05  SC11-HKNJANUM            PIC  X(16).
               05  SC11-KANANAME            PIC  X(20).
               05  SC11-KANANAME2           PIC  X(40).
               05  SC11-SEX                 PIC  X(02).
               05  SC11-TANJOYMD.
                   07  SC11-TANJOG          PIC  X(02).
                   07  SC11-TANJOYY         PIC  X(04).
                   07  SC11-TANJOMM         PIC  X(04).
                   07  SC11-TANJODD         PIC  X(04).
               05  SC11-NYUGAIKBN           PIC  X(02).
               05  SC11-KYURATE             PIC  X(02).
               05  SC11-SRYYM.
                   07  SC11-SRYG            PIC  X(02).
                   07  SC11-SRYYY           PIC  X(04).
                   07  SC11-SRYMM           PIC  X(04).
               05  SC11-NISSU               PIC  X(02).
               05  SC11-TENSU               PIC  X(07).
               05  SC11-ITBFTN              PIC  X(07).
               05  SC11-SKINGK              PIC  X(07).
           03  SC11-KIKANKBN                PIC  X(02)  OCCURS  2.
