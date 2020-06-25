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
      *******************************************************************
      *  プログラム修正履歴
      *  Maj/Min/Rev  修正者      日付      内容
      *   01.00.01    @@@@-田中   10/12/14  コメント履歴削除
      *******************************************************************
       01  K100.
           03  K100-TITLE.
               05  K100-SUBTITLE       PIC X(30).
               05  K100-JOBMSG         PIC X(60).
               05  K100-PAGE           PIC X(03).
               05  K100-SYSYMD         PIC X(22).
               05  K100-SRYYM          PIC X(16).
               05  K100-NYUGAIKBN      PIC X(04).
               05  K100-NENREIKBN      PIC X(18)  OCCURS 2.
               05  K100-NENREIKBN2     PIC X(12)  OCCURS 2.
               05  K100-TJIHI-G        OCCURS 10.
                   07  K100-TJIHI      PIC X(20).
           03 K100-DATA.
              05  K100-TNAME-G         OCCURS 6.
                  07  K100-TNAME       PIC X(20).
              05  K100-YOSO-G          OCCURS 6.
                  07  K100-TENSU-OCC   OCCURS 56.
                      09  K100-TENSU   PIC X(11).
                  07  K100-KAISU-OCC   OCCURS 56.
                      09  K100-KAISU   PIC X(11).
