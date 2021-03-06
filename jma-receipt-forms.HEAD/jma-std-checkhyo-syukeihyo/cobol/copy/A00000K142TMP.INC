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
       01  K142TMP-REC.
           03  K142TMP-KEY.
               05  K142TMP-NYUGAIKBN       PIC  X(01).
               05  K142TMP-AGEINDEX        PIC  9(03).
           03  K142TMP-SRY-TBL OCCURS 56.
               05  K142TMP-TENSU           PIC  9(11).
               05  K142TMP-KAISU           PIC  9(11).
