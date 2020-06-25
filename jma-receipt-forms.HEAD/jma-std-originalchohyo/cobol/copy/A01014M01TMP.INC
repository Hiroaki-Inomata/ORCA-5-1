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
      * 01.00.01     @@@@-門間    11/01/07  コメント履歴削除
      ********************************************************************
       01  M01TMP-REC.
              03  M01TMP-KEY.
                  05  M01TMP-HOSPNUM         PIC  9(02).
                  05  M01TMP-NYUGAIKBN       PIC  X(01).
                  05  M01TMP-SRYKA           PIC  X(02).
              03  M01TMP-SRY-TBL             OCCURS 47.
                  05  M01TMP-KENSU           PIC  9(06).
                  05  M01TMP-NISSU           PIC  9(04).
                  05  M01TMP-TENSU           PIC  9(09).
                  05  M01TMP-SYUEKI          PIC  9(09).
                  05  M01TMP-DAY-TBL.
                      07  M01TMP-DAY  OCCURS 31 PIC 9(01).
