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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ******************************************************************
      *  Maj/Min/Rev  修正者  日付       内容
      *   01.00.01    門間    12/05/01   新規作成
      ******************************************************************
       01  YL01TMP.
           02  YL01TMP-KEY.
      *      優先順位（１～５）
             03  YL01TMP-KEY-TBL      OCCURS 5.
               04  YL01TMP-KEY-D        PIC X(64).
      *      医薬品名
             03  YL01TMP-NAME         PIC X(64).
      *      診療行為コード
             03  YL01TMP-SRYCD        PIC X(9).
      *    入力コード
           02  YL01TMP-INPUTCD      PIC X(10).
      *    薬剤区分
           02  YL01TMP-YKZKBN       PIC X(2).
      *    一般名記載区分
           02  YL01TMP-KISAIKBN     PIC X(2).
      *    一般名
           02  YL01TMP-GNAME        PIC X(64).
      *    薬価
           02  YL01TMP-YAKKA        PIC X(9).
      *    最低薬価
           02  YL01TMP-SAIYAKKA     PIC X(9).
