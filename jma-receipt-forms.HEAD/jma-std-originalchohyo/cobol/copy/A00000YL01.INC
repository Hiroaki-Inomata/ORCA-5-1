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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ******************************************************************
      *  Maj/Min/Rev  修正者  日付       内容
      *   01.00.01    門間    12/05/01   新規作成
      ******************************************************************
       01  YL01.
      *    優先順位（１～５）
           02  YL01-TITLE1     PIC X(90).
      *    パラメタ設定（６～９）
           02  YL01-TITLE2     PIC X(100).
      *    作成年月日
           02  YL01-PRTYMD     PIC X(22).
      *    頁数
           02  YL01-PAGE       PIC X(4).
      *    合計頁数
           02  YL01-GPAGE      PIC X(4).
      *
           02  YL01-TBL        OCCURS  34 TIMES.
      *      診療行為コード
             03  YL01-SRYCD      PIC X(9).
      *      入力コード
             03  YL01-INPUTCD    PIC X(10).
      *      区分
             03  YL01-YKZKBN     PIC X(2).
      *      【般】
             03  YL01-KISAIKBN   PIC X(2).
      *      医薬品名
             03  YL01-NAME       PIC X(64).
      *      一般名
             03  YL01-GNAME      PIC X(64).
      *      薬価
             03  YL01-YAKKA      PIC X(9).
      *      最低薬価
             03  YL01-SAIYAKKA   PIC X(9).
