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
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev	修正者	日付		内容
      *  01.00.01    吉川      2007/05/17   帳票様式変更に伴う修正と
      *                                     仕様変更に伴う修正
      *  01.00.02    吉川      2007/10/05   システム管理マスタ「1901」の
      *                                     医療機関情報を参照
      *  01.00.03    萩野      2018/11/07   修正履歴コメント削除
      *****************************************************************
        01  SC1803.
            02  SC1803-HOSPCD   PIC X(20).
            02  SC1803-SRYYM    PIC X(16).
            02  SC1803-HOSPADRS PIC X(100).
            02  SC1803-HOSPNAME PIC X(100).
            02  SC1803-KOH-TBL          OCCURS  6 TIMES.
              03  SC1803-DATA-TBL         OCCURS  18 TIMES.
                04  SC1803-KENSU    PIC X(5).
                04  SC1803-ITBFTN   PIC X(7).
