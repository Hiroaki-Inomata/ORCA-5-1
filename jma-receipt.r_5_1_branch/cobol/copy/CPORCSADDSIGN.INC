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
      *    検索記号    CPORCSADDSIGN.INC
      *****************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       01  ORCSADDSIGNAREA.
      *    ------------------------------------------------------------
           03  SADDSIGN-KBN        PIC X(01).
      *    SPACE:PTNに従ってSTRに"%"を付加
      *        1:STRの"＊"を"%"に置き換え
      *    ------------------------------------------------------------
           03  SADDSIGN-PTN        PIC X(01).
      *    1:STRの後ろに"%"を追加
      *    2:STRの前に"%"を追加
      *    3:STRの前後に"%"を追加
      *    OTHER:1に準ずる
      *    ------------------------------------------------------------
           03  SADDSIGN-STR        PIC X(1000).
      *
