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
      ********************************************************************
      *    プリンター名取得パラメタ      COPY (CPORCSPRTNMGET.INC)
      *                                  使用プログラム (ORCSPRTNMGET)
      *    設定項目
      *      対象日
      *****************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
       01  ORCSPRTNMGETAREA.
      *
      *    対象日
           03  PRTNMGET-YMD                PIC 9(08).
      *    プリンター名
           03  PRTNMGET-PRTNM-TBL              OCCURS  100.
               05  PRTNMGET-PRTNM          PIC X(16).
      *
           03  PRTNMGET-PRTNM-MAX          PIC 9(03).
      *
      *    復帰コード
           03  PRTNMGET-RC                 PIC 9(04).
      *
