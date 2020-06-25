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
      ************************************************************
      *    薬剤情報画像編集パラメタ　　   COPY (CPORCSMIAREA.INC)
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  ORCSMIAREA.
      *診療コード
           03  ORCSMI-SRYCD        PIC X(09).
      *画像区分（1:裸体、2:ＰＴＰ包装、3:その他）
           03  ORCSMI-IMAGEKBN     PIC 9(01).
      *画像ファイル一時領域のカレント
           03  ORCSMI-TEMPDIR      PIC X(1024).
      *画像ファイル名
           03  ORCSMI-FILENAME     PIC X(128).
      *結果コード（0:成功、0以外:エラー）
           03  ORCSMI-RETCD        PIC 9(01).
      *
