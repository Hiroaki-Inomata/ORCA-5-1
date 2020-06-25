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
      *  薬剤情報       (CPYAKUJYO.INC) 
      *****************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  YAKUJYO-REC.
           03  YAKUJYO-KEY.
      *医療機関ＩＤ
               05  YAKUJYO-HOSPNUM          PIC 9(2).
      *医薬品コード
               05  YAKUJYO-SRYCD           PIC X(09).
      *適用番号
               05  YAKUJYO-TEKNUM          PIC 9(02).
      *医薬品名称
           03  YAKUJYO-NAME                PIC X(170).
      *効能効果
           03  YAKUJYO-EFFECT              PIC X(2000).
      *注意事項
           03  YAKUJYO-CAUTION             PIC X(4000).
      *画像ファイル名
           03  YAKUJYO-PHOTO               PIC X(256).
      *色
           03  YAKUJYO-COLOR               PIC X(360).
      *形
           03  YAKUJYO-SHAPE               PIC X(360).
      *記号
           03  YAKUJYO-MARK                PIC X(360).
      *端末ＩＤ
           03  YAKUJYO-TERMID              PIC  X(32).
      *オペーレータＩＤ
           03  YAKUJYO-OPID                PIC  X(16).
      *作成年月日
           03  YAKUJYO-CREYMD              PIC  X(08).
      *更新年月日
           03  YAKUJYO-UPYMD               PIC  X(08).
      *更新時間
           03  YAKUJYO-UPHMS               PIC  X(06).
