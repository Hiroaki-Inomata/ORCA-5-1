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
      *  薬剤情報       (CPYAKUJYOTEMP.INC) 
      *****************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  TEMP-REC.
           03  TEMP-KEY.
               05  TEMP-HOSPNUM         PIC 9(2).
      *端末ＩＤ
               05  TEMP-TERMID          PIC X(64).
      *医薬品コード
               05  TEMP-SRYCD           PIC X(09).
      *ＹＪコード
               05  TEMP-YJCD            PIC X(12).
      *連番
               05  TEMP-RENNUM          PIC 9(2).
      *形
           03  TEMP-SHAPE               PIC X(360).
      *色
           03  TEMP-COLOR               PIC X(360).
      *記号
           03  TEMP-MARK                PIC X(360).
      *効能効果
           03  TEMP-EFFECT              PIC X(2000).
      *注意事項
           03  TEMP-CAUTION             PIC X(4000).
      *製造会社名
           03  TEMP-MAKE-COMPANY        PIC X(400).
      *販売会社名
           03  TEMP-SALE-COMPANY        PIC X(400).
      *画像ファイル名１
           03  TEMP-PHOTO1-FILENAME     PIC X(256).
      *画像ファイル名２
           03  TEMP-PHOTO2-FILENAME     PIC X(256).
      *画像ファイル名３
           03  TEMP-PHOTO3-FILENAME     PIC X(256).
      *作成年月日
           03  TEMP-CREYMD              PIC  X(08).
      *名称
           03  TEMP-NAME                PIC X(200).
