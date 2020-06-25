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
      *  手術歴レセプトコメントマスター       (CPOPECOM.INC)
      *****************************************************************
      *  修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
      *01  OPECOM-REC.
           03  OPECOM-KEY.
      *医療機関ＩＤ
               05  OPECOM-HOSPNUM        PIC 9(2).
      *患者ＩＤ
               05  OPECOM-PTID          PIC 9(10).
      *手術年月日
               05  OPECOM-SRYYMD        PIC X(08).
      *診療コード
               05  OPECOM-SRYCD         PIC X(09).
      *連番
               05  OPECOM-RENNUM        PIC 9(03).
      *手術名
           03  OPECOM-OPENAME           PIC X(100).
      *コメント
           03  OPECOM-OPECOMMENT        PIC X(100).
      *入外区分（１：入院　２：入院外）
           03  OPECOM-NYUGAIKBN         PIC X(01).
      *診療科
           03  OPECOM-SRYKA             PIC X(02).
      *保険番号
           03  OPECOM-HKNNUM            PIC X(03).
      *労災保険区分（１：短期給付　２：傷病年金　３：アフターケア）
           03  OPECOM-HKNKBN            PIC X(01).
      *保険ＩＤ
           03  OPECOM-HKNID             PIC 9(10).
      *保険組合せ
           03  OPECOM-HKNCOMBI          PIC 9(04).
      *有効開始年月
           03  OPECOM-TEKSTYM           PIC X(06).
      *有効終了年月
           03  OPECOM-TEKEDYM           PIC X(06).
      *端末ＩＤ
           03  OPECOM-TERMID            PIC X(16).
      *オペーレータＩＤ
           03  OPECOM-OPID              PIC X(16).
      *作成年月日
           03  OPECOM-CREYMD            PIC X(08).
      *更新年月日
           03  OPECOM-UPYMD             PIC X(08).
      *更新時間
           03  OPECOM-UPHMS             PIC X(06).
