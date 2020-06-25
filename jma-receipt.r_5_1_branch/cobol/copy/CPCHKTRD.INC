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
      *    チェックマスタ２       COPY (CPCHKTRD.INC)                   *
      *****************************************************************
      *****************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *01  CHKTRD-REC.
           03  CHKTRD-KEY.
               05  CHKTRD-HOSPNUM        PIC 9(2).
      *チェック区分
               05  CHKTRD-CHKKBN         PIC  X(01).
      *診療コード
               05  CHKTRD-SRYCD          PIC  X(09).
      *有効開始年月日
               05  CHKTRD-YUKOSTYMD      PIC  X(08).
      *有効終了年月日
               05  CHKTRD-YUKOEDYMD      PIC  X(08).
      *コード区分
               05  CHKTRD-CDKBN          PIC  X(01).
      *連番号
               05  CHKTRD-RENNUM         PIC  9(03).
      *適用コード
           03  CHKTRD-CD                 PIC  X(9).
      *入外区分
           03  CHKTRD-NYUGAIKBN          PIC  X(1).
      *同一入外制限区分
           03  CHKTRD-NYUGAIBNDKBN       PIC  X(1).
      *日別、月別チェック区分
           03  CHKTRD-DAYMONTHKBN        PIC  X(1).
      *端末ＩＤ
           03  CHKTRD-TERMID             PIC  X(16).
      *オペーレータＩＤ
           03  CHKTRD-OPID               PIC  X(16).
      *作成年月日
           03  CHKTRD-CREYMD             PIC  X(08).
      *更新年月日
           03  CHKTRD-UPYMD              PIC  X(08).
      *更新時間 
           03  CHKTRD-UPHMS              PIC  X(06).
      *主キー更新用項目
           03  CHKTRD-UPKEY.
               05  CHKTRD-UPCHKTRDKBN      PIC  X(01).
               05  CHKTRD-UPSRYCD        PIC  X(09).
               05  CHKTRD-UPYUKOSTYMD    PIC  X(08).
               05  CHKTRD-UPYUKOEDYMD    PIC  X(08).
               05  CHKTRD-UPCDKBN        PIC  X(01).
               05  CHKTRD-UPRENNUM       PIC  9(03).
      *
      *検索用項目
           03  CHKTRD-SELNAME            PIC  X(200).
           03  CHKTRD-SELCDSYU           PIC X(1).
           03  CHKTRD-SELINPUTCD         PIC X(20).
