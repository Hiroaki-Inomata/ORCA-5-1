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
      *    剤毎処方一般名編集パラメタ　　COPY (CPORCSCGENNAME.INC)
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  ORCSCGENNAMEAREA.
      *
      *入力
      *処理区分（１：院内判定なし）
           03  ORCSCGENN-SYORIKBN        PIC  X(01).
      *処理区分２（１：マスタ検索なし）
           03  ORCSCGENN-SYORIKBN2       PIC  X(01).
      *診療日付
           03  ORCSCGENN-SRYYMD          PIC  X(08).
      *後発医薬品への変更可署名(SYS-1030-KOUHATUKA)
           03  ORCSCGENN-1030-KOUHATUKA  PIC  X(01).
      *後発品変更不可（処方毎）有
           03  ORCSCGENN-KOUHATU-IN      PIC  X(01).
      *診療種別
           03  ORCSCGENN-SRYKBN          PIC  X(02).
           03  ORCSCGENN-SRYSYUKBN       PIC  X(03).
      *剤点数
           03  ORCSCGENN-ZAITEN          PIC  9(07).
      *
      *剤内容
           03  ORCSCGENN-DATA-AREA.
      *ＭＡＸ
               05  ORCSCGENN-TBLMAX         PIC  9(03).
               05  ORCSCGENN-DATA-TBL       OCCURS   51.
      *IN
      *            診療コード
                   07  ORCSCGENN-SRYCD          PIC X(09).
      *            診療コード(画面表示）
                   07  ORCSCGENN-INPUTCD        PIC X(20).
      *
      *            点数マスタ（後発品区分）
                   07  ORCSCGENN-TNS-KOUHATUKBN     PIC 9(01).
      *            点数マスタ付加（一般名記載区分）
                   07  ORCSCGENN-IPN-KISAIKBN       PIC 9(01).
      *
      *            一般名コード（薬価基準コード）
                   07  ORCSGETGE-YAKKAKJNCD     PIC X(12).
      *            一般名称
                   07  ORCSCGENN-GENERICNAME    PIC X(200).
      *OUT
      *            処方せん名称　（処理区分２＝１　はなし）
                   07  ORCSCGENN-HEN-NAME       PIC X(100).
      *            名称区分（1:一般名,2：処方名称、3：処方名（一般）
                   07  ORCSCGENN-NAME-KBN       PIC X(01).
      *            処方せん単位名称
                   07  ORCSCGENN-HEN-TANINAME   PIC X(24).
      *
      *
           03  ORCSCGENN-OUT-AREA.
      *        院内・院外区分（１：院内、２：院外）
               05  ORCSCGENN-INNAIKBN        PIC X(01).
      *        エラーコード
               05  ORCSCGENN-ERRCD           PIC 9(01).
      *
