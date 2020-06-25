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
      *    チェックマスタ更新データ    COPY (CPCHKWK1.INC)            *
      *****************************************************************
      *****************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *01  CHKWK-REC.
      *処理区分
           03  CHKWK-SYORI-UPDKBN        PIC  X(01).
      *
           03  CHKWK-KEY.
      *医療機関番号
               05  CHKWK-HOSPNUM         PIC 9(2).
      *チェック区分
               05  CHKWK-CHKKBN          PIC X(1).
      *診療コード
               05  CHKWK-SRYCD           PIC X(9).
      *グループ番号（未使用）
               05  CHKWK-GRPNUM          PIC 9(2).
      *有効開始年月日
               05  CHKWK-YUKOSTYMD       PIC X(8).
      *有効終了年月日
               05  CHKWK-YUKOEDYMD       PIC X(8).
      *コード区分
               05  CHKWK-CDKBN           PIC X(1).
      *適用コード
               05  CHKWK-CD              PIC X(9).
      *有効終了年月日2
           03  CHKWK-YUKOEDYMD2          PIC  X(08).
      *入外区分（未使用）
           03  CHKWK-NYUGAIKBN           PIC X(1).
      *同一入外制御区分（1:入院・外来の判定を行う）
           03  CHKWK-NYUGAIBNDKBN        PIC X(1).
      *処理区分（1:慢性疼痛管理料チェック、2:初診料チェック）
           03  CHKWK-SYORIKBN            PIC X(1).
      *エラー処理（0:エラー、9:エラー、8:処理なし）
           03  CHKWK-CHKERR              PIC 9(1).
      *データ区分（1:マスタ提供、2:ユーザ登録）
           03  CHKWK-DATAKBN             PIC X(1).
      *連番号（画面表示）
           03  CHKWK-RENNUM              PIC 9(3).
      *端末ＩＤ
           03  CHKWK-TERMID              PIC  X(16).
      *オペーレータＩＤ
           03  CHKWK-OPID                PIC  X(16).
      *作成年月日
           03  CHKWK-CREYMD              PIC  X(08).
      *更新年月日
           03  CHKWK-UPYMD               PIC  X(08).
      *更新時間 
           03  CHKWK-UPHMS               PIC  X(06).
