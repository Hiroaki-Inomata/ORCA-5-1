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
      *    チェックマスタ        COPY (CPCHK.INC)                     *
      *****************************************************************
      *****************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *01  CHK-REC.
           03  CHK-KEY.
      *医療機関番号
               05  CHK-HOSPNUM         PIC 9(2).
      *チェック区分
               05  CHK-CHKKBN          PIC X(1).
      *診療コード
               05  CHK-SRYCD           PIC X(9).
      *グループ番号（未使用）
               05  CHK-GRPNUM          PIC 9(2).
      *有効開始年月日
               05  CHK-YUKOSTYMD       PIC X(8).
      *有効終了年月日
               05  CHK-YUKOEDYMD       PIC X(8).
      *コード区分
               05  CHK-CDKBN           PIC X(1).
      *適用コード
               05  CHK-CD              PIC X(9).
      *入外区分（未使用）
           03  CHK-NYUGAIKBN           PIC X(1).
      *同一入外制御区分（1:入院・外来の判定を行う）
           03  CHK-NYUGAIBNDKBN        PIC X(1).
      *処理区分（1:慢性疼痛管理料チェック、2:初診料チェック）
           03  CHK-SYORIKBN            PIC X(1).
      *エラー処理（0:エラー、9:エラー、8:処理なし）
           03  CHK-CHKERR              PIC 9(1).
      *データ区分（1:マスタ提供、2:ユーザ登録）
           03  CHK-DATAKBN             PIC X(1).
      *連番号（画面表示）
           03  CHK-RENNUM              PIC 9(3).
      *端末ＩＤ
           03  CHK-TERMID              PIC  X(16).
      *オペーレータＩＤ
           03  CHK-OPID                PIC  X(16).
      *作成年月日
           03  CHK-CREYMD              PIC  X(08).
      *更新年月日
           03  CHK-UPYMD               PIC  X(08).
      *更新時間 
           03  CHK-UPHMS               PIC  X(06).
      *主キー更新用項目
           03  CHK-UPDKEY.
               05  CHK-UPHOSPNUM       PIC 9(2).
               05  CHK-UPCHKKBN        PIC X(1).
               05  CHK-UPSRYCD         PIC X(9).
               05  CHK-UPGRPNUM        PIC 9(2).
               05  CHK-UPYUKOSTYMD     PIC X(8).
               05  CHK-UPYUKOEDYMD     PIC X(8).
               05  CHK-UPCDKBN         PIC X(1).
               05  CHK-UPCD            PIC X(9).
           03  CHK-SELNAME             PIC X(200).
           03  CHK-SELCDSYU            PIC X(1).
           03  CHK-SELINPUTCD          PIC X(20).
