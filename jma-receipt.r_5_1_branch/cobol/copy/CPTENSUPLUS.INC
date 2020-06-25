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
      *         点数マスタ    COPY (CPTENSUPLSU.INC)     *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 03.04.00     NACL-多々納  06/12/05  上限回数追加
      * 05.01.00     NACL-多々納  20/02/05  ユーザー単位追加
      ************************************************************
      *01  TENSUPLSU-REC.
           03  TNSP-KEY.
               05  TNSP-HOSPNUM          PIC 9(2).
               05  TNSP-SRYCD           PIC  X(09).
               05  TNSP-YUKOSTYMD       PIC  X(08).
               05  TNSP-YUKOEDYMD       PIC  X(08).
      *    処方名称
           03  TNSP-SHOYUKOKETA         PIC  9(02).
           03  TNSP-SHONAME             PIC  X(200).
      *    採血料区分
           03  TNSP-SAIKETUKBN          PIC  9(01).
      *    入力チェック区分
           03  TNSP-INPCHKKBN           PIC  9(01).
      *    添付資料用薬剤分類区分
           03  TNSP-TENPUYAKKBN         PIC  X(02).
      *    全数量記載区分
           03  TNSP-ALLSURYOKBN         PIC  X(01).
      *    数量換算単位コード
           03  TNSP-KANZANTANICD        PIC  X(03).
      *    数量換算単位名称
           03  TNSP-KANZANTANINAME      PIC  X(24).
      *    数量換算値
           03  TNSP-KANZANCHI           PIC  9(05)V9(05).
      *    ユーザー設定上限回数
      *    算定履歴区分（１のみ）
           03  TNSP-SANTEIRRKKBN        PIC  9(01).
      *    月上限回数
           03  TNSP-JGNCNT              PIC  9(03).
      *    日上限回数
           03  TNSP-JGNCNT1D            PIC  9(03).
      *    週上限回数
           03  TNSP-JGNCNT1W            PIC  9(03).
      *    エラー処理
           03  TNSP-JGNCNTERR           PIC  9(01).
      *    他月数
           03  TNSP-JGNCNTETCM          PIC  9(02).
      *    他月上限回数
           03  TNSP-JGNCNTETC           PIC  9(03).
      *
      *    総量編集区分
           03  TNSP-SOURYOHENKBN           PIC  9(01).
      *    一般名記載区分
           03  TNSP-IPN-KISAIKBN           PIC  9(01).
      *    一般名記載単位
           03  TNSP-IPN-KANZANTANICD       PIC  X(03).
      *    一般名記載単位名
           03  TNSP-IPN-KANZANTANINAME     PIC  X(24).
      *    一般名記載換算値
           03  TNSP-IPN-KANZANCHI          PIC  9(05)V9(05).
      *
      *    前回検査日コメント区分
           03  TNSP-ZENKNSKBN              PIC  9(01).
      *
      *    湿布薬関係
           03  TNSP-SIPPURYO1           PIC  9(03).
           03  TNSP-SIPPUIRISU1         PIC  9(03).
           03  TNSP-SIPPURYO2           PIC  9(03).
           03  TNSP-SIPPUIRISU2         PIC  9(03).
      *R02.2
      *    ユーザー単位コード
           03  TNSP-USERTANICD          PIC  X(03).
      *    ユーザー単位名称
           03  TNSP-USERTANINAME        PIC  X(24).
      *
           03  TNSP-TERMID              PIC  X(16).
           03  TNSP-OPID                PIC  X(16).
           03  TNSP-CREYMD              PIC  X(08).
           03  TNSP-UPYMD               PIC  X(08).
           03  TNSP-UPHMS               PIC  X(06).
      *　　主キー更新用項目
           03  TNSP-UP-KEY.
               05  TNSP-UP-HOSPNUM       PIC 9(2).
               05  TNSP-UP-SRYCD         PIC  X(09).
               05  TNSP-UP-YUKOSTYMD     PIC  X(08).
               05  TNSP-UP-YUKOEDYMD     PIC  X(08).
