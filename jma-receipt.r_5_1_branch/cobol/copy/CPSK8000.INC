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
      *         システム管理マスタ     (CPSK8000.INC)            *
      *         （８０００）                                     *
      *         印刷データ等削除設定情報                         *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  SYS-8000-REC.
           03  SYS-8000-KEY.
               05  SYS-8000-HOSPNUM    PIC 9(2).
      *管理コード
               05  SYS-8000-KANRICD    PIC  X(04).
      *区分コード
               05  SYS-8000-KBNCD      PIC  X(08).
      *有効開始年月日
               05  SYS-8000-STYUKYMD   PIC  X(08).
      *有効終了年月日
               05  SYS-8000-EDYUKYMD   PIC  X(08).
      *管理テーブル
           03  SYS-8000-TBL.
      *日次情報
               05  SYS-8000-DAILY      PIC  9(03).
      *
               05  SYS-8000-DAILY-YM   PIC  X(06).
      *
               05  SYS-8000-DAILY-UPYMD
                                       PIC  X(08).
      *月次情報
               05  SYS-8000-MONTHLY    PIC  9(03).
      *
               05  SYS-8000-MONTHLY-YM PIC  X(06).
      *
               05  SYS-8000-MONTHLY-UPYMD
                                       PIC  X(08).
      *入院オーダー
               05  SYS-8000-NORDER     PIC  9(03).
      *
               05  SYS-8000-NORDER-YM  PIC  X(06).
      *
               05  SYS-8000-NORDER-UPYMD
                                       PIC  X(08).
      *予備
           03  SYS-8000-YOBI           PIC  X(449).
      *端末ＩＤ
           03  SYS-8000-TERMID         PIC  X(16).
      *オペーレータＩＤ
           03  SYS-8000-OPID           PIC  X(16).
      *作成年月日
           03  SYS-8000-CREYMD         PIC  X(08).
      *更新年月日
           03  SYS-8000-UPYMD          PIC  X(08).
      *更新時間
           03  SYS-8000-UPHMS          PIC  X(06).
      *主キー更新用項目
           03  SYS-8000-UPD-KEY.
               05  SYS-8000-UPD-HOSPNUM     PIC 9(2).
               05  SYS-8000-UPD-KANRICD     PIC X(04).
               05  SYS-8000-UPD-KBNCD       PIC X(08).
               05  SYS-8000-UPD-STYUKYMD    PIC X(08).
               05  SYS-8000-UPD-EDYUKYMD    PIC X(08).
