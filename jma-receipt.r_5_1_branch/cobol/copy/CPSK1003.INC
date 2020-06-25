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
      *         システム管理マスタ     (CPSK1003.INC)   *
      *         （１００３）                            *
      *         医療機関情報                            *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.04.01    NACL-森脇    06/12/13  ユーザURL追加
      ************************************************************
       01  SYS-1003-REC.
           03  SYS-1003-KEY.
               05  SYS-1003-HOSPNUM     PIC 9(2).
      *管理コード
               05  SYS-1003-KANRICD     PIC  X(04).
      *区分コード
               05  SYS-1003-KBNCD       PIC  X(08).
      *有効開始年月日
               05  SYS-1003-STYUKYMD    PIC  X(08).
      *有効終了年月日
               05  SYS-1003-EDYUKYMD    PIC  X(08).
      *管理テーブル
           03  SYS-1003-TBL.
      *ｅメールアドレス
               05  SYS-1003-EMAIL       PIC  X(100).
      *ホームページアドレス
               05  SYS-1003-HPURL       PIC  X(100).
      *ユーザＵＲＬ
               05  SYS-1003-USERURL     PIC  X(200).
      *予備
           03  SYS-1003-YOBI            PIC  X(100).
      *端末ＩＤ
           03  SYS-1003-TERMID          PIC  X(16).
      *オペーレータＩＤ
           03  SYS-1003-OPID            PIC  X(16).
      *作成年月日
           03  SYS-1003-CREYMD          PIC  X(08).
      *更新年月日
           03  SYS-1003-UPYMD           PIC  X(08).
      *更新時間
           03  SYS-1003-UPHMS           PIC  X(06).
      *主キー更新用項目
           03  SYS-1003-UPD-KEY.
               05  SYS-1003-UPD-HOSPNUM     PIC 9(2).
               05  SYS-1003-UPD-KANRICD     PIC X(04).
               05  SYS-1003-UPD-KBNCD       PIC X(08).
               05  SYS-1003-UPD-STYUKYMD    PIC X(08).
               05  SYS-1003-UPD-EDYUKYMD    PIC X(08).
