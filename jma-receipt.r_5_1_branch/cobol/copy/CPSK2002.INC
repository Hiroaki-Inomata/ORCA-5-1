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
      *         システム管理マスタ     (CPSK2002.INC)   *
      *         （２００２）                            *
      *         レセプト作成指示（個別作成）コード      *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01   MCC-藤原      02.06.13  レセプト種類区分の追加
      ************************************************************
       01  SYS-2002-REC.
           03  SYS-2002-KEY.
               05  SYS-2002-HOSPNUM     PIC 9(2).
      *管理コード
               05  SYS-2002-KANRICD     PIC  X(04).
      *区分コード
               05  SYS-2002-KBNCD       PIC  X(08).
      *有効開始年月日
               05  SYS-2002-STYUKYMD    PIC  X(08).
      *有効終了年月日
               05  SYS-2002-EDYUKYMD    PIC  X(08).
      *管理テーブル
           03  SYS-2002-TBL.
      *県番号
               05  SYS-2002-PTNUM       PIC  X(20).
      *点数表区分
               05  SYS-2002-PTID        PIC  X(10).
      *医療機関コード
               05  SYS-2002-SRYYM       PIC  X(06).
      *医療機関種別
               05  SYS-2002-NYUGAIKBN   PIC  X(01).
      *レセプト種類区分
               05  SYS-2002-RECEKBN     PIC  X(01).
      *予備
           03  SYS-2002-YOBI            PIC  X(462).
      *端末ＩＤ
           03  SYS-2002-TERMID          PIC  X(16).
      *オペーレータＩＤ
           03  SYS-2002-OPID            PIC  X(16).
      *作成年月日
           03  SYS-2002-CREYMD          PIC  X(08).
      *更新年月日
           03  SYS-2002-UPYMD           PIC  X(08).
      *更新時間
           03  SYS-2002-UPHMS           PIC  X(06).
      *主キー更新用項目
           03  SYS-2002-UPD-KEY.
               05  SYS-2002-UPD-HOSPNUM     PIC 9(2).
               05  SYS-2002-UPD-KANRICD     PIC X(04).
               05  SYS-2002-UPD-KBNCD       PIC X(08).
               05  SYS-2002-UPD-STYUKYMD    PIC X(08).
               05  SYS-2002-UPD-EDYUKYMD    PIC X(08).
