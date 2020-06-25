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
      *         システム管理マスタ     (CPSK5012.INC)   *
      *         （５０１２）                            *
      *         定期請求作成指示（個別作成）コード      *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  SYS-5012-REC.
           03  SYS-5012-KEY.
               05  SYS-5012-HOSPNUM     PIC 9(2).
      *管理コード
               05  SYS-5012-KANRICD     PIC  X(04).
      *区分コード
               05  SYS-5012-KBNCD       PIC  X(08).
      *有効開始年月日
               05  SYS-5012-STYUKYMD    PIC  X(08).
      *有効終了年月日
               05  SYS-5012-EDYUKYMD    PIC  X(08).
      *管理テーブル
           03  SYS-5012-TBL.
      *患者番号
               05  SYS-5012-PTNUM       PIC  X(20).
      *患者ＩＤ
               05  SYS-5012-PTID        PIC  X(10).
      *請求年月
               05  SYS-5012-SRYYM       PIC  X(06).
      *作成区分
               05  SYS-5012-SAKKBN      PIC  X(01).
      *請求開始日
               05  SYS-5012-SKYSTYMD    PIC  X(08).
      *請求終了日
               05  SYS-5012-SKYEDYMD    PIC  X(08).
      *入院日
               05  SYS-5012-NYUINYMD    PIC  X(08).
      *退院日
               05  SYS-5012-TAIINYMD    PIC  X(08).
      *期別
               05  SYS-5012-KBT         PIC  X(01).
      *予備
               05  SYS-5012-YOBI        PIC  X(462).
      *端末ＩＤ
           03  SYS-5012-TERMID          PIC  X(16).
      *オペーレータＩＤ
           03  SYS-5012-OPID            PIC  X(16).
      *作成年月日
           03  SYS-5012-CREYMD          PIC  X(08).
      *更新年月日
           03  SYS-5012-UPYMD           PIC  X(08).
      *更新時間
           03  SYS-5012-UPHMS           PIC  X(06).
      *主キー更新用項目
           03  SYS-5012-UPD-KEY.
               05  SYS-5012-UPD-HOSPNUM     PIC 9(2).
               05  SYS-5012-UPD-KANRICD     PIC X(04).
               05  SYS-5012-UPD-KBNCD       PIC X(08).
               05  SYS-5012-UPD-STYUKYMD    PIC X(08).
               05  SYS-5012-UPD-EDYUKYMD    PIC X(08).
