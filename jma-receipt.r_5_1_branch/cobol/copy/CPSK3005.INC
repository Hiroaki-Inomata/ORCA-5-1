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
      *         システム管理マスタ     (CPSK3005.INC)   *
      *         （３００５）                            *
      *         締め時刻管理                            *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  SYS-3005-REC.
           03  SYS-3005-KEY.
               05  SYS-3005-HOSPNUM     PIC 9(2).
      *管理コード
               05  SYS-3005-KANRICD     PIC  X(04).
      *区分コード
               05  SYS-3005-KBNCD       PIC  X(08).
      *有効開始年月日
               05  SYS-3005-STYUKYMD    PIC  X(08).
      *有効終了年月日
               05  SYS-3005-EDYUKYMD    PIC  X(08).
      *管理テーブル
           03  SYS-3005-TBL.
      *処理区分
               05  SYS-3005-PROCKBN     PIC  X(01).
      *診療日自
               05  SYS-3005-STSRYYMD    PIC  X(08).
      *時刻自
               05  SYS-3005-STTIME      PIC  X(04).
      *診療日至
               05  SYS-3005-EDSRYYMD    PIC  X(08).
      *時刻至
               05  SYS-3005-EDTIME      PIC  X(04).
      *予備
               05  SYS-3005-YOBI        PIC  X(475).
      *端末ＩＤ
           03  SYS-3005-TERMID          PIC  X(16).
      *オペーレータＩＤ
           03  SYS-3005-OPID            PIC  X(16).
      *作成年月日
           03  SYS-3005-CREYMD          PIC  X(08).
      *更新年月日
           03  SYS-3005-UPYMD           PIC  X(08).
      *更新時間
           03  SYS-3005-UPHMS           PIC  X(06).
      *主キー更新用項目
           03  SYS-3005-UPD-KEY.
               05  SYS-3005-UPD-HOSPNUM     PIC 9(2).
               05  SYS-3005-UPD-KANRICD     PIC X(04).
               05  SYS-3005-UPD-KBNCD       PIC X(08).
               05  SYS-3005-UPD-STYUKYMD    PIC X(08).
               05  SYS-3005-UPD-EDYUKYMD    PIC X(08).
