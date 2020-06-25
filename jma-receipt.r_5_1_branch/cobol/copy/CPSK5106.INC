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
      *         システム管理マスタ     (CPSK5106.INC)   *
      *         （５１０６）                            *
      *         夜間勤務看護加算                        *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  SYS-5106-REC.
           03  SYS-5106-KEY.
               05  SYS-5106-HOSPNUM     PIC 9(2).
      *管理コード
               05  SYS-5106-KANRICD     PIC  X(04).
      *区分コード
               05  SYS-5106-KBNCD       PIC  X(08).
      *有効開始年月日
               05  SYS-5106-STYUKYMD    PIC  X(08).
      *有効終了年月日
               05  SYS-5106-EDYUKYMD    PIC  X(08).
      *管理テーブル
           03  SYS-5106-TBL.
      *夜間勤務等看護加算
               05  SYS-5106-BTU-YAKANKNGKSN-NM
                                        PIC  X(20).
      *夜間勤務等看護加算２
               05  SYS-5106-BTU-YAKANKNGKSN-NM2
                                        PIC  X(10).
      *予備
           03  SYS-5106-YOBI            PIC  X(470).
      *端末ＩＤ
           03  SYS-5106-TERMID          PIC  X(16).
      *オペーレータＩＤ
           03  SYS-5106-OPID            PIC  X(16).
      *作成年月日
           03  SYS-5106-CREYMD          PIC  X(08).
      *更新年月日
           03  SYS-5106-UPYMD           PIC  X(08).
      *更新時間
           03  SYS-5106-UPHMS           PIC  X(06).
      *主キー更新用項目
           03  SYS-5106-UPD-KEY.
               05  SYS-5106-UPD-HOSPNUM     PIC 9(2).
               05  SYS-5106-UPD-KANRICD     PIC X(04).
               05  SYS-5106-UPD-KBNCD       PIC X(08).
               05  SYS-5106-UPD-STYUKYMD    PIC X(08).
               05  SYS-5106-UPD-EDYUKYMD    PIC X(08).
