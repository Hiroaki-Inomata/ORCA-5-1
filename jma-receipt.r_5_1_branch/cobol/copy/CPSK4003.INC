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
      *-------------------------------------------------*
      *         システム管理マスタ     (CPSK4003.INC)   *
      *         （４００３）                            *
      *         自賠責保険　特定器材等消費税対象外コード*
      *-------------------------------------------------*
       01  SYS-4003-REC.
           03  SYS-4003-KEY.
               05  SYS-4003-HOSPNUM     PIC 9(2).
      *管理コード
               05  SYS-4003-KANRICD     PIC  X(04).
      *区分コード
               05  SYS-4003-KBNCD       PIC  X(08).
      *有効開始年月日
               05  SYS-4003-STYUKYMD    PIC  X(08).
      *有効終了年月日
               05  SYS-4003-EDYUKYMD    PIC  X(08).
      *管理テーブル
           03  SYS-4003-TBL.
      *対象外診療コード
               05  SYS-4003-SRYCD       PIC  X(09)
                                        OCCURS  50.
      *予備
           03  SYS-4003-YOBI            PIC  X(50).
      *端末ＩＤ
           03  SYS-4003-TERMID          PIC  X(16).
      *オペーレータＩＤ
           03  SYS-4003-OPID            PIC  X(16).
      *作成年月日
           03  SYS-4003-CREYMD          PIC  X(08).
      *更新年月日
           03  SYS-4003-UPYMD           PIC  X(08).
      *更新時間
           03  SYS-4003-UPHMS           PIC  X(06).
      *主キー更新用項目
           03  SYS-4003-UPD-KEY.
               05  SYS-4003-UPD-HOSPNUM     PIC 9(2).
               05  SYS-4003-UPD-KANRICD     PIC X(04).
               05  SYS-4003-UPD-KBNCD       PIC X(08).
               05  SYS-4003-UPD-STYUKYMD    PIC X(08).
               05  SYS-4003-UPD-EDYUKYMD    PIC X(08).
