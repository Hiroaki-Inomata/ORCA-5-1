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
      *         システム管理マスタ     (CPSK0031.INC)   *
      *         （１００６）                            *
      *         施設基準情報コード                      *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  SYS-0031-REC.
           03  SYS-0031-KEY.
               05  SYS-0031-HOSPNUM     PIC 9(2).
      *管理コード
               05  SYS-0031-KANRICD     PIC  X(04).
      *区分コード
               05  SYS-0031-KBNCD       PIC  X(08).
      *有効開始年月日
               05  SYS-0031-STYUKYMD    PIC  X(08).
      *有効終了年月日
               05  SYS-0031-EDYUKYMD    PIC  X(08).
      *施設基準名称
           03  SYS-0031-TBL.
               05  SYS-0031-SSTKIJUN    PIC  X(100).
      *予備
           03  SYS-0031-YOBI            PIC  X(400).
      *端末ＩＤ
           03  SYS-0031-TERMID          PIC  X(16).
      *オペーレータＩＤ
           03  SYS-0031-OPID            PIC  X(16).
      *作成年月日
           03  SYS-0031-CREYMD          PIC  X(08).
      *更新年月日
           03  SYS-0031-UPYMD           PIC  X(08).
      *更新時間
           03  SYS-0031-UPHMS           PIC  X(06).
      *主キー更新用項目
           03  SYS-0031-UPD-KEY.
               05  SYS-0031-UPD-HOSPNUM     PIC 9(2).
               05  SYS-0031-UPD-KANRICD     PIC X(04).
               05  SYS-0031-UPD-KBNCD       PIC X(08).
               05  SYS-0031-UPD-STYUKYMD    PIC X(08).
               05  SYS-0031-UPD-EDYUKYMD    PIC X(08).
