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
      *         システム管理マスタ     (CPSK4002.INC)   *
      *         （４００２）                            *
      *         労災保険　損傷区分名称                  *
      *-------------------------------------------------*
       01  SYS-4002-REC.
           03  SYS-4002-KEY.
               05  SYS-4002-HOSPNUM     PIC 9(2).
      *管理コード
               05  SYS-4002-KANRICD     PIC  X(04).
      *区分コード
               05  SYS-4002-KBNCD       PIC  X(08).
      *有効開始年月日
               05  SYS-4002-STYUKYMD    PIC  X(08).
      *有効終了年月日
               05  SYS-4002-EDYUKYMD    PIC  X(08).
      *管理テーブル
           03  SYS-4002-TBL.
      *損傷区分名称
               05  SYS-4002-SONSHOUNAME PIC  X(120).
      *予備
           03  SYS-4002-YOBI            PIC  X(380).
      *端末ＩＤ
           03  SYS-4002-TERMID          PIC  X(16).
      *オペーレータＩＤ
           03  SYS-4002-OPID            PIC  X(16).
      *作成年月日
           03  SYS-4002-CREYMD          PIC  X(08).
      *更新年月日
           03  SYS-4002-UPYMD           PIC  X(08).
      *更新時間
           03  SYS-4002-UPHMS           PIC  X(06).
      *主キー更新用項目
           03  SYS-4002-UPD-KEY.
               05  SYS-4002-UPD-HOSPNUM     PIC 9(2).
               05  SYS-4002-UPD-KANRICD     PIC X(04).
               05  SYS-4002-UPD-KBNCD       PIC X(08).
               05  SYS-4002-UPD-STYUKYMD    PIC X(08).
               05  SYS-4002-UPD-EDYUKYMD    PIC X(08).
