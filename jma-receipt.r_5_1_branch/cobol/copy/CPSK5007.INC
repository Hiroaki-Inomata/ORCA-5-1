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
      *         システム管理マスタ     (CPSK5007.INC)   *
      *         （５００７）                            *
      *         入院指示せん設定情報                    *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  SYS-5007-REC.
           03  SYS-5007-KEY.
               05  SYS-5007-HOSPNUM     PIC 9(2).
      *管理コード
               05  SYS-5007-KANRICD     PIC  X(04).
      *区分コード（０１：入院処方、０２：入院麻薬処方
      *            ０３：注射処方、０４：注射麻薬、
      *            ０５：指示箋）
               05  SYS-5007-KBNCD       PIC  X(08).
      *有効開始年月日
               05  SYS-5007-STYUKYMD    PIC  X(08).
      *有効終了年月日
               05  SYS-5007-EDYUKYMD    PIC  X(08).
      *管理テーブル
           03  SYS-5007-TBL.
      *入院指示せん区分
               05  SYS-5007-SIJISENKBN  PIC  X(01).
      *指示箋
               05  SYS-5007-SIJISEN-AREA   OCCURS   4.
      *        見出し
                   07  SYS-5007-SIJISEN-HEAD   PIC X(30).
      *        印刷部数
                   07  SYS-5007-SIJISEN-PRT    PIC 9(02).
      *予備
               05  SYS-5007-YOBI             PIC  X(371).
      *端末ＩＤ
           03  SYS-5007-TERMID          PIC  X(16).
      *オペーレータＩＤ
           03  SYS-5007-OPID            PIC  X(16).
      *作成年月日
           03  SYS-5007-CREYMD          PIC  X(08).
      *更新年月日
           03  SYS-5007-UPYMD           PIC  X(08).
      *更新時間
           03  SYS-5007-UPHMS           PIC  X(06).
      *主キー更新用項目
           03  SYS-5007-UPD-KEY.
               05  SYS-5007-UPD-HOSPNUM     PIC 9(2).
               05  SYS-5007-UPD-KANRICD     PIC X(04).
               05  SYS-5007-UPD-KBNCD       PIC X(08).
               05  SYS-5007-UPD-STYUKYMD    PIC X(08).
               05  SYS-5007-UPD-EDYUKYMD    PIC X(08).
