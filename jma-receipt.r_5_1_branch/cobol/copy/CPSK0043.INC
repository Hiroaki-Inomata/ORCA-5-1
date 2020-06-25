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
      *         システム管理マスタ     (CPSK0043.INC)   *
      *         （００４３）                            *
      *         日次統計パラメータ情報（標準提供）      *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  SYS-0043-REC.
           03  SYS-0043-KEY.
               05  SYS-0043-HOSPNUM     PIC 9(2).
      *管理コード
               05  SYS-0043-KANRICD     PIC  X(04).
      *区分コード
               05  SYS-0043-KBNCD       PIC  X(08).
      *有効開始年月日
               05  SYS-0043-STYUKYMD    PIC  X(08).
      *有効終了年月日
               05  SYS-0043-EDYUKYMD    PIC  X(08).
      *管理テーブル
           03  SYS-0043-TBL.
      *帳票名
               05  SYS-0043-HCNAME      PIC  X(160).
      *プログラム名
               05  SYS-0043-PRGNAME     PIC  X(24).
      *パラメタ
               05  SYS-0043-PARA-TBL.
                   07  SYS-0043-PARA-G      OCCURS  10.
                       09  SYS-0043-PARAHYOJI
                                        PIC X(20).
                       09  SYS-0043-PARA    PIC X(10).
      *必須区分（SPACE or 1:必須2:任意）
               05  SYS-0043-HISSU-KBN   OCCURS  10
                                        PIC X(01).
      *予備
               05  SYS-0043-YOBI        PIC  X(06).
      *端末ＩＤ
           03  SYS-0043-TERMID          PIC  X(16).
      *オペーレータＩＤ
           03  SYS-0043-OPID            PIC  X(16).
      *作成年月日
           03  SYS-0043-CREYMD          PIC  X(08).
      *更新年月日
           03  SYS-0043-UPYMD           PIC  X(08).
      *更新時間
           03  SYS-0043-UPHMS           PIC  X(06).
      *主キー更新用項目
           03  SYS-0043-UPD-KEY.
               05  SYS-0043-UPD-HOSPNUM     PIC 9(2).
               05  SYS-0043-UPD-KANRICD     PIC X(04).
               05  SYS-0043-UPD-KBNCD       PIC X(08).
               05  SYS-0043-UPD-STYUKYMD    PIC X(08).
               05  SYS-0043-UPD-EDYUKYMD    PIC X(08).
