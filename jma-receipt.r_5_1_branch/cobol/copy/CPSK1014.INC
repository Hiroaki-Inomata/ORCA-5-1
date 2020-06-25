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
      *         システム管理マスタ     (CPSK1014.INC)   *
      *         （１０１４）                            *
      *         包括診療行為管理                        *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  SYS-1014-REC.
           03  SYS-1014-KEY.
               05  SYS-1014-HOSPNUM     PIC 9(2).
      *管理コード
               05  SYS-1014-KANRICD     PIC  X(04).
      *区分コード
               05  SYS-1014-KBNCD       PIC  X(08).
      *有効開始年月日
               05  SYS-1014-STYUKYMD    PIC  X(08).
      *有効終了年月日
               05  SYS-1014-EDYUKYMD    PIC  X(08).
      *管理テーブル
           03  SYS-1014-TBL.
      *包括算定方法
               05  SYS-1014-SANTEI-HOU       PIC  X(01).
      *包括算定区分
               05  SYS-1014-SANTEI-KBN       PIC  X(01).
      *包括算定ルール（入外区分）
               05  SYS-1014-NYUGAIKBN        PIC  X(01).
      *包括算定ルール（保険組合せ区分）（労災・自賠責毎かそれ以外）
               05  SYS-1014-HKNCOMBI-KBN     PIC  X(01).
      *包括算定ルール（診療区分別）
               05  SYS-1014-SRYKBN-G.
                   07  SYS-1014-SRYKBN-KBN   PIC X(01)
                                             OCCURS   50.
      *予備
           03  SYS-1014-YOBI            PIC  X(446).
      *端末ＩＤ
           03  SYS-1014-TERMID          PIC  X(16).
      *オペーレータＩＤ
           03  SYS-1014-OPID            PIC  X(16).
      *作成年月日
           03  SYS-1014-CREYMD          PIC  X(08).
      *更新年月日
           03  SYS-1014-UPYMD           PIC  X(08).
      *更新時間
           03  SYS-1014-UPHMS           PIC  X(06).
      *主キー更新用項目
           03  SYS-1014-UPD-KEY.
               05  SYS-1014-UPD-HOSPNUM     PIC 9(2).
               05  SYS-1014-UPD-KANRICD     PIC X(04).
               05  SYS-1014-UPD-KBNCD       PIC X(08).
               05  SYS-1014-UPD-STYUKYMD    PIC X(08).
               05  SYS-1014-UPD-EDYUKYMD    PIC X(08).
