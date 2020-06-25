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
      *         システム管理マスタ     (CPSK1050.INC)   *
      *         （１０５０）                            *
      *         職員情報２                              *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 04.07.00     NACL-多々納  14/02/10  入力スクロール行数追加
      ************************************************************
       01  SYS-1050-REC.
           03  SYS-1050-KEY.
               05  SYS-1050-HOSPNUM     PIC 9(2).
      *管理コード
               05  SYS-1050-KANRICD     PIC  X(04).
      *区分コード
               05  SYS-1050-KBNCD       PIC  X(08).
      *有効開始年月日
               05  SYS-1050-STYUKYMD    PIC  X(08).
      *有効終了年月日
               05  SYS-1050-EDYUKYMD    PIC  X(08).
      *管理テーブル
           03  SYS-1050-TBL.
      *オペレータＩＤ
               05  SYS-1050-ICCARDRW    PIC  X(1).
      *
      *入力スクロール行数　外来
               05  SYS-1050-GLINECNT    PIC  X(02).
      *入力スクロール行数　入院
               05  SYS-1050-NLINECNT    PIC  X(02).
      *プリンタへのデータ送信単位
               05  SYS-1050-PRTCONF     PIC  X(01).
      *予備
      **** 03  SYS-1050-YOBI            PIC  X(499).
           03  SYS-1050-YOBI            PIC  X(494).
      *端末ＩＤ
           03  SYS-1050-TERMID          PIC  X(16).
      *オペーレータＩＤ
           03  SYS-1050-OPID            PIC  X(16).
      *作成年月日
           03  SYS-1050-CREYMD          PIC  X(08).
      *更新年月日
           03  SYS-1050-UPYMD           PIC  X(08).
      *更新時間
           03  SYS-1050-UPHMS           PIC  X(06).
      *主キー更新用項目
           03  SYS-1050-UPD-KEY.
               05  SYS-1050-UPD-HOSPNUM     PIC 9(2).
               05  SYS-1050-UPD-KANRICD     PIC X(04).
               05  SYS-1050-UPD-KBNCD       PIC X(08).
               05  SYS-1050-UPD-STYUKYMD    PIC X(08).
               05  SYS-1050-UPD-EDYUKYMD    PIC X(08).
