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
      *         システム管理マスタ     (CPSK1902.INC)            *
      *         （１９０２）                                     *
      *         医療機関編集情報−所在地情報                     *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  SYS-1902-REC.
           03  SYS-1902-KEY.
               05  SYS-1902-HOSPNUM     PIC 9(2).
      *管理コード
               05  SYS-1902-KANRICD     PIC  X(04).
      *区分コード
               05  SYS-1902-KBNCD       PIC  X(08).
      *有効開始年月日
               05  SYS-1902-STYUKYMD    PIC  X(08).
      *有効終了年月日
               05  SYS-1902-EDYUKYMD    PIC  X(08).
      *管理テーブル
           03  SYS-1902-TBL.
      *郵便番号
               05  SYS-1902-POST        PIC  X(07).
      *所在地（１行目）
               05  SYS-1902-ADRS1       PIC  X(152).
      *所在地（２行目）
               05  SYS-1902-ADRS2       PIC  X(152).
      *所在地（３行目）
               05  SYS-1902-ADRS3       PIC  X(152).
      *電話番号
               05  SYS-1902-TEL         PIC  X(15).
      *ＦＡＸ番号
               05  SYS-1902-FAX         PIC  X(15).
      *所在地（１行目）文字数
               05  SYS-1902-ADRS1MOJI   PIC  9(02).
      *予備
           03  SYS-1902-YOBI            PIC  X(05).
      *端末ＩＤ
           03  SYS-1902-TERMID          PIC  X(16).
      *オペーレータＩＤ
           03  SYS-1902-OPID            PIC  X(16).
      *作成年月日
           03  SYS-1902-CREYMD          PIC  X(08).
      *更新年月日
           03  SYS-1902-UPYMD           PIC  X(08).
      *更新時間
           03  SYS-1902-UPHMS           PIC  X(06).
      *主キー更新用項目
           03  SYS-1902-UPD-KEY.
               05  SYS-1902-UPD-HOSPNUM     PIC 9(2).
               05  SYS-1902-UPD-KANRICD     PIC X(04).
               05  SYS-1902-UPD-KBNCD       PIC X(08).
               05  SYS-1902-UPD-STYUKYMD    PIC X(08).
               05  SYS-1902-UPD-EDYUKYMD    PIC X(08).
