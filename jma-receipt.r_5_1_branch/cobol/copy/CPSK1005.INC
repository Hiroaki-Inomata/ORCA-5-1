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
      *         システム管理マスタ     (CPSK1005.INC)   *
      *         （１００５）                            *
      *         診療科目情報                            *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 04.07.00     NACL-多々納  13/03/XX  時間外加算等１設定追加
      ************************************************************
       01  SYS-1005-REC.
           03  SYS-1005-KEY.
               05  SYS-1005-HOSPNUM     PIC 9(2).
      *管理コード
               05  SYS-1005-KANRICD     PIC  X(04).
      *区分コード
               05  SYS-1005-KBNCD       PIC  X(08).
      *有効開始年月日
               05  SYS-1005-STYUKYMD    PIC  X(08).
      *有効終了年月日
               05  SYS-1005-EDYUKYMD    PIC  X(08).
      *管理テーブル
           03  SYS-1005-TBL.
      *診療科名
               05  SYS-1005-SRYKANAME   PIC  X(80).
      *短縮名１
               05  SYS-1005-SRYKANAME1  PIC  X(20).
      *短縮名２
               05  SYS-1005-SRYKANAME2  PIC  X(12).
      *短縮名３
               05  SYS-1005-SRYKANAME3  PIC  X(04).
      *レセ電算診療科（コード）
               05  SYS-1005-RECESRYKA   PIC  X(02).
      *
      *処置時間外加算等１設定
               05  SYS-1005-JIKANKSN1-40     PIC  X(01).
      *手術時間外加算等１設定
               05  SYS-1005-JIKANKSN1-50     PIC  X(01).
      *予備
      **   03  SYS-1005-YOBI            PIC  X(382).
      *予備
           03  SYS-1005-YOBI            PIC  X(380).
      *端末ＩＤ
           03  SYS-1005-TERMID          PIC  X(16).
      *オペーレータＩＤ
           03  SYS-1005-OPID            PIC  X(16).
      *作成年月日
           03  SYS-1005-CREYMD          PIC  X(08).
      *更新年月日
           03  SYS-1005-UPYMD           PIC  X(08).
      *更新時間
           03  SYS-1005-UPHMS           PIC  X(06).
      *主キー更新用項目
           03  SYS-1005-UPD-KEY.
               05  SYS-1005-UPD-HOSPNUM     PIC 9(2).
               05  SYS-1005-UPD-KANRICD     PIC X(04).
               05  SYS-1005-UPD-KBNCD       PIC X(08).
               05  SYS-1005-UPD-STYUKYMD    PIC X(08).
               05  SYS-1005-UPD-EDYUKYMD    PIC X(08).
