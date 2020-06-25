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
      *         システム管理マスタ     (CPSK3004.INC)   *
      *         （３００４）                            *
      *         県単独事業情報コード                    *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-藤原    04.08.31  パラメタ種別、プリンタ名
      *                                     の追加
      *
      *  04.09.01    NACL-藤原    16/11/18  Ｄｏｃｋｅｒ対応
      ************************************************************
       01  SYS-3004-REC.
           03  SYS-3004-KEY.
               05  SYS-3004-HOSPNUM     PIC 9(2).
      *管理コード
               05  SYS-3004-KANRICD     PIC  X(04).
      *区分コード
               05  SYS-3004-KBNCD       PIC  X(08).
      *有効開始年月日
               05  SYS-3004-STYUKYMD    PIC  X(08).
      *有効終了年月日
               05  SYS-3004-EDYUKYMD    PIC  X(08).
      *シェルID
           03  SYS-3004-SHELLID         PIC  X(020).
      *処理名称
           03  SYS-3004-SHORIMSG        PIC  X(040).
      *設定フラグ
           03  SYS-3004-CHK.
               05  SYS-3004-CHKFLG      PIC  X(01)
                                        OCCURS   4.
      *パラメタ種別
           03  SYS-3004-PARAKBN         PIC  X(01).
      *プリンタ名
           03  SYS-3004-PRTNM           PIC  X(16).
      *レポジトリ名
           03  SYS-3004-NAME            PIC  X(128).
      *予備
           03  SYS-3004-YOBI            PIC  X(290).
      *端末ＩＤ
           03  SYS-3004-TERMID          PIC  X(16).
      *オペーレータＩＤ
           03  SYS-3004-OPID            PIC  X(16).
      *作成年月日
           03  SYS-3004-CREYMD          PIC  X(08).
      *更新年月日
           03  SYS-3004-UPYMD           PIC  X(08).
      *更新時間
           03  SYS-3004-UPHMS           PIC  X(06).
      *主キー更新用項目
           03  SYS-3004-UPD-KEY.
               05  SYS-3004-UPD-HOSPNUM     PIC 9(2).
               05  SYS-3004-UPD-KANRICD     PIC X(04).
               05  SYS-3004-UPD-KBNCD       PIC X(08).
               05  SYS-3004-UPD-STYUKYMD    PIC X(08).
               05  SYS-3004-UPD-EDYUKYMD    PIC X(08).
