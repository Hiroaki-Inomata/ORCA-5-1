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
      *         システム管理マスタ     (CPSK1030.INC)            *
      *         （１０３０）                                     *
      *         帳票編集区分                                     *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     MCC-多々納   02/03/15  後発医薬品追加
      * 01.00.02     NACL-多々納  04/12/10  カルテ３号紙内容追加
      ************************************************************
       01  SYS-1030-REC.
           03  SYS-1030-KEY.
               05  SYS-1030-HOSPNUM     PIC 9(2).
      *管理コード
               05  SYS-1030-KANRICD     PIC  X(04).
      *区分コード
               05  SYS-1030-KBNCD       PIC  X(08).
      *有効開始年月日
               05  SYS-1030-STYUKYMD    PIC  X(08).
      *有効終了年月日
               05  SYS-1030-EDYUKYMD    PIC  X(08).
      *管理テーブル
           03  SYS-1030-TBL.
      *処方せんＧＥ表示区分
               05  SYS-1030-SYOHOGEPRTFLG
                                        PIC  X(01).
      *処方せん後発医薬品表示区分
               05  SYS-1030-SYOHODSPNAME
                                        PIC  X(20).
      *カルテ３号紙内容
               05  SYS-1030-KARUTE3KBN
                                        PIC  X(01).
      *薬剤情報−画像ファイルフォルダ
               05  SYS-1030-GAZOFOLDER
                                        PIC  X(128).
      *後発医薬品への変更可署名
               05  SYS-1030-KOUHATUKA
                                        PIC  X(01).
      *予備
           03  SYS-1030-YOBI            PIC  X(349).
      *端末ＩＤ
           03  SYS-1030-TERMID          PIC  X(16).
      *オペーレータＩＤ
           03  SYS-1030-OPID            PIC  X(16).
      *作成年月日
           03  SYS-1030-CREYMD          PIC  X(08).
      *更新年月日
           03  SYS-1030-UPYMD           PIC  X(08).
      *更新時間
           03  SYS-1030-UPHMS           PIC  X(06).
      *主キー更新用項目
           03  SYS-1030-UPD-KEY.
               05  SYS-1030-UPD-HOSPNUM     PIC 9(2).
               05  SYS-1030-UPD-KANRICD     PIC X(04).
               05  SYS-1030-UPD-KBNCD       PIC X(08).
               05  SYS-1030-UPD-STYUKYMD    PIC X(08).
               05  SYS-1030-UPD-EDYUKYMD    PIC X(08).
