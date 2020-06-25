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
      *         システム管理マスタ     (CPSK9000.INC)            *
      *         （９０００）                                     *
      *         ＣＬＡＩＭ接続情報                               *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 03.05.00     NACL-多々納  07/07/17  外来診察料展開追加
      ************************************************************
       01  SYS-9000-REC.
           03  SYS-9000-KEY.
               05  SYS-9000-HOSPNUM     PIC 9(2).
      *管理コード
               05  SYS-9000-KANRICD     PIC  X(04).
      *区分コード
               05  SYS-9000-KBNCD       PIC  X(08).
      *有効開始年月日
               05  SYS-9000-STYUKYMD    PIC  X(08).
      *有効終了年月日
               05  SYS-9000-EDYUKYMD    PIC  X(08).
      *管理テーブル
           03  SYS-9000-TBL.
      *接続有無区分
               05  SYS-9000-SETUZOKU    PIC  X(01).
      *送信用インスタンスコード
               05  SYS-9000-CODE        PIC  X(01).
      *受付ポップアップ表示
               05  SYS-9000-UKEBUTTON-G.
                 07 SYS-9000-UKEBUTTON  PIC  X(01) OCCURS 2.
      *患者登録ポップアップ表示
               05  SYS-9000-KANBUTTON-G.
                 07 SYS-9000-KANBUTTON  PIC  X(01) OCCURS 2.
      *診療行為ポップアップ表示
               05  SYS-9000-SYNBUTTON-G.
                 07 SYS-9000-SYNBUTTON  PIC  X(01) OCCURS 2.
      *患者登録送信フラグ
           03  SYS-9000-PT-FLG          PIC  X(01).
      *レセ電変換フラグ
           03  SYS-9000-RECEDEN-FLG     PIC  X(01).
      *病名集約フラグ
           03  SYS-9000-BYOMEI-SYU-FLG  PIC  X(01).
      *病名同期フラグ
           03  SYS-9000-BYOMEI-DOUKI-FLG
                                        PIC  X(01).
      *入院オーダー取込フラグ
           03  SYS-9000-NYU-ORDER-FLG
                                        PIC  X(01).
      *マルチホストフラグ
           03  SYS-9000-MULTIHOST-FLG
                                        PIC  X(01).
      *再送信フラグ
           03  SYS-9000-RESEND-FLG
                                        PIC  X(01).
      *外来診察料展開
           03  SYS-9000-SINSATU-FLG     PIC  X(01).
      *予備
           03  SYS-9000-YOBI            PIC  X(485).
      *端末ＩＤ
           03  SYS-9000-TERMID          PIC  X(16).
      *オペーレータＩＤ
           03  SYS-9000-OPID            PIC  X(16).
      *作成年月日
           03  SYS-9000-CREYMD          PIC  X(08).
      *更新年月日
           03  SYS-9000-UPYMD           PIC  X(08).
      *更新時間
           03  SYS-9000-UPHMS           PIC  X(06).
      *主キー更新用項目
           03  SYS-9000-UPD-KEY.
               05  SYS-9000-UPD-HOSPNUM     PIC 9(2).
               05  SYS-9000-UPD-KANRICD     PIC X(04).
               05  SYS-9000-UPD-KBNCD       PIC X(08).
               05  SYS-9000-UPD-STYUKYMD    PIC X(08).
               05  SYS-9000-UPD-EDYUKYMD    PIC X(08).
