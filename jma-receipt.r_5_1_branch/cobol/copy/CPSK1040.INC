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
      *         システム管理マスタ     (CPSK1040.INC)   *
      *         （１０４０）                            *
      *         データチェック機能情報                  *
      *         公費負担者番号・受給者番号チェク情報    *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  SYS-1040-REC.
           03  SYS-1040-KEY.
               05  SYS-1040-HOSPNUM     PIC 9(2).
      *管理コード
               05  SYS-1040-KANRICD     PIC  X(04).
      *区分コード
               05  SYS-1040-KBNCD       PIC  X(08).
      *有効開始年月日
               05  SYS-1040-STYUKYMD    PIC  X(08).
      *有効終了年月日
               05  SYS-1040-EDYUKYMD    PIC  X(08).
      *管理テーブル
           03  SYS-1040-TBL.
      *負担者番号抜けチェック
               05  SYS-1040-FTNJANUMCHK PIC  X(01).
      *受給者番号抜けチェック
               05  SYS-1040-JKYSNUMCHK  PIC  X(01).
      *負担者番号半角文字許可
               05  SYS-1040-FTNJHANKAKUCHK
                                        PIC  X(01).
      *負担者番号全角文字許可
               05  SYS-1040-FTNJZENKAKUCHK
                                        PIC  X(01).
      *受給者番号半角文字許可
               05  SYS-1040-JKYSHANKAKUCHK
                                        PIC  X(01).
      *受給者番号全角文字許可
               05  SYS-1040-JKYSZENKAKUCHK
                                        PIC  X(01).
      *負担者番号数字以外の文字許可
               05  SYS-1040-FTNJNUMERICCHK
                                        PIC  X(01).
      *受給者番号数字以外の文字許可
               05  SYS-1040-JKYSNUMERICCHK
                                        PIC  X(01).
      *負担者番号桁数チェック
               05  SYS-1040-FTNJKETACHK
                                        PIC  X(01).
      *負担者番号桁数最小
               05  SYS-1040-FTNJKETAMIN
                                        PIC  X(02).
      *負担者番号桁数最大
               05  SYS-1040-FTNJKETAMAX
                                        PIC  X(02).
      *受給者番号桁数チェック
               05  SYS-1040-JKYSKETACHK
                                        PIC  X(01).
      *受給者番号桁数最小
               05  SYS-1040-JKYSKETAMIN
                                        PIC  X(02).
      *受給者番号桁数最大
               05  SYS-1040-JKYSKETAMAX
                                        PIC  X(02).
      *予備
               05  SYS-1040-YOBI        PIC  X(482).
      *端末ＩＤ
           03  SYS-1040-TERMID          PIC  X(16).
      *オペーレータＩＤ
           03  SYS-1040-OPID            PIC  X(16).
      *作成年月日
           03  SYS-1040-CREYMD          PIC  X(08).
      *更新年月日
           03  SYS-1040-UPYMD           PIC  X(08).
      *更新時間
           03  SYS-1040-UPHMS           PIC  X(06).
      *主キー更新用項目
           03  SYS-1040-UPD-KEY.
               05  SYS-1040-UPD-HOSPNUM     PIC 9(2).
               05  SYS-1040-UPD-KANRICD     PIC X(04).
               05  SYS-1040-UPD-KBNCD       PIC X(08).
               05  SYS-1040-UPD-STYUKYMD    PIC X(08).
               05  SYS-1040-UPD-EDYUKYMD    PIC X(08).
