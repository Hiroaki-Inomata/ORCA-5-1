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
      *----------------------------------------------------------*
      *    診療行為入力項目変換パラメタ　  COPY (CPORCSC97.INC)
      *                              (ORCSC97 で使用）
      *
      *----------------------------------------------------------*
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.06.00    NACL-多々納  11/02/14  用法コメント対応
      *  04.08.00    NACL-多々納  13/12/20  行挿入対応
      ************************************************************
      *
       01  ORCSC97AREA.
      *    ＰＧ名
           03  LNK-SC97-PG             PIC X(06).
      *    処理区分（空白：全部、
      *              １：半角変換のみ）
           03  LNK-SC97-KBN            PIC X(01).
      *    入力出力領域
           03  LNK-SC97-INPUTCD        PIC X(54).
      *    ＳＰＡライン
           03  LNK-SC97-GMN-IDX        PIC 9(04).
      *    画面ライン
           03  LNK-SC97-MAP-IDX        PIC 9(04).
      *    時間外特例医療機関
           03  LNK-SC97-JIKANTOKU          PIC  9(01).
      *
      *    出力領域
           03  LNK-SC97-OUTAREA.
      *        ’＿’有無
               05  LNK-SC97-JODOU          PIC 9(01).
      *        コード変更有無（０：変更なし、１：変更あり）
               05  LNK-SC97-NYURYOKU       PIC 9(01).
      *        入力コード
               05  LNK-SC97-INCD           PIC X(54).
      *        入力コード桁数
               05  LNK-SC97-INCDCNT        PIC 9(02).
      *        入力コード種別
               05  LNK-SC97-CDSYU          PIC X(01).
      *        編集値
               05  LNK-SC97-INPUT-G.
      *        数量
                   07  LNK-SC97-SURYO          PIC 9(07)V9(05).
      *        回数
                   07  LNK-SC97-KAISU          PIC 9(03).
      *        分画数
                   07  LNK-SC97-BUNGSU         PIC 9(03).
      *        その入力値
                   07  LNK-SC97-MOJI1          PIC X(10).
                   07  LNK-SC97-MOJI2          PIC X(10).
                   07  LNK-SC97-MOJI3          PIC X(10).
                   07  LNK-SC97-MOJI4          PIC X(10).
                   07  LNK-SC97-MOJI5          PIC X(10).
      *        時間外フラグ
                   07  LNK-SC97-JIKAN          PIC 9(01).
      *
      *        数量
                   07  LNK-SC97-SURYOFLG      PIC 9(02).
                   07  LNK-SC97-SURYOCNT      PIC 9(02).
      *        回数（＊アリ）
                   07  LNK-SC97-KAISUCNT       PIC 9(02).
      *        回数（＊アリ）
                   07  LNK-SC97-KAISUFLG       PIC 9(01).
      *        分画数
                   07  LNK-SC97-BUNGSUFLG      PIC 9(01).
      *        その入力値
                   07  LNK-SC97-MCNT1          PIC 9(02).
                   07  LNK-SC97-MCNT2          PIC 9(02).
                   07  LNK-SC97-MCNT3          PIC 9(02).
                   07  LNK-SC97-MCNT4          PIC 9(02).
                   07  LNK-SC97-MCNT5          PIC 9(02).
      *        残量廃棄
                   07  LNK-SC97-AUTOFLG2        PIC X(01).
      *        換算値入力
                   07  LNK-SC97-INKBNFLG1       PIC X(01).
      *        内服種類数
                   07  LNK-SC97-INKBNFLG2       PIC X(01).
      *        継続コメント
                   07  LNK-SC97-CONTFLG         PIC X(01).
      *!!!!!!!!!!!!!!!!!!
      *        挿入行数
                   07  LNK-SC97-INSCNT          PIC 9(02).
      *        挿入連続
                   07  LNK-SC97-INSREN          PIC 9(01).
      *!!!!!!!!!!!!!!!!!!
      *
      *        検索画面へ
               05  LNK-SC97-KENSAKU        PIC X(01).
      *        入院回数
               05  LNK-SC97-NYUIN-KAISU.
                   07  LNK-SC97-NYU-DAY    PIC 9(02)
                                           OCCURS   31.
      *
