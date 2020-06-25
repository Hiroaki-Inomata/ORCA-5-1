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
      *    全角→半角変換用パラメタ      COPY (CPORCSZENHANCNV.INC)
      *                                  使用プログラム (ORCSZENHANCNV)
      *    設定項目
      *      処理区分  １：歴日  ２：数字
      *      入力文字
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *
       01  ORCSZENHANCNVAREA.
      *    処理区分    １：歴日
      *                ２：数字
      *                ３：文字
           03  LNK-ZENHANCNV-SYORI           PIC X(01).
      *    復帰コード
           03  LNK-ZENHANCNV-RC              PIC 9(01).
      *
      *    入力情報
           03  LNK-ZENHANCNV-IN.
      *        入力文字
               05  LNK-ZENHANCNV-IN-DATA     PIC X(50).
      *        入力チェック文字数
               05  LNK-ZENHANCNV-IN-LEN      PIC 9(02).
      *        入力文字数 
               05  LNK-ZENHANCNV-IN-LEN1     PIC 9(02).
      *
      *    出力情報
           03  LNK-ZENHANCNV-OUT.
      *        日付（西暦）
               05  LNK-ZENHANCNV-OT-SYMD    PIC X(08).
      *        日付（和暦）
               05  LNK-ZENHANCNV-OT-WYMD    PIC X(07).
      *        日付（表示）
               05  LNK-ZENHANCNV-OT-GYMD    PIC X(09).
      *        文字
               05  LNK-ZENHANCNV-OT-DATA    PIC X(25).
      *        
               05  LNK-ZENHANCNV-OT-LEN     PIC 9(02).
      *        数字
               05  LNK-ZENHANCNV-OT-NUM     PIC 9(18).
