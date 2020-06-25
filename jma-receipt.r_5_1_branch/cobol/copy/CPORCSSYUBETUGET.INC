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
      *    レセプト種別返却パラメタ      COPY (CPORCSSYUBETUGET.INC)
      *                                  使用プログラム (ORCSSYUBETUGET)
      *    設定項目
      *        入外区分
      *        診療年月
      *        保険番号
      *        国保区分
      *        社保区分
      *        老人区分
      *        公費数
      *        本人家族区分
      *    返却項目
      *        レセプト種別
      *        レセプト種別（特別療養費）
      *        レセプト種別（紙レセプト）
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.03.01    NACL-藤原    08/05/07  公費４種までの併用の記録対応
      *                                     レセプト種別（紙レセプト）の追加
      *
      *  04.07.01    NACL-藤原    14/03/30  平成２６年４月改正対応
      ************************************************************
      *
       01  ORCSSYUBETUGETAREA.
      *
           03  LNK-SYUBETUGET-SYOFLG           PIC 9(01).
      *
           03  LNK-SYUBETUGET-IN.
               05  LNK-SYUBETUGET-NYUGAIKBN    PIC X(01).
               05  LNK-SYUBETUGET-SRYYM        PIC 9(06).
               05  LNK-SYUBETUGET-HKNNUM       PIC X(03).
      *
               05  LNK-SYUBETUGET-KOKUHO       PIC X(01).
               05  LNK-SYUBETUGET-IHO          PIC X(01).
               05  LNK-SYUBETUGET-RJN          PIC X(01).
               05  LNK-SYUBETUGET-KOHI-CNT     PIC 9(01).
               05  LNK-SYUBETUGET-HONKZKKBN    PIC X(01).
      *
               05  LNK-SYUBETUGET-PREFNUM      PIC X(02).
               05  LNK-SYUBETUGET-HKN-HONKZKKBN
                                               PIC X(01).
               05  LNK-SYUBETUGET-HKN-HOJOKBN  PIC X(01).
               05  LNK-SYUBETUGET-HKN-HKNKOHSBTKBN
                                               PIC X(01).
      * 
               05  LNK-SYUBETUGET-HKNKBN       PIC X(01).
               05  LNK-SYUBETUGET-SAIGAIKBN    PIC X(01).
      *
               05  LNK-SYUBETUGET-KOHI-TBL     OCCURS  10.
                   07  LNK-SYUBETUGET-KOHID        PIC 9(10).
                   07  LNK-SYUBETUGET-KOHNUM       PIC X(03).
                   07  LNK-SYUBETUGET-KOHPAYKBN    PIC X(02).
                   07  LNK-SYUBETUGET-KOHHBTNUM    PIC X(02).
                   07  LNK-SYUBETUGET-RECESKYKBN   PIC X(01).
      *
               05  LNK-SYUBETUGET-AGE          PIC 9(03).
               05  LNK-SYUBETUGET-SYUGAKUKBN   PIC 9(01).
               05  LNK-SYUBETUGET-BIRTHDAY     PIC 9(08).
      *
               05  LNK-SYUBETUGET-RJNHKNNUM    PIC X(03).
               05  LNK-SYUBETUGET-RJNPAYKBN    PIC X(02).
      *
           03  LNK-SYUBETUGET-OT.
      *        レセプト種別（レセ電）
               05  LNK-SYUBETUGET-RECESYUBETU  PIC 9(04).
      *        レセプト種別（特別療養費）
               05  LNK-SYUBETUGET-RECESYUBETU-X
                                               PIC 9(04).
      *        レセプト種別（紙レセプト）
               05  LNK-SYUBETUGET-RECESYUBETU-Y
                                               PIC 9(04).
