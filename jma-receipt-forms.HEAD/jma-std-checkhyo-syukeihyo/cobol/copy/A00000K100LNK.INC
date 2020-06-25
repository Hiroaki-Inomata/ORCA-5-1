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
      *******************************************************************
      *    診療行為区分別集計表サブプログラム アギユメント
      *******************************************************************
      *  プログラム修正履歴
      *  Maj/Min/Rev  修正者      日付      内容
      *   01.00.01    @@@@-田中   10/12/14  コメント履歴削除
      *   02.00.01    @@@@-門間   18/02/01  金額表示機能の追加
      *******************************************************************
       01  A00000K100-LNK.
           03  K100LNK-PRTKANRI-RENNUM              PIC 9(04).
           03  K100LNK-PRTKANRI-TBL-KEY             PIC X(08).
           03  K100LNK-PRTKANRI-TBL-GROUP           PIC X(14).
           03  K100LNK-PRTKANRI-SHORI-RENNUM        PIC 9(04).
           03  K100LNK-PRTKANRI-SRYYM               PIC X(06).
           03  K100LNK-PRTKANRI-SKYYMD              PIC X(08).
           03  K100LNK-PRTKANRI-SHELLID             PIC X(20).
           03  K100LNK-PRTKANRI-PRIORITY            PIC 9(04).
           03  K100LNK-PRTKANRI-TERMID              PIC X(64).
           03  K100LNK-PRTKANRI-OPID                PIC X(16).
           03  K100LNK-PRTKANRI-PRTNM               PIC X(16).
           03  K100LNK-JOBID                        PIC 9(07).
           03  K100LNK-SHELLID                      PIC X(08).
           03  K100LNK-HOSPNUM                      PIC 9(02).
           03  K100LNK-SRYYM                        PIC X(08).
           03  K100LNK-NYUGAIKBN                    PIC X(01).
           03  K100LNK-HKNTG                        PIC X(01).
           03  K100LNK-HOUKATU                      PIC X(01).
           03  K100LNK-TKKBN                        PIC X(01).

