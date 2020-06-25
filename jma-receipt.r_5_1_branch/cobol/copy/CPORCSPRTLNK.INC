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
000100************************************************************
000200* 印刷ＤＢ用情報領域      CPORCSPRTLNK.INC                 *
000300************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-藤原    03/03/10  端末ＩＤを６４に変更
      ************************************************************
           03  LNK-PRTKANRI-AREA.
      *    連番
               05  LNK-PRTKANRI-RENNUM    PIC 9(04).
      *    ＫＥＹ
               05  LNK-PRTKANRI-TBL-KEY   PIC X(08).
      *    グループ
               05  LNK-PRTKANRI-TBL-GROUP PIC X(14).
      *    UUID
               05  LNK-PRTKANRI-TBL-UUID  PIC X(36).
      *    処理内連番
               05  LNK-PRTKANRI-SHORI-RENNUM
                                          PIC 9(04).
      *    業務コード
               05  LNK-PRTKANRI-GYOUMU-CD
                                          PIC 9(03).
      *    診療年月
               05  LNK-PRTKANRI-SRYYM     PIC X(06).
      *    請求年月日
               05  LNK-PRTKANRI-SKYYMD    PIC X(08).
      *    シェルＩＤ
               05  LNK-PRTKANRI-SHELLID   PIC X(20).
      *    印刷順連番
               05  LNK-PRTKANRI-PRIORITY  PIC 9(04).
      *    端末ＩＤ
               05  LNK-PRTKANRI-TERMID    PIC X(64).
      *    オペレータＩＤ
               05  LNK-PRTKANRI-OPID      PIC X(16).
      *    プリンタ名
               05  LNK-PRTKANRI-PRTNM     PIC X(16).
