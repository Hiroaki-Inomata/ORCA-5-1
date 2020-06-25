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
      *****************************************************************
      *  適応病名マスタ       (CPTEKIOUBYOMEIWK.INC)
      *****************************************************************
      *****************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *01  TEKIOUBYOMEIWK-REC.
      *
      *処理区分
           03  TEKBYOWK-SYORIKBN          PIC X(01).
           03  TEKBYOWK-KEY.
      *チェック区分
               05  TEKBYOWK-CHKKBN        PIC X(01).
      *診療行為コード
               05  TEKBYOWK-SRYCD         PIC X(09).
      *連番号
               05  TEKBYOWK-RENNUM        PIC 9(04).
      *適応病名
           03  TEKBYOWK-BYOMEI            PIC X(400).
      *病名コード数
           03  TEKBYOWK-BYOMEICDSU        PIC 9(02).
      *病名コード
           03  TEKBYOWK-BYOMEICD-G.
               05  TEKBYOWK-BYOMEICD-1    PIC X(07).
               05  TEKBYOWK-BYOMEICD-2    PIC X(07).
               05  TEKBYOWK-BYOMEICD-3    PIC X(07).
               05  TEKBYOWK-BYOMEICD-4    PIC X(07).
               05  TEKBYOWK-BYOMEICD-5    PIC X(07).
               05  TEKBYOWK-BYOMEICD-6    PIC X(07).
               05  TEKBYOWK-BYOMEICD-7    PIC X(07).
               05  TEKBYOWK-BYOMEICD-8    PIC X(07).
               05  TEKBYOWK-BYOMEICD-9    PIC X(07).
               05  TEKBYOWK-BYOMEICD-10   PIC X(07).
               05  TEKBYOWK-BYOMEICD-11   PIC X(07).
               05  TEKBYOWK-BYOMEICD-12   PIC X(07).
               05  TEKBYOWK-BYOMEICD-13   PIC X(07).
               05  TEKBYOWK-BYOMEICD-14   PIC X(07).
               05  TEKBYOWK-BYOMEICD-15   PIC X(07).
               05  TEKBYOWK-BYOMEICD-16   PIC X(07).
               05  TEKBYOWK-BYOMEICD-17   PIC X(07).
               05  TEKBYOWK-BYOMEICD-18   PIC X(07).
               05  TEKBYOWK-BYOMEICD-19   PIC X(07).
               05  TEKBYOWK-BYOMEICD-20   PIC X(07).
               05  TEKBYOWK-BYOMEICD-21   PIC X(07).
      *
           03  TEKBYOWK-BYOMEICD-R        REDEFINES TEKBYOWK-BYOMEICD-G.
               05  TEKBYOWK-BYOMEICD      PIC X(07)
                                          OCCURS   21.
      *病名編集フラグ
           03  TEKBYOWK-BYOMEIHENFLG      PIC X(01).
      *基本病名コード
           03  TEKBYOWK-KHNBYOMEICD       PIC X(07).
      *基本部位コード１
           03  TEKBYOWK-KHNBUICD-1        PIC X(07).
      *基本部位コード２
           03  TEKBYOWK-KHNBUICD-2        PIC X(07).
      *基本部位コード３
           03  TEKBYOWK-KHNBUICD-3        PIC X(07).
      *端末ＩＤ
           03  TEKBYOWK-TERMID            PIC X(32).
      *オペーレータＩＤ
           03  TEKBYOWK-OPID              PIC X(16).
      *作成年月日
           03  TEKBYOWK-CREYMD            PIC X(08).
      *更新年月日
           03  TEKBYOWK-UPYMD             PIC X(08).
      *更新時間
           03  TEKBYOWK-UPHMS             PIC X(06).
