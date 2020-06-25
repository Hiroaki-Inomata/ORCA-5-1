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
      *    収集データ（病名情報）         COPY (CPCDF003.INC)    *
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-藤原    04/02/01  病名コード２１に変更
      *                                     病名コード数９（０２）に変更
      ************************************************************
      *01  COLDAT03-REC.
           03  COLDAT03-KEY.
      *        医療機関ＩＤ
               05  COLDAT03-HOSPID         PIC  X(24).
      *        診療年月
               05  COLDAT03-SRYYM          PIC  9(06).
      *        入外区分
               05  COLDAT03-NYUGAIKBN      PIC  X(01).
      *        患者ＩＤ
               05  COLDAT03-PTID           PIC  9(10).
      *        レセプト診療科
               05  COLDAT03-RECEKA         PIC  X(02).
      *        診療科
               05  COLDAT03-SRYKA          PIC X(02).
      *        診療開始日
               05  COLDAT03-SRYYMD         PIC X(08).
      *        連番号
               05  COLDAT03-RENNUM         PIC 9(02).
      *レセ電診療科
           03  COLDAT03-RECESRYKA      PIC  X(02).
      *病名コード数
           03  COLDAT03-BYOMEICDSU     PIC 9(02).
      *病名コード
           03  COLDAT03-BYOMEICD-G.
               05  COLDAT03-BYOMEICD   PIC X(07)
                                       OCCURS   21.
      *疑いフラグ
           03  COLDAT03-UTAGAIFLG      PIC X(01).
      *主病名フラグ
           03  COLDAT03-SYUBYOFLG      PIC X(01).
      *慢性フラグ
           03  COLDAT03-MANSEIKBN      PIC 9(02).
      *保険組合せ番号
           03  COLDAT03-HKNCOMBI       PIC 9(04).
      *レセプト表示フラグ
           03  COLDAT03-REZEFLG        PIC X(01).
      *転帰区分
           03  COLDAT03-TENKIKBN       PIC X(01).
      *転帰日
           03  COLDAT03-TENKIYMD       PIC X(08).
      *病名
           03  COLDAT03-BYOMEI         PIC X(160).
      *病名文字数
           03  COLDAT03-BYOMEIMOJI     PIC 9(02).
      *病名編集フラグ
           03  COLDAT03-BYOMEIHENFLG   PIC X(01).
      *保険病名フラグ
           03  COLDAT03-HKNBYOMEIFLG   PIC X(01).
