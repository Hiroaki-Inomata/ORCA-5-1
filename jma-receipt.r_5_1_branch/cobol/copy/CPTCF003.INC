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
      *    月次統計データ（病名情報）     COPY (CPTCF003.INC)    *
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-藤原    04/02/01  病名コード２１に変更
      *                                     病名コード数９（０２）に変更
      *
      *  03.05.01    NACL-藤原    07/04/19  グループ診療対応
      *
      *  04.06.01    NACL-藤原    12/10/26  特記事項０７（老併）及び
      *                                     ０８（老健）のレセプト対応
      ************************************************************
      *01  TOUKEI03-REC.
           03  TOUKEI03-KEY.
      *        医療機関ＩＤ
               05  TOUKEI03-HOSPNUM        PIC 9(2).
      *        診療年月
               05  TOUKEI03-SRYYM          PIC  9(06).
      *        入外区分
               05  TOUKEI03-NYUGAIKBN      PIC  X(01).
      *        患者ＩＤ
               05  TOUKEI03-PTID           PIC  9(10).
      *        入所中区分
               05  TOUKEI03-RECEIPT-KBN    PIC  X(10).
      *        レセプト診療科
               05  TOUKEI03-RECEKA         PIC  X(02).
      *        診療科
               05  TOUKEI03-SRYKA          PIC X(02).
      *        診療開始日
               05  TOUKEI03-SRYYMD         PIC X(08).
      *        連番号
               05  TOUKEI03-RENNUM         PIC 9(02).
      *レセ電診療科
           03  TOUKEI03-RECESRYKA      PIC  X(02).
      *病名コード数
           03  TOUKEI03-BYOMEICDSU     PIC 9(02).
      *病名コード
           03  TOUKEI03-BYOMEICD-G.
               05  TOUKEI03-BYOMEICD   PIC X(07)
                                       OCCURS   21.
      *疑いフラグ
           03  TOUKEI03-UTAGAIFLG      PIC X(01).
      *主病名フラグ
           03  TOUKEI03-SYUBYOFLG      PIC X(01).
      *慢性フラグ
           03  TOUKEI03-MANSEIKBN      PIC 9(02).
      *保険組合せ番号
           03  TOUKEI03-HKNCOMBI       PIC 9(04).
      *レセプト表示フラグ
           03  TOUKEI03-REZEFLG        PIC X(01).
      *転帰区分
           03  TOUKEI03-TENKIKBN       PIC X(01).
      *転帰日
           03  TOUKEI03-TENKIYMD       PIC X(08).
      *病名
           03  TOUKEI03-BYOMEI         PIC X(160).
      *病名文字数
           03  TOUKEI03-BYOMEIMOJI     PIC 9(02).
      *病名編集フラグ
           03  TOUKEI03-BYOMEIHENFLG   PIC X(01).
      *保険病名フラグ
           03  TOUKEI03-HKNBYOMEIFLG   PIC X(01).
