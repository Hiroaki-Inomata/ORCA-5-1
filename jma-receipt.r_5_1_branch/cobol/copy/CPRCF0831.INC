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
      *    労災レセプト明細書（入院）                            *
      *            （病名並替え）   COPY (CPRCF0831.INC)         *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-藤原    04/02/01  病名コード２１に変更
      *  03.05.01    NACL-門脇    07/06/01  グループ診療対応
      *
      *  04.07.01    NACL-藤原    13/01/21  労災レセ電対応
      ************************************************************
      *01  RECE831-REC.
           03  RECE831-KEY.
               05  RECE831-KEY1.
      *医療機関ＩＤ
                   07  RECE831-HOSPNUM     PIC 9(02).
      *診療年月
                   07  RECE831-SRYYM       PIC 9(06).
      *入外区分
                   07  RECE831-NYUGAIKBN   PIC X(01).
      *患者ＩＤ
                   07  RECE831-PTID        PIC 9(10).
      *レセプト科
                   07  RECE831-RECEKA      PIC X(02).
      *保険ＩＤ        
                   07  RECE831-HKNID       PIC 9(10).
      *適用開始年月日
                   07  RECE831-TEKSTYMD    PIC X(08). 
      *主病名フラグ
               05  RECE831-SYUBYOFLG       PIC X(01).
      *診療開始日
               05  RECE831-SRYYMD          PIC X(08).
      *転帰区分
               05  RECE831-TENKIKBN        PIC X(01).
      *転帰日
               05  RECE831-TENKIYMD        PIC X(08).
      *連番号
               05  RECE831-RENNUM          PIC 9(03).
      *患者番号
           03  RECE831-PTNUM               PIC X(20).
      *診療科
           03  RECE831-SRYKA               PIC X(02).
      *基本病名コード
           03  RECE831-KHNBYOMEICD         PIC X(07).
      *病名コード
           03  RECE831-BYOMEICD-G.
               05  RECE831-BYOMEICD        PIC X(07)
                                           OCCURS   21.
      *疑いフラグ
           03  RECE831-UTAGAIFLG           PIC X(01).
      *慢性フラグ
           03  RECE831-MANSEIKBN           PIC 9(02).
      *保険組合番号
           03  RECE831-HKNCOMBI            PIC X(04).
      *レセプト表示フラグ
           03  RECE831-REZEFLG             PIC X(01).
      *レセプト表示月数
           03  RECE831-REZEMM              PIC 9(02).
      *病名
           03  RECE831-BYOMEI              PIC X(160).
      *病名文字数
           03  RECE831-BYOMEIMOJI          PIC 9(03).
      *補足コメント
           03  RECE831-HOSOKU-COMMENT      PIC X(40).
      *入力病名
           03  RECE831-IN-BYOMEI           PIC X(160).
      *入力病名文字数
           03  RECE831-IN-BYOMEIMOJI       PIC 9(03).
      *病名編集フラグ
           03  RECE831-BYOMEIHENFLG        PIC X(01).
      *レセプト種別
           03  RECE831-RECESYUBETU         PIC 9(04).
