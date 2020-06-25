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
      *  患者病名マスター       (CPPTBYOMEI.INC)
      *****************************************************************
      *  修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-藤原     01/06/06  保険病名フラグの追加
      *  01/00/02    MCC-藤原     01.12.06  新しいレイアウトに変更     
      *
      *  新患者病名マスタ対応
      *  01.02.01    NACL-藤原    04/01/07  新しいレイアウトに変更
      *ver.2.6.0 
      *  01.02.02    NACL-藤原    05/05/26  病名検索用項目を追加
      *
      *  03.04.01    NACL-藤原    06/10/31  削除フラグ、削除オペレータ
      *                                     ＩＤを追加
      *
      *  04.06.01    NACL-藤原    10/06/20  病名の入力順対応
      *                                     入力順連番を追加 
      *  04.06.02    NACL-藤原    10/11/05  補足コメント対応
      *
      *  04.07.01    NACL-藤原    13/07/12  第三者行為対応
      *  04.07.02    NACL-藤原    15/07/29  患者病名登録時の登録時間記録
      *                                     と画面表示対応
      *                                     登録時間を追加
      *
      *  04.08.01    NACL-藤原    16/02/17  退院証明書の傷病名欄記載対応
      *                                     合併症に係わる情報追加対応
      *****************************************************************
      *
      *01  PTBYOMEI-REC.
           03  PTBYO-KEY.
      *医療機関ＩＤ
               05  PTBYO-HOSPNUM        PIC 9(2).
      *患者ＩＤ
               05  PTBYO-PTID          PIC 9(10).
      *診療科
               05  PTBYO-SRYKA         PIC X(02).
      *診療開始日
               05  PTBYO-SRYYMD        PIC X(08).
      *連番号
               05  PTBYO-RENNUM        PIC 9(02).
      *基本病名コード
           03  PTBYO-KHNBYOMEICD       PIC X(07).
      *病名入力コード
           03  PTBYO-BYOMEIINPUTCD-G.
               05  PTBYO-BYOMEIINPUTCD PIC X(20)
                                       OCCURS  6.
      *病名コード数
           03  PTBYO-BYOMEICDSU        PIC 9(02).
      *病名コード
           03  PTBYO-BYOMEICD-G.
               05  PTBYO-BYOMEICD-1    PIC X(07).
               05  PTBYO-BYOMEICD-2    PIC X(07).
               05  PTBYO-BYOMEICD-3    PIC X(07).
               05  PTBYO-BYOMEICD-4    PIC X(07).
               05  PTBYO-BYOMEICD-5    PIC X(07).
               05  PTBYO-BYOMEICD-6    PIC X(07).
               05  PTBYO-BYOMEICD-7    PIC X(07).
               05  PTBYO-BYOMEICD-8    PIC X(07).
               05  PTBYO-BYOMEICD-9    PIC X(07).
               05  PTBYO-BYOMEICD-10   PIC X(07).
               05  PTBYO-BYOMEICD-11   PIC X(07).
               05  PTBYO-BYOMEICD-12   PIC X(07).
               05  PTBYO-BYOMEICD-13   PIC X(07).
               05  PTBYO-BYOMEICD-14   PIC X(07).
               05  PTBYO-BYOMEICD-15   PIC X(07).
               05  PTBYO-BYOMEICD-16   PIC X(07).
               05  PTBYO-BYOMEICD-17   PIC X(07).
               05  PTBYO-BYOMEICD-18   PIC X(07).
               05  PTBYO-BYOMEICD-19   PIC X(07).
               05  PTBYO-BYOMEICD-20   PIC X(07).
               05  PTBYO-BYOMEICD-21   PIC X(07).
      *
           03  PTBYO-BYOMEICD-R        REDEFINES   PTBYO-BYOMEICD-G.
               05  PTBYO-BYOMEICD      PIC X(07)
                                       OCCURS   21.
      *疑いフラグ
           03  PTBYO-UTAGAIFLG         PIC X(01).
      *主病名フラグ
           03  PTBYO-SYUBYOFLG         PIC X(01).
      *慢性フラグ
           03  PTBYO-MANSEIKBN         PIC 9(02).
      *入外区分
           03  PTBYO-NYUGAIKBN         PIC X(01).
      *保険組合せ番号
           03  PTBYO-HKNCOMBI          PIC 9(04).
      *公費ＩＤ（第三者行為のみ）
           03  PTBYO-KOHID             PIC 9(10).
      *レセプト表示フラグ
           03  PTBYO-REZEFLG           PIC X(01).
      *レセプト表示月数
           03  PTBYO-REZEMM            PIC 9(02).
      *転帰区分
           03  PTBYO-TENKIKBN          PIC X(01).
      *転帰日
           03  PTBYO-TENKIYMD          PIC X(08).
      *病名
           03  PTBYO-BYOMEI            PIC X(160).
      *病名文字数
           03  PTBYO-BYOMEIMOJI        PIC 9(02).
      *カルテ病名
           03  PTBYO-CHARTBYOMEI       PIC X(160).
      *カルテ病名文字数
           03  PTBYO-CHARTBYOMEIMOJI   PIC 9(02).
      *補足コメント
           03  PTBYO-HOSOKU-COMMENT    PIC X(80).
      *補足コメントコード
           03  PTBYO-HOSOKUCD-G.
               05  PTBYO-HOSOKUCD      PIC X(20)
                                       OCCURS   3.
      *病名編集フラグ
           03  PTBYO-BYOMEIHENFLG      PIC X(01).
      *保険病名フラグ
           03  PTBYO-HKNBYOMEIFLG      PIC X(01).
      *レセ電エラーフラグ
           03  PTBYO-RECEDENFLG        PIC X(01).
      *保険番号
           03  PTBYO-HKNNUM            PIC X(03).
      *基本部位コード
           03  PTBYO-KHNBUICD-G.
               05  PTBYO-KHNBUICD      PIC X(07)
                                       OCCURS  3.
      *入力順連番
           03  PTBYO-SEQNUM            PIC 9(04).
      *退院証明書記載フラグ
           03  PTBYO-DISCHARGEFLG      PIC X(01).
      *原疾患区分
           03  PTBYO-CLASS1            PIC 9(02).
      *合併症区分
           03  PTBYO-CLASS2            PIC 9(02).
      *削除フラグ
           03  PTBYO-DLTFLG            PIC X(01).
      *削除オペレータＩＤ
           03  PTBYO-DLT-OPID          PIC X(16).
      *端末ＩＤ
           03  PTBYO-TERMID            PIC X(16).
      *オペレータＩＤ
           03  PTBYO-OPID              PIC X(16).
      *作成年月日
           03  PTBYO-CREYMD            PIC X(08).
      *作成時間
           03  PTBYO-CREHMS            PIC X(06).
      *更新年月日
           03  PTBYO-UPYMD             PIC X(08).
      *更新時間
           03  PTBYO-UPHMS             PIC X(06).
      *
      *病名検索用項目
           03  PTBYO-XXBYOMEI         PIC X(160).
