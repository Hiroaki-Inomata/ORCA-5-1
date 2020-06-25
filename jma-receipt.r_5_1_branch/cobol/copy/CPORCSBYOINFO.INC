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
      *    病名一覧返却用パラメタ        COPY (CPORCSBYOINFO.INC)
      *                                  使用プログラム (ORCSBYOINFO)
      *    設定項目
      *      患者番号
      *      診療年月
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.08.01    NACL-藤原    16/02/29  合併症に係わる情報追加対応
      *                                     原疾患区分、合併症区分
      *  04.08.02    NACL-藤原    16/09/08  全ての患者病名情報の取得対応
      *  04.08.03    NACL-藤原    17/05/15  補足コメントコード返却対応
      *
      *  05.00.01    NACL-藤原    19/05/14  退院証明書返却対応
      *  05.00.02    NACL-藤原    19/06/17  第三者行為返却対応
      ************************************************************
      *
       01  ORCSBYOINFOAREA.
      *    患者番号
           03  SBYOINFO-PTNUM              PIC X(20).
      *    患者ＩＤ
           03  SBYOINFO-PTID               PIC 9(10).
      *    診療年月
           03  SBYOINFO-SRYYM              PIC 9(06).
      *
           03  SBYOINFO-SELECT-MODE        PIC X(03).
      *
      *    出力領域
           03  SBYOINFO-OUT-AREA.
      *        病名情報オーバーフラグ
      ***      05  SBYOINFO-OVERFLOW       PIC X(01).
      *
               05  SBYOINFO-OUT-TBL        OCCURS  200.
      *        診療科
                   07  SBYOINFO-SRYKA      PIC X(02).
      *        診療開始日
                   07  SBYOINFO-SRYYMD     PIC X(08).
      *        病名入力コード
                   07  SBYOINFO-BYOMEIINPUTCD-G.
                       09  SBYOINFO-BYOMEIINPUTCD
                                           PIC X(20)   OCCURS  6.
      *        病名コード
                   07  SBYOINFO-BYOMEICD-G.
                       09  SBYOINFO-BYOMEICD
                                           PIC X(07)   OCCURS   21.
      *        フラグ
                   07  SBYOINFO-FLG-G.
                       09  SBYOINFO-FLG    PIC X(01)   OCCURS   21.
      *        病名
                   07  SBYOINFO-BYOMEI-G.
                       09  SBYOINFO-BYOMEI-T
                                           PIC X(200)  OCCURS   21.
      *        病名
                   07  SBYOINFO-BYOMEI     PIC X(160).
      *        カルテ病名
                   07  SBYOINFO-CHARTBYOMEI
                                           PIC X(160).
      *        補足コメント
                   07  SBYOINFO-HOSOKU-COMMENT
                                           PIC X(80).
      *        補足コメントコード
                   07  SBYOINFO-HOSOKUCD-G.
                       09    SBYOINFO-HOSOKUCD
                                           PIC X(20)   OCCURS   3.
                   07  SBYOINFO-HOSOKUCD-COMMENT-G.
                       09  SBYOINFO-HOSOKUCD-COMMENT
                                           PIC X(80)   OCCURS   3.
      *        転帰区分
                   07  SBYOINFO-TENKIKBN   PIC X(01).
      *        転帰日
                   07  SBYOINFO-TENKIYMD   PIC X(08).
      *        疑いフラグ
                   07  SBYOINFO-UTAGAIFLG  PIC X(01).
      *        主病名フラグ
                   07  SBYOINFO-SYUBYOFLG  PIC X(01).
      *        慢性フラグ
                   07  SBYOINFO-MANSEIKBN  PIC 9(02).
      *        入外区分
                   07  SBYOINFO-NYUGAIKBN  PIC X(01).
      *        保険組合せ番号
                   07  SBYOINFO-HKNCOMBI   PIC 9(04).
      *        レセプト表示フラグ
                   07  SBYOINFO-REZEFLG    PIC X(01).
      *        レセプト表示月数
                   07  SBYOINFO-REZEMM     PIC 9(02).
      *        保険病名フラグ
                   07  SBYOINFO-HKNBYOMEIFLG
                                           PIC X(01).
      *        病名編集フラグ
                   07  SBYOINFO-BYOMEIHENFLG
                                           PIC X(01).
      *        原疾患区分
                   07  SBYOINFO-CLASS1     PIC 9(02).
      *        合併症区分
                   07  SBYOINFO-CLASS2     PIC 9(02).
      *        退院証明書
                   07  SBYOINFO-DISCHARGEFLG
                                           PIC X(1).
      *        公費ＩＤ（第三者行為のみ）
                   07  SBYOINFO-KOHID      PIC 9(10).
      *   リターンコード
           03  SBYOINFO-RC                 PIC 9(01).
