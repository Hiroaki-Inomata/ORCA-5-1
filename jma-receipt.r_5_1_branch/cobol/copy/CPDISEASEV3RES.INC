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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  05.00.01    NACL-藤原    18/05/01  急性フラグの追加
      *                                     傷病名コードを２１に変更
      *****************************************************************
      ******************************************************************
      *    ＡＰＩ患者病名情報             (xml2)
      *                                   COPY (CPDISEASEV3RES.INC)
      ******************************************************************
       01  XML-DISEASERES.
           03  DISEASES-CONTEXT            PIC S9(9)   BINARY.
           03  DISEASES-OBJECT             PIC X(36).
           03  DISEASES-MODE               PIC S9(9)   BINARY.
           03  DISEASES-RECORDNAME         PIC X(128).
           03  DISEASES-DISEASE-RES.
      *        実施日
               05  DISEASES-INFORMATION-DATE
                                           PIC X(10).
      *        実施時間
               05  DISEASES-INFORMATION-TIME
                                           PIC X(8).
      *        返却コード
               05  DISEASES-API-RESULT     PIC X(3).
      *        返却メッセージ
               05  DISEASES-API-RESULT-MSG PIC X(100).
      *
               05  DISEASES-RESKEY         PIC X(100).
      *
               05  DISEASES-PERFORM-DATE   PIC X(10).
               05  DISEASES-PERFORM-TIME   PIC X(8).
               05  DISEASES-DPERTMENT-CODE PIC X(2).
               05  DISEASES-DPERTMENT-NAME PIC X(80).
               05  DISEASES-PATIENT-ID     PIC X(20).
               05  DISEASES-BASE-MONTH     PIC X(7).
               05  DISEASES-DIAG-TBL-G     OCCURS   50.
      *            エラーコード
                   07  DISEASES-RESULT         PIC X(3).
      *            メッセージ
                   07  DISEASES-RESULT-MSG     PIC X(100).
       *            警告コード
                   07  DISEASES-DIAG-WARNING   PIC X(03).
      *            メッセージ
                   07  DISEASES-DIAG-MSG       PIC X(100).
      *            行番号
                   07  DISEASES-DIAG-POSITION  PIC X(02).
      *            疾患開始日
                   07   DISEASES-DIAG-STARTDATE
                                               PIC X(10).
      *            病名
                   07  DISEASES-DIAG-NAME      PIC X(80).
      *            病名コード
                   07  DISEASES-DIAG-CODE      PIC X(155).
      *            移行(廃止)
                   07  DISEASES-DIAG-CHANGE    PIC X(02).
      *        不一致病名情報
               05  DISEASES-UNMATCH-INFORMATION.
      *            不一致病名情報オーバーフラグ
                   07  DISEASES-INFORMATION-OVERFLOW
                                               PIC X(5).
      *            不一致病名一覧
                   07  DISEASES-UNMATCH-INFORMATION    OCCURS  50.
                       09  DISEASES-SPICOD1    PIC X(155).
                       09  DISEASES-SPINAME1   PIC X(80).
      *                補足コメント
                       09  DISEASES-HOSOKU-COMMENT
                                               PIC X(80).
      *                補足コメントコード
                       09  DISEASES-HOSOKUCD-G.
                           11  DISEASES-HOSOKUCD-OCC   OCCURS  3.
                               13  DISEASES-HOSOKUCD-O
                                               PIC X(20).
                               13  DISEASES-HOSOKUCD-COMMENT-O
                                               PIC X(80).
      *                入外区分
                       09  DISEASES-NYUGAIKBN  PIC X(1).
      *                主病
                       09  DISEASES-SYUOBYOFLG PIC X(2).
      *                疑いフラグ
                       09  DISEASES-UTAGAIFLG  PIC X(1).
      *                急性フラグ
                       09  DISEASES-KYUSEIFLG  PIC X(1).
      *                疾患開始日
                       09  DISEASES-STARTDATE  PIC X(10).
      *                疾患終了日
                       09  DISEASES-ENDDATE    PIC X(10).
      *                転帰
                       09  DISEASES-TENKIKBN   PIC X(1).
      *                カルテ病名
                       09  DISEASES-KARTE-NAME PIC X(80).
      *                疾患区分
                       09  DISEASES-MANSEIKBN  PIC X(2).
      *                保険適用
                       09  DISEASES-HKNCOMBI   PIC X(4).
      *                レセプト表示
                       09  DISEASES-REZEFLG    PIC X(1).
      *                表示期間
                       09  DISEASES-REZEMM     PIC X(2).
      *                保険病名
                       09  DISEASES-HKNBYOMEIFLG
                                               PIC X(1).
      *                退院証明書
                       09  DISEASES-DISCHARGEFLG
                                               PIC X(1).
      *                原疾患区分
                       09  DISEASES-CLASS1     PIC X(2).
      *                合併症区分
                       09  DISEASES-CLASS2     PIC X(2).
