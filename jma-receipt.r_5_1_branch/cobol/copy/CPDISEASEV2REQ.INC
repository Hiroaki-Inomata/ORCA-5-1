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
      *****************************************************************
      ******************************************************************
      *    ＡＰＩ患者病名情報             (xml2)
      *                                   COPY (CPDISEASEV3REQ.INC)
      ******************************************************************
       01  XML-DISEASEREQ.
           03  DISEASEQ-CONTEXT            PIC S9(9)   BINARY.
           03  DISEASEQ-OBJECT             PIC X(36).
           03  DISEASEQ-MODE               PIC S9(9)   BINARY.
           03  DISEASEQ-RECORDNAME         PIC X(128).
           03  DISEASEQ-PATIENTREQ.
               05  DISEASEQ-PATIENT-ID     PIC X(20).
               05  DISEASEQ-BASE-MONTH     PIC X(7).
               05  DISEASEQ-PERFORM-DATE   PIC X(10).
               05  DISEASEQ-PERFORM-TIME   PIC X(8).
               05  DISEASEQ-DEPARTMENT-CODE
                                           PIC X(2).
      *
               05  DISEASEQ-INFORMATION        OCCURS  50.
      *            保険区分（１：医保以外）
                   07   DISEASEQ-INSURANCE-CLASS
                                               PIC X(1).
                   07  DISEASEQ-SPICOD1        PIC X(50).
                   07  DISEASEQ-SPINAME1       PIC X(80).
                   07  DISEASEQ-SNDSPITBL         OCCURS  6.
                       09   DISEASEQ-SPICOD2   PIC X(7).
                       09   DISEASEQ-SPINAME2  PIC X(20).
      *            補足コメントコード
                   07   DISEASEQ-HOSOKUCD-G.
                       09   DISEASEQ-HOSOKUCD  PIC X(10)
                                                   OCCURS  3.
      *            補足コメント
                   07   DISEASEQ-HOSOKU-COMMENT
                                               PIC X(40).
      *            入外区分
                   07   DISEASEQ-NYUGAIKBN     PIC X(1).
      *            主病
                   07   DISEASEQ-SYUBYOFLG     PIC X(2).
      *            疑いフラグ
                   07   DISEASEQ-UTAGAIFLG     PIC X(1).
      *            疾患開始日
                   07   DISEASEQ-STARTDATE     PIC X(10).
      *            疾患終了日
                   07   DISEASEQ-ENDDATE       PIC X(10).
      *            転帰
                   07   DISEASEQ-TENKIKBN      PIC X(2).
      *            カルテ病名
                   07   DISEASEQ-KARTE-NAME    PIC X(80).
      *            疾患区分
                   07   DISEASEQ-MANSEIKBN     PIC X(4).
      *            保険適用
                   07   DISEASEQ-HKNCOMBI      PIC X(4).
      *            レセプト表示
                   07   DISEASEQ-REZEFLG       PIC X(4).
      *            表示期間
                   07   DISEASEQ-REZEMM        PIC X(4).
      *            保険病名
                   07   DISEASEQ-HKNBYOMEIFLG  PIC X(4).
      *            退院証明書
                   07   DISEASEQ-DISCHARGEFLG  PIC X(4).
      *            原疾患区分
                   07   DISEASEQ-CLASS1        PIC X(4).
      *            合併症区分
                   07   DISEASEQ-CLASS2        PIC X(4).
      *
