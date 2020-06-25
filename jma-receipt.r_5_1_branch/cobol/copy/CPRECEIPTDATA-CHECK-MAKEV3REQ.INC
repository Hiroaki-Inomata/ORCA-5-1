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
      *    ＡＰＩレセ電データ情報（チェック用レセ電データ作成）(xml2)
      *                        COPY (CPRECEIPTDATA-CHECK-MAKEV3REQ.INC)
      ******************************************************************
        01  XML-RECEIPTDATA-CHECK-MAKEREQ.
            02  RECEDMAKEQ-CONTEXT          PIC S9(9)   BINARY.
            02  RECEDMAKEQ-OBJECT           PIC X(36).
            02  RECEDMAKEQ-MODE             PIC S9(9)   BINARY.
            02  RECEDMAKEQ-RECORDNAME       PIC X(128).
            02  RECEDMAKEQ-PATIENTREQ.
      *         リクエスト番号
                03  RECEDMAKEQ-REQUEST-NUMBER
                                           PIC X(2).
      *         カルテUID
                03  RECEDMAKEQ-KARTE-UID   PIC X(36).
      *         オルカUID
                03  RECEDMAKEQ-ORCA-UID    PIC X(36).
      *         実施日
                03  RECEDMAKEQ-PERFORM-DATE
                                           PIC X(10).
      *         診療年月
                03  RECEDMAKEQ-PERFORM-MONTH
                                           PIC X(7).
      *         請求年月日
                03  RECEDMAKEQ-AC-DATE     PIC X(10).
      *         作成モード(Check:点検用 Ckeck以外:提出用)
                03  RECEDMAKEQ-CREATE-MODE PIC X(6).
      *         提出先
                03  RECEDMAKEQ-SUBMISSION-MODE
                                           PIC X(2).
      *         レセ電データチェック(Yes:チェックする Yes以外:チェックしない)
                03  RECEDMAKEQ-CHECK-MODE  PIC X(3).
      *         保険者番号(00000000:直接請求する保険者以外)
                03  RECEDMAKEQ-HKNJANUM    PIC X(8).
      *         期間指定
                03  RECEDMAKEQ-START-MONTH PIC X(7).
                03  RECEDMAKEQ-END-MONTH   PIC X(7).
      *         個別対象患者一覧
                03  RECEDMAKEQ-PATIENT-INFORMATION OCCURS  100.
      *             個別対象−患者番号
                    05  RECEDMAKEQ-PATIENT-ID      PIC X(20).
      *             個別対象−診療年月
                    05  RECEDMAKEQ-PATIENT-MONTH   PIC X(7).
      *             個別対象−入外区分
                    05  RECEDMAKEQ-PATIENT-INOUT   PIC X(2).
      *
