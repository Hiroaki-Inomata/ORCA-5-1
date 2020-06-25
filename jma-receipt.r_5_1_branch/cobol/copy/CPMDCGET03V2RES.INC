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
      ******************************************************************
      *    ＡＰＩレスポンス診療行為情報取得（受診年月診療内容）(xml2)
      *                                      COPY (CPMDCGET03V2RES.INC)
      ******************************************************************
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
        01  XML-MEDICALGET03RES.
            02  MDC03RES-CONTEXT           PIC S9(9)   BINARY.
            02  MDC03RES-OBJECT            PIC X(36).
            02  MDC03RES-MODE              PIC S9(9)   BINARY.
            02  MDC03RES-RECORDNAME        PIC X(128).
            02  MDC03RES-MEDICALRES.
      *       実施日
              03  MDC03RES-INFORMATION-DATE    PIC X(10).
      *       実施時間
              03  MDC03RES-INFORMATION-TIME    PIC X(8).
      *       返却コード
              03  MDC03RES-API-RESULT          PIC X(2).
      *       返却メッセージ
              03  MDC03RES-API-RESULT-MSG      PIC X(100).
      *
              03  MDC03RES-RESKEY              PIC X(100).
      *       患者情報
              03  MDC03RES-PATIENT-INFOMATION.
      *           患者番号
                  05  MDC03RES-PATIENTID         PIC X(20).
      *           患者氏名
                  05  MDC03RES-NAME              PIC X(100).
      *           患者カナ氏名
                  05  MDC03RES-KANANAME          PIC X(100).
      *           生年月日
                  05  MDC03RES-BIRTHDAY          PIC X(10).
      *           性別
                  05  MDC03RES-SEX               PIC X(1).
      *       受診年月
              03  MDC03RES-PERFORM-DATE        PIC X(07).
      *       診療内容情報
              03  MDC03RES-MEDICAL-INF         OCCURS   400.
      *           診療区分
                  05  MDC03RES-MDC-CLASS           PIC X(02).
      *           診療区分名称
                  05  MDC03RES-MDC-CLASS-NAME      PIC X(50).
      *           診療コード
                  05  MDC03RES-PRSCRPT-CODE        PIC X(9).
      *           診療名称
                  05  MDC03RES-PRSCRPT-NAME        PIC X(140).
      *           単位
                  05  MDC03RES-UNIT-CODE           PIC X(03).
      *           単位名称
                  05  MDC03RES-UNIT-NAME           PIC X(24).
      *           包括区分
                  05  MDC03RES-MDC-CODE            PIC X(01).
      *           包括区分（True：包括剤）
                  05  MDC03RES-INCLUSION-CLASS     PIC X(05).
      *           算定日合計
                  05  MDC03RES-MDC-CLASS-NUMBER    PIC X(02).
      *           算定日
                  05  MDC03RES-MDC-DAY-INF         PIC X(31).
      *           投薬情報
                  05  MDC03RES-MDC-DAY-G           OCCURS   31.
      *               算定日
                      07  MDC03RES-DAY                  PIC X(02).
      *               日合計使用量
                      07  MDC03RES-DAY-PRSCRPT-NUMBER   PIC X(11).
      *
