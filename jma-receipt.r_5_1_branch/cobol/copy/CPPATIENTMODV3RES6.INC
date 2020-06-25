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
      *    ＡＰＩ患者登録情報（介護情報）(一体化　V3)
      *                                   COPY (CPPATIENTMODV3RES6.INC)
      ******************************************************************
       01  XML-PATIENTMODRES.
           02  PTMODRES-CONTEXT           PIC S9(9)   BINARY.
           02  PTMODRES-OBJECT            PIC X(36).
           02  PTMODRES-MODE              PIC S9(9)   BINARY.
           02  PTMODRES-RECORDNAME        PIC X(128).
           02  PTMODRES-APPOINTRES.
      *      リクエスト番号
             03  PTMODRES-REQUEST-NUMBER    PIC X(2).
      *      レスポンス番号
             03  PTMODRES-RESPONSE-NUMBER   PIC X(2).
      *      UID（電子カルテ）
             03  PTMODRES-KARTE-UID         PIC X(36).
      *      UID（オルカ）
             03  PTMODRES-ORCA-UID          PIC X(36).
      *      継続区分
             03  PTMODRES-CONT-MODE         PIC X(05).
      *      実施日
             03  PTMODRES-INFORMATION-DATE  PIC X(10).
      *      実施時間
             03  PTMODRES-INFORMATION-TIME  PIC X(8).
      *      返却コード
             03  PTMODRES-API-RESULT        PIC X(3).
      *      返却メッセージ
             03  PTMODRES-API-RESULT-MSG    PIC X(100).
      *
             03  PTMODRES-RESKEY              PIC X(100).
      *      患者番号
             03  PTMODRES-PATIENTID        PIC X(20).
      *      患者情報
             03  PTMODRES-PTINF-INFO.
      *        患者氏名
               05  PTMODRES-NAME             PIC X(100).
      *        患者カナ氏名
               05  PTMODRES-KANANAME         PIC X(100).
      *        生年月日
               05  PTMODRES-BIRTHDAY         PIC X(10).
      *        性別
               05  PTMODRES-SEX              PIC X(01).
      *
      *      介護保険情報
             03  PTMODRES-CARE-INFO-G.
      *        介護保険情報
               05  PTMODRES-CARE-INSURANCE-G     OCCURS   10.
      *            保険者番号
                   07  PTMODRES-CARE-INSURANCE-NUMBER  PIC X(06).
      *            被保険者番号
                   07  PTMODRES-CARE-POLICYHOLDER      PIC X(10).
      *            有効開始日
                   07  PTMODRES-CARE-START-DATE        PIC X(10).
      *            有効終了日
                   07  PTMODRES-CARE-END-DATE          PIC X(10).
      *            エラーコード
                   07  PTMODRES-CARE-ERRCD             PIC X(04).
                   07  PTMODRES-CARE-ERRCD-MSG         PIC X(100).
      *
      *      要介護認定情報
             03  PTMODRES-NEED-INFO-G.
      *        要介護認定情報
               05  PTMODRES-CERTIFICATION-G    OCCURS   50.
      *            要介護状態区分
                   07  PTMODRES-NEED-CARE-STATE-CODE   PIC X(02).
      *            要介護状態名称
                   07  PTMODRES-NEED-CARE-STATE        PIC X(20).
      *            認定日付
                   07  PTMODRES-NEED-DATE              PIC X(10).
      *            有効開始日
                   07  PTMODRES-NEED-START-DATE        PIC X(10).
      *            有効終了日
                   07  PTMODRES-NEED-END-DATE          PIC X(10).
      *            エラーコード
                   07  PTMODRES-NEED-ERRCD             PIC X(04).
                   07  PTMODRES-NEED-ERRCD-MSG         PIC X(100).
      *
      *      エラーメッセージ
             03  PTMODRES-PATIENT-ERRMSG-G.
                 05  PTMODRES-PATIENT-ERRMSG-INF  OCCURS   50.
                     07  PTMODRES-ERROR        PIC X(04).
                     07  PTMODRES-ERROR-MSG    PIC X(100).
      *
