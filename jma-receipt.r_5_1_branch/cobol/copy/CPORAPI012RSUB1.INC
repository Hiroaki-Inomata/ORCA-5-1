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
      ******************************************************************
      *    患者情報基本情報編集サブ
      *                                    COPY (CPORAPI012RSUB1.INC)
      ******************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 05.00.00     NACL-多々納  18/10/XX  患者個別情報から禁忌薬剤追加
      ************************************************************
       01  ORAPI012RSUB1AREA.
      *
      *    患者ＩＤ
           03  API12SUB-PTID               PIC 9(10).
      *
           03  API12SUB-PATIENT-INFOMATION.
      *        地域連携ID
               05  API12SUB-COMMUNITY-CID        PIC  X(20).
      *        同意フラグ
               05  API12SUB-COMMUNITY-CID-AGREE  PIC  X(05).
      *
      *        初回来院日
               05  API12SUB-FIRST-DATE           PIC  X(10).
      *        最終来院日（退院日）
               05  API12SUB-LAST-DATE            PIC  X(10).
      *        入院中区分（１：入院中）
               05  API12SUB-OUTPATIENT-CLASS     PIC  X(01).
      *        入院日（入院中のみ）
               05  API12SUB-ADMISSION-DATE       PIC  X(10).
      *        退院日（当日退院時のみ）
               05  API12SUB-DISCHARGE-DATE       PIC  X(10).
      *    介護情報
           03  API12SUB-CARE-INF-G.
      *        介護保険情報
               05  API12SUB-CARE-INSURANCE-G    OCCURS   10.
      *            保険者番号
                   07  API12SUB-CARE-INSURANCE-NUMBER  PIC X(06).
      *            被保険者番号
                   07  API12SUB-CARE-POLICYHOLDER      PIC X(10).
      *            有効開始日
                   07  API12SUB-CARE-START-DATE        PIC X(10).
      *            有効終了日
                   07  API12SUB-CARE-END-DATE          PIC X(10).
      *            要介護認定情報
               05  API12SUB-CERTIFICATION-G    OCCURS   50.
      *            要介護状態区分
                   07  API12SUB-NEED-CARE-STATE-CODE   PIC X(02).
      *            要介護状態名称
                   07  API12SUB-NEED-CARE-STATE        PIC X(20).
      *            認定日付
                   07  API12SUB-NEED-DATE              PIC X(10).
      *            有効開始日
                   07  API12SUB-NEED-START-DATE        PIC X(10).
      *            有効終了日
                   07  API12SUB-NEED-END-DATE          PIC X(10).
      *
      *    地域包括診療対象疾病
           03  API12SUB-COMMUNITY-DISEASE.
               05  API12SUB-TARGET-DISEASE     PIC X(05)
                                                   OCCURS   4.
      *
      *    患者個人番号情報
           03  API12SUB-PTMYNUMBER-G.
               05  API12SUB-PTMYNUMBER-TBL     OCCURS   20.
      *            ＩＤ識別
                   07  API12SUB-ID-KEY           PIC X(10).
      *            ＩＤ番号
                   07  API12SUB-MY-NUMBER        PIC X(20).
      *            備考
                   07  API12SUB-DESCRIPTION      PIC X(100).
      *
      *H30.10
      *    患者個別情報
           03  API12SUB-PTCONF-INFO.
      *        妊婦設定
               05  API12SUB-NINPUKBN             PIC X(05).
      *        認知症地域包括診療
               05  API12SUB-NINTIHOKATU          PIC X(05).
      *        小児かかりつけ診療
               05  API12SUB-SYOUNIKAKARI         PIC X(05).
      *
      *    管理料等自動算定
           03  API12SUB-AUTO-SRYCD-G.
               05  API12SUB-AUTO-SRYCD-TBL     OCCURS   3.
                   07  API12SUB-AUTO-SRYCD       PIC X(09).
                   07  API12SUB-AUTO-NAME        PIC X(80).
                   07  API12SUB-AUTO-ENDYMD      PIC X(10).
      *
      *    患者禁忌薬剤
           03  API12SUB-KINKI-YKZAI-G.
               05  API12SUB-KINKI-YKZAI-TBL    OCCURS   100.
      *            禁忌薬剤コード
                   07  API12SUB-DRUGCD-CODE       PIC X(09).
                   07  API12SUB-DRUGCD-NAME       PIC X(80).
      *            有効終了日（マスタ終了日）
                   07  API12SUB-DRUGCD-END-DATE   PIC X(10).
      *            禁忌開始日
                   07  API12SUB-START-DATE        PIC X(10).
      *
