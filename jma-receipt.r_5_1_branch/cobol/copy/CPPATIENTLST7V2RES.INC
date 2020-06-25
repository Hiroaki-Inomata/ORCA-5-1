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
      *    ＡＰＩレスポンスメモ内容取得(xml2)
      *                                   COPY (CPPATIENTLST7V2RES.INC)
      ******************************************************************
        01  XML-PATIENTLST7RES.
            02  PTRES-CONTEXT          PIC S9(9)   BINARY.
            02  PTRES-OBJECT           PIC X(36).
            02  PTRES-MODE             PIC S9(9)   BINARY.
            02  PTRES-RECORDNAME       PIC X(128).
            02  PTRES-PATIENTINFORES.
      *       実施日
              03  PTRES-INFORMATION-DATE   PIC X(10).
      *       実施時間
              03  PTRES-INFORMATION-TIME   PIC X(8).
      *       返却コード
              03  PTRES-API-RESULT         PIC X(3).
      *       返却メッセージ
              03  PTRES-API-RESULT-MSG     PIC X(100).
              03  PTRES-RESKEY             PIC X(100).
      *
              03  PTRES-PATIENT-INFOMATION.
      *           患者番号
                  05  PTRES-PATIENTID          PIC X(20).
      *           漢字氏名
                  05  PTRES-NAME               PIC X(100).
      *           カナ氏名
                  05  PTRES-KANANAME           PIC X(100).
      *           生年月日
                  05  PTRES-BIRTHDAY           PIC X(10).
      *           性別
                  05  PTRES-SEX                PIC X(1).
      *        基準日付
               03  PTRES-BASE-DATE        PIC X(10).
      *        メモ情報
               03  PTRES-MEMOINF-INFOMATION.
      *            メモ情報
                   05  PTRES-MEMO-INFO     OCCURS  10 TIMES.
      *                診療科
                       07  PTRES-SRYKA              PIC X(02).
      *                診療科名称
                       07  PTRES-SRYKA-MEI          PIC X(40).
      *                メモ内容
                       07  PTRES-MEMO               PIC X(1000).
      *                受付情報
                       07  PTRES-UKETUKE-INFO.
      *                    受付診療科（診療科＝００のみ）
                           09  PTRES-UKE-SRYKA          PIC X(02).
                           09  PTRES-UKE-SRYKA-MEI      PIC X(40).
      *                    保険情報
                           09  PTRES-INSURANCE-INFO.
      *                        保険組合せ番号
                               11  PTRES-COMBINATION-NUMBER  PIC X(04).
      *                        非表示区分
                               11  PTRES-COMBINATION-HYOJI   PIC X(01).
      *                        保険の種類
                               11  PTRES-MAIN-INSURANCE-CLASS
                                                             PIC X(03).
      *                        保険者番号
                               11  PTRES-MAIN-INSURANCE-NUMBER
                                                             PIC X(08).
      *                        保険の種類名称
                               11  PTRES-MAIN-INSURANCE-NAME
                                                             PIC X(20).
      *                        記号
                               11  PTRES-MAIN-MARK           PIC X(80).
      *                        番号
                               11  PTRES-MAIN-NUMBER         PIC X(80).
      *                        継続区分
                               11  PTRES-MAIN-CONTINUATION   PIC X(01).
      *                        補助区分
                               11  PTRES-MAIN-ASSISTANCE     PIC X(01).
      *                        本人家族区分
                               11  PTRES-MAIN-FAMILY         PIC X(01).
      *                        被保険者名
                               11  PTRES-MAIN-POLICYHOLDER   PIC X(100).
      *                        有効開始日
                               11  PTRES-MAIN-START-DATE     PIC X(10).
      *                        有効終了日
                               11  PTRES-MAIN-END-DATE       PIC X(10).
      *                        公費情報
                               11  PTRES-PUBLIC-INSURANCE-INFO
                                                  OCCURS  4 TIMES.
      *                            公費の種類
                                   13  PTRES-INSURANCE-CLASS
                                                             PIC X(03).
      *                            公費の種類名称
                                   13  PTRES-INSURANCE-NAME  PIC X(20).
      *                            負担者番号
                                   13  PTRES-PROVIDER-NUMBER
                                                             PIC X(08).
      *                            受給者番号
                                   13  PTRES-RECIPIENT-NUMBER
                                                             PIC X(20).
      *                            有効開始日
                                   13  PTRES-INSURANCE-START-DATE
                                                             PIC X(10).
      *                            有効終了日
                                   13  PTRES-INSURANCE-END-DATE
                                                             PIC X(10).
      *
