      *******************************************************************
      * Project code name "ORCA"
      * ����ɸ��쥻�ץȥ��եȡ�JMA standard receipt software��
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
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      * 05.00.00     NACL-¿��Ǽ  19/01/XX  ʣ�����ݸ��б�
      *****************************************************************
      ******************************************************************
      *    ���Уɥ쥹�ݥ󥹿��Ź԰���Ͽ���� (���β�)
      *                                      COPY (CPMDCXMLV3RE3.INC)
      *         ���Ź԰�����
      ******************************************************************
       01  XML-MEDICALV3RES3.
            03  MDCRES-CONTEXT             PIC S9(9)   BINARY.
            03  MDCRES-OBJECT              PIC X(36).
            03  MDCRES-MODE                PIC S9(9)   BINARY.
            03  MDCRES-RECORDNAME          PIC X(128).
            03  MDCRES-MEDICALRES.
      *        �ꥯ�������ֹ�
               05  MDCRES-REQUEST-NUMBER    PIC X(2).
      *        �쥹�ݥ��ֹ�
               05  MDCRES-RESPONSE-NUMBER   PIC X(2).
      *        UID���Żҥ���ơ�
               05  MDCRES-KARTE-UID         PIC X(36).
      *        UID�ʥ��륫��
               05  MDCRES-ORCA-UID          PIC X(36).
      *        �»���
               05  MDCRES-INFORMATION-DATE  PIC X(10).
      *        �»ܻ���
               05  MDCRES-INFORMATION-TIME  PIC X(8).
      *        �ֵѥ�����
               05  MDCRES-API-RESULT        PIC X(3).
      *        �ֵѥ�å�����
               05  MDCRES-API-RESULT-MSG    PIC X(100).
      *
               05  MDCRES-RESKEY            PIC X(100).
      *        ��������
               05  MDCRES-PERFORM-DATE      PIC X(10).
               05  MDCRES-PERFORM-TIME      PIC X(8).
      *        ������ա���ɼȯ������
               05  MDCRES-BASE-DATE         PIC X(10).
      *        ���Ų�
               05  MDCRES-APPOINT-DEP-CODE  PIC X(2).
               05  MDCRES-APPOINT-DEP-NAME  PIC X(80).
      *        �ɥ�����
               05  MDCRES-APPOINT-DR-CODE   PIC X(5).
               05  MDCRES-APPOINT-DR-NAME   PIC X(80).
      *        ���Ծ���
               05  MDCRES-PATIENTID        PIC X(20).
               05  MDCRES-NAME             PIC X(100).
               05  MDCRES-KANANAME         PIC X(100).
               05  MDCRES-BIRTHDAY         PIC X(10).
               05  MDCRES-SEX              PIC X(1).
      *        �ݸ�����
               05  MDCRES-INSURANCE-G.
      *            �ݸ��ȹ礻�ֹ�
                   09  MDCRES-COMBINATION-NUMBER     PIC X(04).
      *            �ݸ��ȹ礻��ô���
                   09  MDCRES-COMBINATION-RATE       PIC X(03).
      *            �ݸ��μ���
                   09  MDCRES-MAIN-INSURANCE-CLASS   PIC X(03).
      *            �ݸ����ֹ�
                   09  MDCRES-MAIN-INSURANCE-NUMBER  PIC X(08).
      *            �ݸ��μ���̾��
                   09  MDCRES-MAIN-INSURANCE-NAME    PIC X(20).
      *            ����
                   09  MDCRES-MAIN-MARK              PIC X(80).
      *            �ֹ�
                   09  MDCRES-MAIN-NUMBER            PIC X(80).
      *            ��³��ʬ
                   09  MDCRES-MAIN-CONTINUATION      PIC X(01).
      *            �����ʬ
                   09  MDCRES-MAIN-ASSISTANCE        PIC X(01).
      *            �����ʬ̾��
                   09  MDCRES-MAIN-ASSI-NAME         PIC X(20).
      *            �ܿͲ�²��ʬ
                   09  MDCRES-MAIN-FAMILY            PIC X(01).
      *            �������
                   09  MDCRES-PUBLIC-INSURANCE-INFO           
                                              OCCURS  4 TIMES.
      *               ����μ���
                      11  MDCRES-INSURANCE-CLASS         PIC X(03).
      *               ����μ���̾��
                      11  MDCRES-INSURANCE-NAME          PIC X(20).
      *               ��ô���ֹ�
                      11  MDCRES-PROVIDER-NUMBER         PIC X(08).
      *               ������ֹ�
                      11  MDCRES-RECIPIENT-NUMBER        PIC X(20).
      *               �������ôΨ�ʳ��
                      11  MDCRES-RATE-OUTPATIENT         PIC X(04).
      *               ����ݸ����
                      11  MDCRES-MONEY-OUTPATIENT        PIC X(06).
      *
      *        ���������ֵ�
               05  MDCRES-MEDICAL-INF-G.
                 07  MDCRES-MDC-INFO          OCCURS   120.
      *!!!
      *2019.1
      *             �ݸ������Ų�����
                    09  MDCRES-CHENGE-REC.
      *                 �ǡ�����ʬ��DEP��ʣ�����Ųʡ�COM:�ݸ��ȹ礻��
                        11  MDCRES-MDC-DATA-KBN            PIC X(03).
      *                 �ݸ��ȹ礻�ֹ�
                        11  MDCRES-MDC-COMBINATION-NUMBER  PIC X(04).
      *                 �ݸ��μ���̾��
                        11  MDCRES-MDC-MAIN-INS-NAME       PIC X(20).
                        11  MDCRES-MDC-PUBLIC-INS-INFO.
      *                 ����μ���̾��
                            13  MDCRES-MDC-INSURANCE-NAME  PIC X(20)
                                                     OCCURS  4 TIMES.
      *                 ���Ų�
                        11  MDCRES-MDC-DEP-CODE            PIC X(2).
                        11  MDCRES-MDC-DEP-CODE-NAME       PIC X(20).
      *                 �ɥ�����
                        11  MDCRES-MDC-DR-CODE             PIC X(5).
                        11  MDCRES-MDC-DR-CODE-NAME        PIC X(80).
      *!!!
      *             ���Ŷ�ʬ
                    09  MDCRES-MDC-CLASS-KBN         PIC X(2).
      *             ���ż��̶�ʬ
                    09  MDCRES-MDC-CLASS             PIC X(3).
      *             ���ż���̾��
                    09  MDCRES-MDC-CLASS-NAME        PIC X(40).
      *             ���
                    09  MDCRES-MDC-CLASS-NUMBER      PIC X(03).
      *             ������
                    09  MDCRES-MDC-POINT             PIC X(08).
      *             �޶��
                    09  MDCRES-MDC-MONEY             PIC X(08).
      *             ����ʬ��True�����ޡ�
                    09  MDCRES-INCLUSION-CLASS       PIC X(05).
      *
      *             ��縡���θ�����
                    09  MDCRES-HKTKNSA-CNT           PIC X(02).
      *
      *             �޺����ǽϢ��
                    09  MDCRES-DEL-NUMBER            PIC X(02).
      *
      *             �޺����ʬ
                    09  MDCRES-DEL-INF               PIC X(06).
      *
                    09  MDCRES-PRSCRPT-INFO-G.
                      10  MDCRES-PRESCRIPTION-INFO    OCCURS   50.
      *                 ���ť�����
                        11  MDCRES-PRSCRPT-CODE          PIC X(9).
      *                 ����̾��
                        11  MDCRES-PRSCRPT-NAME          PIC X(140).
      *                 ����
                        11  MDCRES-PRSCRPT-NUMBER        PIC X(11).
      *                 ñ�̥�����
                        11  MDCRES-PRSCRPT-TANICD        PIC X(03).
      *                 ñ��̾��
                        11  MDCRES-PRSCRPT-TANICD-NAME   PIC X(24).
      *                 ������
                        11  MDCRES-PRSCRPT-MONEY         PIC X(07).
      *                 ��������������
                        11  MDCRES-PRSCRPT-ATAI-G.
                            13  MDCRES-PRSCRPT-ATAI      PIC X(10)
                                                         OCCURS   5.
      *                 �ե����ʬ���
                        11  MDCRES-PRSCRPT-FILMNUM       PIC X(03).
      *                 ����̾���ܶ�ʬ�ʣ�������̾����������̾�ʰ���̾��
                        11  MDCRES-PRSCRPT-GENERIC       PIC X(01).
      *                 ����̾̾��
                        11  MDCRES-PRSCRPT-GENNAME       PIC X(200).
      *                 ��³�ؼ���ʬ
                        11  MDCRES-PRSCRPT-CONTKBN       PIC X(01).
      *                 ����������ؼ���ʬ
                        11  MDCRES-PRSCRPT-NAIFKBN       PIC X(01).
      *                 ��ư����ʤ���ʬ�ʺ������ܼ��Ӳû���
                        11  MDCRES-PRSCRPT-NOAUTO        PIC X(03).
      *                 ��ư��ʬ
                        11  MDCRES-PRSCRPT-AUTOKBN       PIC X(01).
      *
      *         �������ƥ��顼
                05  MDCRES-MEDICAL-MSG1-G.
      *           ���顼������
                  07  MDCRES-MEDI-RESULT         PIC X(04).
      *           ���顼��å�����
                  07  MDCRES-MEDI-RESULT-MSG     PIC X(100).
      *
      *        ��ɼ�ֹ�ʼ礿���
               05  MDCRES-INVOICD-NUMBER     PIC  X(07).
      *
      *2019.1
      *        ������Ǽ�����ֵ�
               05  MDCRES-IN-CD-INF.
                   07  MDCRES-IN-CD-CHNGE-INF      OCCURS   15.
      *                �ݸ��ȹ礻�ֹ�
                       09  MDCRES-IN-CD-COMB-NUMBER    PIC X(04).
      *                ���Ų�
                       09  MDCRES-IN-CD-DEP-CODE       PIC X(02).
      *
      *                ��Ǽ�����
                       09  MDCRES-IN-CD-IC-MONEY       PIC  X(10).
      *                Ĵ����ۣ�
                       09  MDCRES-IN-CD-AD-MONEY1      PIC  X(10).
      *                Ĵ����ۣ�
                       09  MDCRES-IN-CD-AD-MONEY2      PIC  X(10).
      *            Ĵ����ۣ���ñ�ȡ�
                   07  MDCRES-IN-AD-MONEY1         PIC  X(10).
      *            Ĵ����ۣ���ñ�ȡ�
                   07  MDCRES-IN-AD-MONEY2         PIC  X(10).
      *            �����
                   07  MDCRES-IN-IC-MONEY          PIC  X(10).
      *            �ֶ��
                   07  MDCRES-IN-RE-MONEY          PIC  X(10).
      *
               05  MDCRES-IC-INF.
      *            ������ˡ��ʬ
                   07  MDCRES-IC-CODE           PIC  X(02).
                   07  MDCRES-IC-CODE-NAME      PIC  X(20).
      *!!
      *H29.7
      *            �����갷����ʬ
                   07  MDCRES-IC-REQUEST           PIC X(01).
                   07  MDCRES-IC-REQUEST-NAME      PIC X(40).
      *
      *            �����������  ��Ĵ���⡦�����ޤࡣ�����Ϻ��ۡ�
                   07  MDCRES-TOTAL-AC-MONEY      PIC  X(10).
      *
      *            ��������ۡʹ�ס�
                   07  MDCRES-TOTAL-IC-MONEY      PIC  X(10).
      *            �����ֶ�ۡʹ�ס�
                   07  MDCRES-TOTAL-RE-MONEY      PIC  X(10).
      *
      *            ��������۾�³ۡ������ֶ�ۡʿ����Τߡ�
                   07  MDCRES-MAX-IC-MONEY        PIC X(10).
      *
      *
      *        ��Ǽ�����ֵ�
               05  MDCRES-CD-INF.
      *!!!
      *2019.1
                   07  MDCRES-CD-CHNGE-INF.
      *                �ݸ��ȹ礻�ֹ�
                       09  MDCRES-CD-COMBINATION-NUMBER    PIC X(04).
                       09  MDCRES-CD-COMBINATION-NAME      PIC X(40).
      *                ���Ų�
                       09  MDCRES-CD-DEP-CODE              PIC X(02).
                       09  MDCRES-CD-DEP-CODE-NAME         PIC X(20).
      *!!!
      *            ����������
                   07  MDCRES-IC-MONEY          PIC  X(10).
      *
      *            Ĵ����ۣ�
                   07  MDCRES-AD-MONEY1         PIC  X(10).
      *            Ĵ����ۣ�
                   07  MDCRES-AD-MONEY2         PIC  X(10).
      *            �����ۡ�Ĵ�����ޤޤʤ���
                   07  MDCRES-AC-MONEY          PIC  X(10).
      *
      *            �����ֶ�ۡ��������ʳ��Ͻ�����
                   07  MDCRES-RE-MONEY          PIC  X(10).
      *
      *            ��Ǽ�����ֵѡ������������
                   07  MDCRES-SYUNOU-AREA.
      *            �ݸ�Ŭ�Ѷ��
                       09  MDCRES-AI-MONEY         PIC  X(10).
      *             ������
                       09  MDCRES-OE-MONEY         PIC  X(10).
      *             ���ް�����ô��
                       09  MDCRES-DG-SMONEY        PIC  X(10).
      *             Ϸ�Ͱ�����ô��
                       09  MDCRES-OM-SMONEY           PIC  X(10).
      *             ���������ô��
                       09  MDCRES-PI-SMONEY           PIC  X(10).
      *             ϫ�Ҷ�۹��
                       09  MDCRES-LSI-TOTALMONEY      PIC  X(10).
      *
      *         ������������
                    07  MDCRES-AC-POINT-INF.
      *             �ݸ��������
                        09  MDCRES-AC-TOTAL-POINT         PIC  X(10).
      *             �ݸ�����
                        09  MDCRES-AC-POINT-G.
                            11  MDCRES-AC-POINT-TBL      OCCURS   16.
                                13  MDCRES-AC-POINT-NAME   PIC  X(30).
      *                         �ݸ�����
                                13  MDCRES-AC-POINT        PIC  X(10).
      *                         Ŭ�ѳ����
                                13  MDCRES-AC-TGMONEY      PIC  X(10).
      *              ��������
                     07  MDCRES-JIHI-G.
                         09  MDCRES-JIHI-TBL      OCCURS   10.
                             11  MDCRES-JIHI-NAME       PIC  X(20).
      *                      ������
                             11  MDCRES-JIHI-MONEY      PIC  X(10).
      *                      �����ۡʾ����Ǥ����
                             11  MDCRES-JIHI-MONEY-TAX  PIC  X(10).
      *                  ��������ǡʺƷǡ�
                         09  MDCRES-JIHI-TAX    PIC X(10).
      *
      *            ��������Ǽ�����ֵ�
                   07  MDCRES-OLD-CD-INF.
      *                ����������
                       09  MDCRES-OLD-IC-MONEY         PIC  X(10).
      *                ����������
                       09  MDCRES-OLD-AC-MONEY         PIC  X(10).
      *                ����Ĵ����ۣ�
                       09  MDCRES-OLD-AD-MONEY1        PIC  X(10).
      *                ����Ĵ����ۣ�
                       09  MDCRES-OLD-AD-MONEY2        PIC  X(10).
      *
      *H29.6
      *        ���Ⱦ���
               05  MDCRES-DISCOUNT-INF.
                   07  MDCRES-DISCOUNT-NAME        PIC X(12).
               05  MDCRES-MAE-CD-INF.
      *            ����ޤǤ�̤����
                   07  MDCRES-MAE-MISYU             PIC X(10).
      *            ����ޤǤ�Ķ���
                   07  MDCRES-MAE-CHOUKA            PIC X(10).
      *
      *2019.1
      * ʣ�����ݸ�
      *        ��Ǽ�����ֵѡ����١�
               05  MDCRES-SUB-CD-INF.
                 06  MDCRES-SUB-CD-INF-OCC          OCCURS   15.
                   07  MDCRES-SUB-CHNGE-INF.
      *                �ݸ��ȹ礻�ֹ�
                       09  MDCRES-SUB-COMBINATION-NUMBER    PIC X(04).
      *                ���Ų�
                       09  MDCRES-SUB-DEP-CODE         PIC X(02).
                       09  MDCRES-SUB-DEP-CODE-NAME    PIC X(20).
      *                �ɥ�����
                       09  MDCRES-SUB-DR-CODE          PIC X(5).
      *                ��ɼ�ֹ�
                       09  MDCRES-SUB-INVOICD-NUMBER   PIC  X(07).
      *            ����������
                   07  MDCRES-SUB-IC-MONEY          PIC  X(10).
      *
      *            Ĵ����ۣ�
                   07  MDCRES-SUB-AD-MONEY1         PIC  X(10).
      *            Ĵ����ۣ�
                   07  MDCRES-SUB-AD-MONEY2         PIC  X(10).
      *            �����ۡ�Ĵ�����ޤޤʤ���
                   07  MDCRES-SUB-AC-MONEY          PIC  X(10).
      *
      *            �����ֶ�ۡ��������ʳ��Ͻ�����
                   07  MDCRES-SUB-RE-MONEY          PIC  X(10).
      *
      *            ��Ǽ�����ֵѡ������������
                   07  MDCRES-SUB-SYUNOU-AREA.
      *            �ݸ�Ŭ�Ѷ��
                       09  MDCRES-SUB-AI-MONEY         PIC  X(10).
      *             ������
                       09  MDCRES-SUB-OE-MONEY         PIC  X(10).
      *             ���ް�����ô��
                       09  MDCRES-SUB-DG-SMONEY        PIC  X(10).
      *             Ϸ�Ͱ�����ô��
                       09  MDCRES-SUB-OM-SMONEY        PIC  X(10).
      *             ���������ô��
                       09  MDCRES-SUB-PI-SMONEY        PIC  X(10).
      *             ϫ�Ҷ�۹��
                       09  MDCRES-SUB-LSI-TOTALMONEY   PIC  X(10).
      *
      *         ������������
                    07  MDCRES-SUB-AC-POINT-INF.
      *             �ݸ��������
                        09  MDCRES-SUB-AC-TOTAL-POINT      PIC  X(10).
      *             �ݸ�����
                        09  MDCRES-SUB-AC-POINT-G.
                            11  MDCRES-SUB-AC-POINT-TBL    OCCURS   16.
                                13  MDCRES-SUB-AC-POINT-NAME
                                                           PIC  X(30).
      *                         �ݸ�����
                                13  MDCRES-SUB-AC-POINT    PIC  X(10).
      *                         Ŭ�ѳ����
                                13  MDCRES-SUB-AC-TGMONEY  PIC  X(10).
      *              ��������
                     07  MDCRES-SUB-JIHI-G.
                         09  MDCRES-SUB-JIHI-TBL      OCCURS   10.
                             11  MDCRES-SUB-JIHI-NAME       PIC  X(20).
      *                      ������
                             11  MDCRES-SUB-JIHI-MONEY      PIC  X(10).
      *                      �����ۡʾ����Ǥ����
                             11  MDCRES-SUB-JIHI-MONEY-TAX  PIC  X(10).
      *                  ��������ǡʺƷǡ�
                         09  MDCRES-SUB-JIHI-TAX    PIC X(10).
      *
      *            ��������Ǽ�����ֵ�
                   07  MDCRES-SUB-OLD-CD-INF.
      *                ����������
                       09  MDCRES-SUB-OLD-IC-MONEY         PIC  X(10).
      *                ����������
                       09  MDCRES-SUB-OLD-AC-MONEY         PIC  X(10).
      *                ����Ĵ����ۣ�
                       09  MDCRES-SUB-OLD-AD-MONEY1        PIC  X(10).
      *                ����Ĵ����ۣ�
                       09  MDCRES-SUB-OLD-AD-MONEY2        PIC  X(10).
      *
      *            �ޤȤ����Ϻ��� �ʿ��Ųʡᣰ���ʳ���
                   07  MDCRES-SUB-GRP-SGKMONEY-INF.
                       09  MDCRES-SUB-GRP-SGKMONEY         PIC X(10).
      *!!!!
      *2019.1
              05  MDCRES-PRINT-INIT-AREA.
      *           �������μ��񡦿��������ٽ�ȯ�Զ�ʬ
                  07  MDCRES-HAKKOU-KBN        PIC X(01).
                  07  MDCRES-HAKKOU-KBN-MEI    PIC X(30).
      *H29.6
      *   Ģɼ�����ؼ���ʬ
              05  MDCRES-PRINT-AREA.
      *           �����������
                  07  MDCRES-PRINT-SHOHOU      PIC X(01).
                  07  MDCRES-PRINT-SHOHOU-MEI  PIC X(30).
      *           �������μ������
                  07  MDCRES-PRINT-SEIKYU      PIC X(01).
                  07  MDCRES-PRINT-SEIKYU-MEI  PIC X(30).
      *           ���������ٽ����
                  07  MDCRES-PRINT-MEISAI      PIC X(01).
                  07  MDCRES-PRINT-MEISAI-MEI  PIC X(30).
      *           ���޾������
                  07  MDCRES-PRINT-YAKUJYO     PIC X(01).
                  07  MDCRES-PRINT-YAKUJYO-MEI PIC X(30).
      *           ������Ģ����
                  07  MDCRES-PRINT-TECHOU      PIC X(01).
                  07  MDCRES-PRINT-TECHOU-MEI  PIC X(30).
      *           ͽ��ɼ����
                  07  MDCRES-PRINT-YOYAKU      PIC X(01).
                  07  MDCRES-PRINT-YOYAKU-MEI  PIC X(30).
      *           DATA-ID 2019/09/19 Added
              05  MDCRES-DATA-ID               PIC X(37).
      *