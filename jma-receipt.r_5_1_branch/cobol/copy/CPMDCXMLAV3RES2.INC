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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
      ******************************************************************
      *    ���Уɥ쥹�ݥ��������Ź԰٥����å����� (���β�)
      *                                      COPY (CPMDCXMLAV3RE2.INC)
      *         ���Ź԰�����
      ******************************************************************
       01  XML-MEDICALAV3RES2.
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
      *        �����ֵѤ���
               05  MDCRES-MEDICAL-SELECT-FLAG   PIC X(05).
      *        ��ɼ�ֹ�
      ***      05  MDCRES-INVOICD-NUMBER    PIC  X(07).
      *        ������
               05  MDCRES-NYUIN-DATE        PIC  X(10).
      *        �ౡ��
               05  MDCRES-TAIIN-DATE        PIC  X(10).
      *        Ʊ��������ʬ
               05  MDCRES-DOUJITSUKBN       PIC X(1).
      *        �������Ųʡ��ݸ��ȹ礻�ֹ�
               05  MDCRES-MOD-DEP-CODE       PIC X(2).
               05  MDCRES-MOD-COMBINATION    PIC X(4).
      *
      *        ���Ų�
               05  MDCRES-APPOINT-DEP-CODE  PIC X(2).
               05  MDCRES-APPOINT-DEP-NAME  PIC X(80).
      *        �ɥ�����
      *        05  MDCRES-APPOINT-DR-CODE   PIC X(5).
      **       05  MDCRES-APPOINT-DR-NAME   PIC X(80).
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
      *               11  MDCRES-RATE-OUTPATIENT         PIC X(04).
      *               ����ݸ����
      *               11  MDCRES-MONEY-OUTPATIENT        PIC X(06).
      *       ���������ֵ�
              05  MDCRES-REQ-MEDICAL-INF-G.
      *         ���ֳ���ʬ
                07  MDCRES-REQ-OFFTIME               PIC X(01).
      *         ���ž���
                07  MDCRES-REQ-INFO-G.
                  09  MDCRES-REQ-INFO          OCCURS   110.
      *             ���ż��̶�ʬ
                    11  MDCRES-REQ-CLASS             PIC X(3).
      *             ���ż���̾��
                    11  MDCRES-REQ-CLASS-NAME        PIC X(40).
      *             ���
                    11  MDCRES-REQ-CLASS-NUMBER      PIC X(03).
      *             ������
                    11  MDCRES-REQ-DAY-G.
                        13  MDCRES-REQ-DAY           PIC X(01)
                                                     OCCURS   31.
      *
                    11  MDCRES-REQ-PRSCRRT-INFO    OCCURS   50.
      *                 ���ť�����
                        13  MDCRES-REQ-PRSCRPT-CODE          PIC X(9).
      *                 ����̾��
                        13  MDCRES-REQ-PRSCRPT-NAME          PIC X(140).
      *                 ����
                        13  MDCRES-REQ-PRSCRPT-NUMBER        PIC X(11).
      *                 ������
                        13  MDCRES-REQ-PRSCRPT-MONEY         PIC X(07).
      *                 ��������������
                        13  MDCRES-REQ-PRSCRPT-ATAI-G.
                            15  MDCRES-REQ-PRSCRPT-ATAI      PIC X(10)
                                                         OCCURS   5.
      *                 �ե����ʬ���
                        13  MDCRES-REQ-PRSCRPT-FILMNUM      PIC X(03).
      *                 ��³�ؼ���ʬ
                        13  MDCRES-REQ-PRSCRPT-CONTKBN       PIC X(01).
      *                 ����������ؼ���ʬ
                        13  MDCRES-REQ-PRSCRPT-NAIFKBN       PIC X(01).
      *                 ��ư����ʤ���ʬ�ʺ������ܼ��Ӳû��ʤɡ�
                        13  MDCRES-REQ-PRSCRPT-NOAUTO        PIC X(03).
      *
      *                 ��ư��ʬ
                        13  MDCRES-REQ-PRSCRPT-AUTOKBN       PIC X(01).
      *       �������ƥ��顼
              05  MDCRES-PRSCRPT-ERRMSG-G.
                  07  MDCRES-PRSCRPT-ERRMSG-TBL    OCCURS   100.
      *               ���顼������
                      09  MDCRES-PRSC-RESULT         PIC X(3).
      *               ���顼��å�����
                      09  MDCRES-PRSC-RESULT-MSG     PIC X(100).
      *               ���顼����
                      09  MDCRES-PRSC-POSITION       PIC X(03).
      *               ���顼���ֹ�
                      09  MDCRES-PRSC-ITEM-POSITION  PIC X(02).
      *
      *       ���������ֵ�
              05  MDCRES-MEDICAL-INF-G.
                  07  MDCRES-MDC-INFO          OCCURS   120.
      *             ���Ŷ�ʬ
                    09  MDCRES-MDC-CLASS-KBN         PIC X(2).
      *             ���ż��̶�ʬ
                    09  MDCRES-MDC-CLASS             PIC X(3).
      *             ���ż���̾��
                    09  MDCRES-MDC-CLASS-NAME        PIC X(40).
      *             ���
                    09  MDCRES-MDC-CLASS-NUMBER      PIC X(03).
      *             ������
                    09  MDCRES-MDC-DAY-G.
                        11  MDCRES-MDC-DAY           PIC X(01)
                                                     OCCURS   31.
      *
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
                    09  MDCRES-PRSCRPT-INFO-G.
                      10  MDCRES-PRSCRPT-INFO    OCCURS   50.
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
      *
      *                 ��³�ؼ���ʬ
                        11  MDCRES-PRSCRPT-CONTKBN       PIC X(01).
      *                 ����������ؼ���ʬ
                        11  MDCRES-PRSCRPT-NAIFKBN       PIC X(01).
      *                 ��ư����ʤ���ʬ�ʺ������ܼ��Ӳû���
                        11  MDCRES-PRSCRPT-NOAUTO        PIC X(03).
      *
      *                 ��ư��ʬ
                        11  MDCRES-PRSCRPT-AUTOKBN       PIC X(01).
      *
      *        �������ƥ��顼
               05  MDCRES-MEDICAL-MSG1-G.
      *           ���顼������
                  07  MDCRES-MEDI-RESULT         PIC X(04).
      *           ���顼��å�����
                  07  MDCRES-MEDI-RESULT-MSG.
                      09  MDCRES-MEDI-RESULT-MSG1    PIC X(100).
                      09  MDCRES-MEDI-RESULT-MSG2    PIC X(100).
      *           ���顼����
                  07  MDCRES-MEDI-POSITION       PIC X(03).
      *           ���顼���ֹ�
                  07  MDCRES-MEDI-ITEM-POSITION  PIC X(02).
      *           ���顼������
                  07  MDCRES-MEDI-CODE           PIC X(09).
      *           ��˥󥰥�å�����
                  07  MDCRES-MEDI-WARNING-TBL    OCCURS  100.
      *               ��˥󥰥�����
                      09  MDCRES-MEDI-WARNING        PIC X(04).
      *               ��˥󥰥�å�����
                      09  MDCRES-WARNING-MSG.
                          11  MDCRES-WARNING-MSG1        PIC X(100).
                          11  MDCRES-WARNING-MSG2        PIC X(100).
      *               ��˥󥰺���
                      09  MDCRES-WRG-POSITION        PIC X(03).
      *               ��˥󥰹��ֹ�
                      09  MDCRES-WRG-RITEM-POSITION  PIC X(02).
      *               ��˥󥰥�����
                      09  MDCRES-WRG-MEDI-CODE       PIC X(09).
      *
      *       ��ǧ��å����������ɡ����������ɡ�
              05  MDCRES-SELECT-CODE         PIC X(04).
      *       ��ǧ��å���������
              05  MDCRES-SELECT-MSG          PIC X(100).
      *