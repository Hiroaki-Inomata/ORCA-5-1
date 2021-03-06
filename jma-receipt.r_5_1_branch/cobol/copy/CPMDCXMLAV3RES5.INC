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
      *    ���Уɥ쥹�ݥ�����Ĵ��������ѹ����� (���β�)
      *                                      COPY (CPMDCXMLAV3RE5.INC)
      *         ���Ź԰����ơ�������
      ******************************************************************
       01  XML-MEDICALAV3RES5.
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
      *
      *        ���Ծ���
               05  MDCRES-PATIENTID        PIC X(20).
               05  MDCRES-NAME             PIC X(100).
               05  MDCRES-KANANAME         PIC X(100).
               05  MDCRES-BIRTHDAY         PIC X(10).
               05  MDCRES-SEX              PIC X(1).
      *        ����ǯ��
               05  MDCRES-PERFORM-YM       PIC X(07).
      *
      *�ꥯ�����ȡᣰ��
      *  �����޾���
               05  MDCRES-MEDICAL-INF-G.
                 07  MDCRES-MDC-INFO          OCCURS   100.
      *             ���Ų�
                    09  MDCRES-MDC-DEP-CODE             PIC X(2).
      *             ���Ų�̾��
                    09  MDCRES-MDC-DEP-CODE-NAME        PIC X(20).
      *             �ݸ��ȹ礻�ֹ�
                    09  MDCRES-MDC-COMBINATION-NUMBER   PIC X(04).
      *             �ݸ��μ���̾��
                    09  MDCRES-MDC-MAIN-INS-NAME        PIC X(20).
                    09  MDCRES-MDC-PUBLIC-INS-INFO.
      *             ����μ���̾��
                        11  MDCRES-MDC-INSURANCE-NAME  PIC X(20)
                                                     OCCURS  4 TIMES.
      *             �ݸ��ȹ礻�ֹ����
                    09  MDCRES-MDC-COMBINATION-MODE   PIC X(01).
      *             ���ֹ�
      ****          09  MDCRES-MDC-ZAINUM            PIC X(08).
      *
      *             ���Ŷ�ʬ
                    09  MDCRES-MDC-CLASS-KBN         PIC X(2).
      *             ���ż��̶�ʬ
                    09  MDCRES-MDC-CLASS             PIC X(3).
      *             ���ż���̾��
                    09  MDCRES-MDC-CLASS-NAME        PIC X(40).
      *             ��������β��
                    09  MDCRES-MDC-DAY-G.
      *                 ��������
                        11  MDCRES-MDC-RRK-G.
      *                 ���
                            13  MDCRES-MDC-CLASS-NUMBER  PIC X(03)
                                                     OCCURS   31.
      *             Ʊ���������λ���
                    09  MDCRES-MDC-REDAY-G.
                        11  MDCRES-MDC-REDAY-OCC    OCCURS   31.
      *                     Ʊ����������
                            13  MDCRES-MDC-REDAY        PIC X(02).
      *                     ���
                            13  MDCRES-MDC-RECLASS-NUMBER  PIC X(03).
      *             ������
                    09  MDCRES-MDC-POINT             PIC X(08).
      *
      *             ����ʬ��True�����ޡ�
                    09  MDCRES-INCLUSION-CLASS       PIC X(05).
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
      *                 ��ư��ʬ
                        11  MDCRES-PRSCRPT-AUTOKBN     PIC X(01).
      *
      *        ����Ĵ�����ֵ�
               05  MDCRES-MEDICAL-CHOZAIINF-G.
                   07  MDCRES-MDC-CHOZA-INF        OCCURS   10.
      *             ���Ų�
                    09  MDCRES-CHZ-DEP-CODE        PIC X(2).
      *             ���Ų�
                    09  MDCRES-CHZ-DEP-CODE-NAME   PIC X(20).
      *             �ݸ��ȹ礻�ֹ�
                    09  MDCRES-CHZ-COMBINATION     PIC X(4).
      *             �ݸ��μ���̾��
                    09  MDCRES-CHZ-MAIN-INS-NAME   PIC X(20).
                    09  MDCRES-CHZ-PUBLIC-INS-INFO.
      *             ����μ���̾��
                        11  MDCRES-CHZ-INSURANCE-NAME  PIC X(20)
                                                     OCCURS  4 TIMES.
      *             �ݸ��ȹ礻�ֹ����
                    09  MDCRES-CHZ-COMBINATION-MODE   PIC X(01).
      *
      *             ���ֹ�
                    09  MDCRES-CHZ-ZAINUM            PIC X(8).
      *
      *             ���Ŷ�ʬ
                    09  MDCRES-CHZ-CLASS-KBN         PIC X(2).
      *             ���ż��̶�ʬ
                    09  MDCRES-CHZ-CLASS             PIC X(3).
      *             ���ż���̾��
                    09  MDCRES-CHZ-CLASS-NAME        PIC X(40).
      *             ������
                    09  MDCRES-CHZ-DAY-G.
      *                 ��������
                        11  MDCRES-CHZ-RRK-G.
      *                 ���
                            13  MDCRES-CHZ-CLASS-NUMBER  PIC X(03)
                                                     OCCURS   31.
      *             Ʊ���������λ���
                    09  MDCRES-CHZ-REDAY-G.
                        11  MDCRES-CHZ-REDAY-OCC    OCCURS   31.
      *                     Ʊ����������
                            13  MDCRES-CHZ-REDAY        PIC X(02).
      *                     ���
                            13  MDCRES-CHZ-RECLASS-NUMBER  PIC X(03).
      *
      *             ������
                    09  MDCRES-CHZ-POINT             PIC X(08).
      *             ����ʬ��True�����ޡ�
                    09  MDCRES-CHZ-INCLUSION-CLASS   PIC X(05).
      *
                    09  MDCRES-CHZ-INFO-G.
                        11  MDCRES-CHZ-INFO    OCCURS   5.
      *                 ���ť�����
                        13  MDCRES-CHZ-CODE        PIC X(9).
      *                 ����̾��
                        13  MDCRES-CHZ-NAME        PIC X(140).
      *                 ��ư��ʬ
                        13  MDCRES-CHZ-AUTOKBN     PIC X(01).
      *
      *
      *        ��å������ΰ�
               05  MDCRES-MEDICAL-MSG1-G.
      *           ���顼������
                  07  MDCRES-MEDI-RESULT         PIC X(04).
      *           ���顼��å�����
                  07  MDCRES-MEDI-RESULT-MSG.
                      09  MDCRES-MEDI-RESULT-MSG1        PIC X(100).
                      09  MDCRES-MEDI-RESULT-MSG2        PIC X(100).
      *           ��˥󥰥�å�����
                  07  MDCRES-MEDI-WARNING-TBL    OCCURS  40.
      *               ��˥󥰥�����
                      09  MDCRES-WARNING             PIC X(04).
      *               ��˥󥰥�å�����
                      09  MDCRES-WARNING-MSG         PIC X(100).
      *
