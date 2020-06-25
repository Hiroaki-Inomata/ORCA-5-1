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
      ******************************************************************
      *    ���Уɥ쥹�ݥ��象��������������象���ա�(xml2)
      *                                   COPY (CPVISITPTLST01V2RES.INC)
      ******************************************************************
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
        01  XML-VISITPTLST01RES.
            02  VIS01RES-CONTEXT           PIC S9(9)   BINARY.
            02  VIS01RES-OBJECT            PIC X(36).
            02  VIS01RES-MODE              PIC S9(9)   BINARY.
            02  VIS01RES-RECORDNAME        PIC X(128).
            02  VIS01RES-MEDICALRES.
      *       �»���
              03  VIS01RES-INFORMATION-DATE    PIC X(10).
      *       �»ܻ���
              03  VIS01RES-INFORMATION-TIME    PIC X(8).
      *       �ֵѥ�����
              03  VIS01RES-API-RESULT          PIC X(2).
      *       �ֵѥ�å�����
              03  VIS01RES-API-RESULT-MSG      PIC X(100).
      *
              03  VIS01RES-RESKEY              PIC X(100).
      *
      *       �象����
              03  VIS01RES-VISIT-DATE          PIC X(10).
      *       �����������
              03  VIS01RES-MEDICAL-INF     OCCURS   1000.
      *           ���Ծ���
                  05  VIS01RES-PATIENT-INFOMATION.
      *               �����ֹ�
                      07  VIS01RES-PATIENTID         PIC X(20).
      *               ���Ի�̾
                      07  VIS01RES-NAME              PIC X(100).
      *               ���ԥ��ʻ�̾
                      07  VIS01RES-KANANAME          PIC X(100).
      *               ������
                      07  VIS01RES-BIRTHDAY          PIC X(10).
      *               ����
                      07  VIS01RES-SEX               PIC X(1).
      *           ���Ų�
                  05  VIS01RES-DEP-CODE          PIC X(02).
      *           ���Ų�û��̾��
                  05  VIS01RES-DEP-NAME          PIC X(20).
      *           �ɥ�����������
                  05  VIS01RES-DR-CODE           PIC X(5).
      *           �ɥ�������̾
                  05  VIS01RES-DR-NAME           PIC X(80).
      *           ��ɼ�ֹ�
                  05  VIS01RES-DENPNUM           PIC X(07).
      *           Ϣ��
                  05  VIS01RES-NUMBER            PIC X(01).
      *           �ݸ��ȹ礻�ֹ�
                  05  VIS01RES-COMBINATION-NUMBER  PIC X(04).
      *           �ݸ��ȹ礻����
                  05  VIS01RES-INSURANCE-INFO.
      *             �ݸ��μ���
                    07  VIS01RES-MAIN-INSURANCE-CLASS   PIC X(03).
      *             �ݸ��μ���̾��
                    07  VIS01RES-MAIN-INSURANCE-NAME    PIC X(20).
      *             �ݸ����ֹ�
                    07  VIS01RES-MAIN-INSURANCE-NUMBER  PIC X(08).
      *             ����
                    07  VIS01RES-MAIN-MARK              PIC X(80).
      *             �ֹ�
                    07  VIS01RES-MAIN-NUMBER            PIC X(80).
      *             �������
                    07  VIS01RES-PUBLIC-INSURANCE-INFO
                                              OCCURS  4 TIMES.
      *               ����μ���
                      09  VIS01RES-INSURANCE-CLASS         PIC X(03).
      *               ����μ���̾��
                      09  VIS01RES-INSURANCE-NAME          PIC X(20).
      *               ��ô���ֹ�
                      09  VIS01RES-PROVIDER-NUMBER         PIC X(08).
      *               ������ֹ�
                      09  VIS01RES-RECIPIENT-NUMBER        PIC X(20).
      *           ������
                  05  VIS01RES-UPDATE-DATE         PIC X(10).
      *           ��������
                  05  VIS01RES-UPDATE-TIME         PIC X(08).
      *           ���Ծ��󡡹�����
                  05  VIS01RES-PTNUM-UPDATE        PIC X(10).
      *           ���Ծ��󡡹�������
                  05  VIS01RES-PTNUM-UPTIME        PIC X(08).
      *