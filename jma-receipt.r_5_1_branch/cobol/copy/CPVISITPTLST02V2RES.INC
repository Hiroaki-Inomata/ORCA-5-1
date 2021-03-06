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
        01  XML-VISITPTLST02RES.
            02  VIS02RES-CONTEXT           PIC S9(9)   BINARY.
            02  VIS02RES-OBJECT            PIC X(36).
            02  VIS02RES-MODE              PIC S9(9)   BINARY.
            02  VIS02RES-RECORDNAME        PIC X(128).
            02  VIS02RES-VISITPTLSTRES.
      *       �»���
              03  VIS02RES-INFORMATION-DATE    PIC X(10).
      *       �»ܻ���
              03  VIS02RES-INFORMATION-TIME    PIC X(8).
      *       �ֵѥ�����
              03  VIS02RES-API-RESULT          PIC X(2).
      *       �ֵѥ�å�����
              03  VIS02RES-API-RESULT-MSG      PIC X(100).
      *
              03  VIS02RES-RESKEY              PIC X(100).
      *
      *       �象ǯ��
              03  VIS02RES-VISIT-DATE          PIC X(07).
      *       ���Ų�
              03  VIS02RES-DEP-CODE            PIC X(02).
      *       ���Ų�û��̾��
              03  VIS02RES-DEP-NAME            PIC X(20).
      *       �����������
              03  VIS02RES-MEDICAL-INF     OCCURS   2000.
      *           ���Ծ���
                  05  VIS02RES-PATIENT-INFOMATION.
      *               �����ֹ�
                      07  VIS02RES-PATIENTID         PIC X(20).
      *               ���Ի�̾
                      07  VIS02RES-NAME              PIC X(100).
      *               ���ԥ��ʻ�̾
                      07  VIS02RES-KANANAME          PIC X(100).
      *               ������
                      07  VIS02RES-BIRTHDAY          PIC X(10).
      *               ����
                      07  VIS02RES-SEX               PIC X(1).
      *           �象��
                  05  VIS02RES-VISIT-CALENDAR.
                      07  VIS02RES-VISIT-DAY       PIC X(01)
                                                   OCCURS   31.
      *
