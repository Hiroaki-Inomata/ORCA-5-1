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
      ******************************************************************
      *    ���Уɥꥯ�����ȴ��Ծ�����������ʻ�̾������(xml2)
      *                                 COPY (CPPATIENTLST3V2SEQ.INC)
      ******************************************************************
        01  XML-PATIENTLST3REQ.
            03  PATLSTREQ-CONTEXT          PIC S9(9)   BINARY.
            03  PATLSTREQ-OBJECT           PIC X(36).
            03  PATLSTREQ-MODE             PIC S9(9)   BINARY.
            03  PATLSTREQ-RECORDNAME       PIC X(128).
      *     ���Ծ���
            03  PATLSTREQ-PTINF-INFO.
      *         ���Ի�̾
                05  PATLSTREQ-NAME             PIC X(100).
      *         ������ǯ����
                05  PATLSTREQ-BIRTHDAY-START   PIC X(10).
      *         ��λ��ǯ����
                05  PATLSTREQ-BIRTHDAY-END     PIC X(10).
      *         ����
                05  PATLSTREQ-SEX              PIC X(01).
      *         ������ʬ��0:���������衢1:������2�������
                05  PATLSTREQ-INOUTKBN         PIC X(01).
      *