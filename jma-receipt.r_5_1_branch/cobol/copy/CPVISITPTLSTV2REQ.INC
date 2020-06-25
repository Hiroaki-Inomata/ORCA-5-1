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
      *    ���Уɥꥯ�������象����������� (xml2)
      *                                        COPY (CPVISITPTLSTEQ.INC)
      ******************************************************************
        01  XML-VISITPTLSTREQ.
            02  VISREQ-CONTEXT           PIC S9(9)   BINARY.
            02  VISREQ-OBJECT            PIC X(36).
            02  VISREQ-MODE              PIC S9(9)   BINARY.
            02  VISREQ-RECORDNAME        PIC X(128).
            02  VISREQ-VISITPTREQ.
      *       ������ʬ
              03  VISREQ-REQUEST-NUMBER    PIC X(02).
      *       ��������
              03  VISREQ-VISIT-DATE        PIC X(10).
      *       ���Ų�
              03  VISREQ-DEP-CODE          PIC X(02).
      *