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
      *----------------------------------------------------------*
      *    ����ɽ�ѥ�᥿��     COPY (ORCBD008 �ǻ��ѡ�
      *----------------------------------------------------------*
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  ORCBD008AREA.
      *    ������ʬ (1:��ɼȯ����/ 2:����ǯ������
           03  ORCBD008-SYORIKBN           PIC X(01).
      *    ���� (1:��ɼȯ����/ 2:����ǯ������
           03  ORCBD008-SYORIYMD           PIC X(08).
      *    ������ʬ (1:����/ 2:�����
           03  ORCBD008-NYUGAIKBN          PIC X(01).
      *    �¤ӽ�
           03  ORCBD008-SORTKBN            PIC X(01).