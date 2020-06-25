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
      ************************************************************
      *    �����ݸ������ѥ�᥿��COPY (CPORCSSKJHKN.INC)
      *                        ���ѥץ������ (ORCSSKJHKN)
      *    �������
      *      ���ŵ����ֹ�
      *      ���ԣɣ�
      *      ����ǯ��
      *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  ORCSSKJHKN-AREA.
      *
      *���ŵ����ֹ�
           03  LNK-SKJHKN-HOSPNUM             PIC 9(2).
      *���ԣɣ�
           03  LNK-SKJHKN-PTID                PIC  9(10).
      *����ǯ��
           03  LNK-SKJHKN-SANTEIYM            PIC  X(06).
      *�ֵѹ���
           03  LNK-SKJHKN-HENKYAKU-AREA.
      *������ʬ
             05  LNK-SKJHKN-TBL.
               07  LNK-SKJHKN-DAY             OCCURS  31.
      *          �ݸ��ȹ礻�ֹ�
      *        ��ź����������ʬ��ź������Ʊ��������ʬ����������̵����
                 09  LNK-SKJHKN-HKNCOMBINUM   PIC X(04)  OCCURS  2.
      *          �ݸ�����
      *        �ʣ������ݡ��������ݡ�����������𡡣���ϫ�ҡ���������
      *          �����������������񡡣�����������������̵����
                 09  LNK-SKJHKN-HKNSYU        PIC X(01)  OCCURS  2.
      *
      *���������ɡʣ�������ǯ��顼���������Ը������顼
      *            ���������ʤ������������ƥ�ͽ�����Ϥʤ�
      *            ���������ƥ�ͽ���ʣ���ϥ��顼��
           03  LNK-SKJHKN-RC                PIC S9(9) BINARY.
      *