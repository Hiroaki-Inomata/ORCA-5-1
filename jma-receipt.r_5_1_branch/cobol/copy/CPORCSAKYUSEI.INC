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
      *    ���������������Ŵ�����Ƚ��ѥ�᥿������COPY (CPORCSAKYUSEI.INC)
      *                                ���ѥץ������ (ORCSAKYUSEI)
      *    �������
      *      ���ŵ��أɣ�
      *      ���ԣɣ�
      *      ����ǯ��
      *      �ݸ���ʬ
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  ORCSAKYUSEI-AREA.
      *
      *���ŵ��أɣ�
           03  LNK-AKYUSEI-HOSPNUM                PIC  9(02).
      *���ԣɣ�
           03  LNK-AKYUSEI-PTID                   PIC  9(10).
      *���������ֹ�
           03  LNK-AKYUSEI-RRKNUM                 PIC  9(03).
      *
      *����������
      *     ����ʿ������ǯ��������˰��������������Ŵ������λ���̵��
      *     ����ʿ������ǯ��������˰��������������Ŵ�����������
      *     ����ʿ������ǯ��������˰��������������Ŵ�����������
      *     ��������¾���顼
           03  LNK-AKYUSEI-RC                     PIC S9(9) BINARY.
      *
