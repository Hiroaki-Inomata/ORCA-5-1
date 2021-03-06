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
      ********************************************************************
      *    ������������� �ʻ�������     COPY (SKT1915.INC) 
      *
      *  ��������    ��ȼ�        ����
      *  08/04/22    ��ƣ          ��������(���������б�)
      ********************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ********************************************************************
        01  SKT1915.
      *     ����ǯ��
            02  SKT1915-SRYYM   PIC X(16).
      *     ���ŵ��ؽ���
            02  SKT1915-ADRS    PIC X(100).
      *     ���ŵ��������ֹ�
            02  SKT1915-TEL PIC X(28).
      *     ���ŵ���̾��
            02  SKT1915-HOSPNAME    PIC X(100).
      *     ���߼�̾
            02  SKT1915-KAISETUNAME PIC X(40).
      *     ���ŵ��إ�����
            02  SKT1915-HOSPCD  PIC X(40).
      *     �ݸ����ֹ�
            02  SKT1915-HKNJANAME   PIC X(100).
      *     ����ǯ����
            02  SKT1915-SEIYMD  PIC X(22).
      *     ����
            02  SKT1915-ZAI1    PIC X(2).
            02  SKT1915-ZAI2    PIC X(2).
      *     �ڡ����ֹ�
            02  SKT1915-NO  PIC X(4).
      *     ����ơ��֥�
            02  SKT1915-SEIKYU-TBL      OCCURS  2 TIMES.
      *     �����ơ��֥�
              03  SKT1915-NYUGAI-TBL      OCCURS  2 TIMES.
      *     ���
                04  SKT1915-KENSU   PIC X(5).
      *     ����
                04  SKT1915-NISSU   PIC X(5).
      *     ����
                04  SKT1915-TENSU   PIC X(9).
      *     ������ô��
                04  SKT1915-ITBFTN  PIC X(9).
      *     �������
              03  SKT1915-S-KENSU PIC X(5).
      *     ��������
              03  SKT1915-S-NISSU PIC X(5).
      *     ���
              03  SKT1915-S-KINGK PIC X(9).
      *     ɸ����ô��
              03  SKT1915-S-FTNGK PIC X(9).
      *     ��������ơ��֥�
            02  SKT1915-KOUHI-TBL       OCCURS  3 TIMES.
      *     ����̾
              03  SKT1915-KOHNAME PIC X(10).
      *     ˡ���ֹ�
              03  SKT1915-HBTNUM  PIC X(2).
      *     ���������ơ��֥�
              03  SKT1915-KNYUGAI-TBL      OCCURS  2 TIMES.
      *     ������
                04  SKT1915-KKENSU  PIC X(5).
      *     ��������
                04  SKT1915-KNISSU  PIC X(5).
      *     ��������
                04  SKT1915-KTENSU  PIC X(9).
      *     ���������ô��
                04  SKT1915-KITBFTN PIC X(9).
      *     ���񿩻����
              03  SKT1915-S-KKENSU    PIC X(5).
      *     ���񿩻�����
              03  SKT1915-S-KNISSU    PIC X(5).
      *     ���񿩻����
              03  SKT1915-S-KKINGK    PIC X(9).
      *     ����ɸ����ô��
              03  SKT1915-S-KFTNGK    PIC X(9).
      *     ��������ơ��֥�
            02  SKT1915-TKOUHI-TBL      OCCURS  2 TIMES.
      *     �����ơ��֥�
              03  SKT1915-TNYUGAI-TBL      OCCURS  2 TIMES.
      *     ����������
                04  SKT1915-TKENSU PIC X(5).
      *     ������������
                04  SKT1915-TNISSU PIC X(5).
      *     ������������
                04  SKT1915-TTENSU PIC X(9).
      *     �������������ô��
                 04  SKT1915-TITBFTN   PIC X(9).
