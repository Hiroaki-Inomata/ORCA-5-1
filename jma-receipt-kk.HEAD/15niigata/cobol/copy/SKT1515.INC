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
      *    ������������� �ʿ��㸩��     COPY (SKT1515.INC) 
      *
      *  ��������    ��ȼ�        ����
      *  08/04/29    ����          ��������(���������б�)
      ********************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ********************************************************************
        01  SKT1515.
      *    ����ǯ��
            03  SKT1515-SRYYM   PIC X(16).
      *    ���ŵ��ؽ���
            03  SKT1515-ADRS    PIC X(100).
      *    ���ŵ���̾��
            03  SKT1515-HOSPNAME    PIC X(100).
      *    ���߼�̾
            03  SKT1515-KAISETUNAME PIC X(40).
      *    ���ŵ���TEL
            03  SKT1515-TEL PIC X(15).
      *    �ݸ����ֹ�
            03  SKT1515-HKNJANUM    PIC X(12).
      *    ���ŵ��إ�����
            03  SKT1515-HOSPCD  PIC X(14).
      *    �ݸ���̾
            03  SKT1515-HKNJANAME   PIC X(100).
      *    ����ǯ����
            03  SKT1515-SEIYMD  PIC X(22).
      *    ����
            03  SKT1515-ZAI1    PIC X(2).
            03  SKT1515-ZAI2    PIC X(2).
      *    �ڡ����ֹ�
            03  SKT1515-NO  PIC X(4).
      *    ����ơ��֥�
            03  SKT1515-SEIKYU-TBL      OCCURS  2 TIMES.
      *    �����ơ��֥�
              05  SKT1515-NYUGAI-TBL      OCCURS  2 TIMES.
      *    ���
                07  SKT1515-KENSU   PIC X(5).
      *    ����
                07  SKT1515-NISSU   PIC X(5).
      *    ����
                07  SKT1515-TENSU   PIC X(9).
      *    ������ô��
                07  SKT1515-ITBFTN  PIC X(9).
      *    �������
              05  SKT1515-S-KENSU PIC X(5).
      *    ��������
              05  SKT1515-S-NISSU PIC X(5).
      *    ���
              05  SKT1515-S-KINGK PIC X(9).
      *    ɸ����ô��
              05  SKT1515-S-FTNGK PIC X(9).
      *    ��������ơ��֥�
            03  SKT1515-FTN-TBL         OCCURS  2 TIMES.
      *    ���
              05  SKT1515-FKENSU  PIC X(5).
      *    ����
              05  SKT1515-FTENSU  PIC X(9).
      *    �������
              05  SKT1515-S-FKENSU    PIC X(5).
      *    ���
              05  SKT1515-S-FKINGK    PIC X(9).
      *    ɸ����ô��
              05  SKT1515-S-FFTNGK    PIC X(9).
      *    ��������ơ��֥�
            03  SKT1515-KOUHI-TBL       OCCURS  5 TIMES.
      *    ˡ���ֹ�
              05  SKT1515-HBTNUM  PIC X(2).
      *    ����̾
              05  SKT1515-KOHNAME PIC X(10).
      *    ���������ơ��֥�
              05  SKT1515-KNYUGAI-TBL     OCCURS  2 TIMES.
      *    ������
                07  SKT1515-KKENSU  PIC X(5).
      *    ��������
                07  SKT1515-KNISSU  PIC X(5).
      *    ��������
                07  SKT1515-KTENSU  PIC X(9).
      *    ���������ô��
                07  SKT1515-KITBFTN PIC X(9).
      *    ���񿩻����
              05  SKT1515-S-KKENSU    PIC X(5).
      *    ���񿩻�����
              05  SKT1515-S-KNISSU    PIC X(5).
      *    ���񿩻����
              05  SKT1515-S-KKINGK    PIC X(9).
      *    ����ɸ����ô��
              05  SKT1515-S-KFTNGK    PIC X(9).
