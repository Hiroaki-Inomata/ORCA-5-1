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
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������  ����        ����
      *  01.00.01    ����    2010/08/03  ��������������б�
      *  01.00.02    ���    2016/10/04  Ģɼ�ͼ����ѹ�
      *  01.00.03    ���    2018/11/02  �������򥳥��Ⱥ��
      *  01.00.04    ���    2019/09/02  ���߼Ի�̾�ΰ���ʸ����������
      ********************************************************************
        01  SKT1300.
      *    ����ǯ��
            02  SKT1300-SEIYM   PIC X(16).
      *    ����
            02  SKT1300-ADRS    PIC X(100).
      *    ���ŵ���̾
            02  SKT1300-HOSPNAME    PIC X(100).
      *    ���߼�̾
            02  SKT1300-KAISETUNAME PIC X(40).
      *    �����ֹ�
            02  SKT1300-TEL PIC X(15).
            02  SKT1300-HOSPCD  PIC X(20).
            02  SKT1300-ZIP-TBL.
              03  SKT1300-ZIP1    PIC X(6).
              03  SKT1300-ZIP2    PIC X(2).
              03  SKT1300-ZIP3    PIC X(2).
              03  SKT1300-ZIP4    PIC X(2).
              03  SKT1300-ZIP5    PIC X(2).
      *    ����
            02  SKT1300-ZAI1    PIC X(2).
            02  SKT1300-ZAI2    PIC X(2).
      *    ����������
            02  SKT1300-TOKUBETU  PIC X(10).
      *    ���ץơ��֥�
            02  SKT1300-SYUKEI-TBL      OCCURS  2 TIMES.
              03  SKT1300-HKN-TBL         OCCURS  4 TIMES.
                04  SKT1300-KENSU   PIC X(5).
                04  SKT1300-TENSU   PIC X(14).
                04  SKT1300-SKENSU  PIC X(5).
                04  SKT1300-SKINGK  PIC X(14).
                04  SKT1300-HEIYOKEN  PIC X(5).
      *    �������Խ��ץơ��֥�
            02  SKT1300-KOUKI-TRL       OCCURS  2 TIMES.
              03  SKT1300-KKENSU  PIC X(5).
              03  SKT1300-KTENSU  PIC X(14).
              03  SKT1300-KHEIYOKEN   PIC X(5).
              03  SKT1300-KSKENSU PIC X(5).
              03  SKT1300-KSKINGK PIC X(14).
      *    ���߼�̾��
            02  SKT1300-KAISETUNAME2 PIC X(60).