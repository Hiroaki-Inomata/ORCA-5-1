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
      *  01.00.01    ����    2011/06/15  ��������������б�
      *  01.00.02    ����    2012/04/19  Ģɼ�ͼ����ѹ�
      *  01.00.03    ���    2018/10/11  �������򥳥��Ⱥ��
      ********************************************************************
       01  SKT0320.
      *    �ڡ����ֹ�
           03  SKT0320-NO                PIC  X(04).
      *    ����������
           03  SKT0320-TOKUBETU          PIC  X(10).
      *    ����ơ��֥�
           03  SKT0320-KOH-TBL     OCCURS  9.
      *    �����ơ��֥�
               05  SKT0320-NYUGAI-TBL     OCCURS  2.
      *    ���
                   07  SKT0320-KENSU     PIC  X(05).
      *    ������
                   07  SKT0320-NISSU     PIC  X(05).
      *    ����
                   07  SKT0320-TENSU     PIC  X(09).
      *    ������ô��
                   07  SKT0320-ITBFTN    PIC  X(09).
      *    �������ܡ����
               05  SKT0320-S-KENSU       PIC  X(05).
      *    �������ܡ�����
               05  SKT0320-S-NISSU       PIC  X(05).
      *    �������ܡ����
               05  SKT0320-S-KINGAKU     PIC  X(09).
      *    �������ܡ�ɸ����ô��
               05  SKT0320-S-FTNGAKU     PIC  X(09).
      *    ����ơ��֥�
           03  SKT0320-KOHNUM-TBL     OCCURS  3.
      *    ����̾
               05  SKT0320-KOHNAME       PIC  X(10).
      *    �����ֹ�
               05  SKT0320-KOHNUM        PIC  X(02).
