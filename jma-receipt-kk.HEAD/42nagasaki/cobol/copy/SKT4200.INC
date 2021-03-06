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
      *  01.00.01    ����    2011/01/12  ��������������б�
      *  01.00.02    ����    2013/02/14  �������������������ܤ��ɲ�
      *  01.00.03    ����    2019/02/07  �������򥳥��Ⱥ��
      *  01.00.04    ���    2019/03/04  Ģɼ�ͼ����ѹ�
      *  01.00.05    ���    2019/05/22  �����б�
      ********************************************************************
       01  SKT4200.
      *����ǯ��
           03  SKT4200-SRYY                PIC  X(04).
           03  SKT4200-SRYM                PIC  X(04).
      *͹���ֹ�
           03  SKT4200-POST                PIC  X(08).
      *���ŵ��ؽ����
           03  SKT4200-ADRS                PIC  X(100).
      *���ŵ���̾��
           03  SKT4200-HOSPNAME            PIC  X(100).
      *���ŵ��س��߼�̾
           03  SKT4200-KAISETU             PIC  X(80).
      *���ŵ��إ�����
           03  SKT4200-HOSPCD              PIC  X(07).
      *�����ֹ�
           03  SKT4200-TEL                 PIC  X(15).
      *����������
           03  SKT4200-TOKUBETU            PIC  X(10).
      *����ơ��֥�
           03  SKT4200-SEIKYU  OCCURS 17.
      *���ܤε��ա����
               05  SKT4200-KENSU           PIC  X(05).
      *���ܤε��ա�����
               05  SKT4200-TENSU           PIC  X(09).
      *�ݸ��Կ�
           03  SKT4200-KENNAI              PIC  X(03).
           03  SKT4200-KENGAI              PIC  X(03).
      *��۷��
           03  SKT4200-KOGAKU              PIC  X(03) OCCURS 4.
      *�ݸ��Կ�
           03  SKT4200-KENNAI-KOUKI        PIC  X(03).
           03  SKT4200-KENGAI-KOUKI        PIC  X(03).
      *��۳�ǧ
           03  SKT4200-KOGAKU-KBN          PIC  X(02) OCCURS 2.
      *����ǯ��(����)
           03  SKT4200-SRYG                PIC  X(04).
