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
      *  �ץ�����ཤ������
      * Maj/Min/Rev ������  ����        ����
      *  01.00.01   ����    2008/05/26  Ģɼ�ͼ����ѹ���ȼ������
      *  01.00.02   ����    2008/06/05  ���ŵ��إ����ɥХ��ȿ�����
      *  01.00.03   ����    2010/10/01  ��������������б�
      *  01.00.04   ���    2018/11/12  �������򥳥��Ⱥ��
      ********************************************************************
       01  SKT1920.
      *    NO
           03  SKT1920-NO                  PIC  X(04).
      *    ����ǯ��
           03  SKT1920-SRYYM               PIC  X(16).
      *    ����ǯ����
           03  SKT1920-SEIYMD              PIC  X(22).
      *    ͹���ֹ�
      *     03  SKT1920-POST                PIC  X(18).
      *    ����
           03  SKT1920-ADRS                PIC  X(100).
      *    ���ŵ���̾
           03  SKT1920-HOSPNAME            PIC  X(100).
      *    ���߼�̾
           03  SKT1920-KAISETUNAME         PIC  X(40).
      *    �����ֹ�
           03  SKT1920-TEL                 PIC  X(15).
      *    �ݸ����ֹ�
           03  SKT1920-HKNJANAME           PIC  X(16).
      *    �ݸ����ֹ�
           03  SKT1920-HKNJANUM            PIC  X(12).
      *    ���ŵ��إ�����
           03  SKT1920-HOSPCD              PIC  X(20).
      *    ���ճ��
           03  SKT1920-KYURATE-TBL  OCCURS  3.
               05  SKT1920-KYURATE         PIC  X(02).
      *    ����
           03  SKT1920-ZAI1                PIC  X(02).
           03  SKT1920-ZAI2                PIC  X(02).
      *    ����������
           03  SKT1920-TOKUBETU            PIC  X(10).
      *    ����ơ��֥�
           03  SKT1920-SEIKYU-TBL  OCCURS  9.
      *    �����ơ��֥�
               05  SKT1920-NYUGAI-TBL  OCCURS  2.
      *    ���
                   07  SKT1920-KENSU       PIC  X(05).
      *    ���ż�����
                   07  SKT1920-NISSU       PIC  X(05).
      *    ����
                   07  SKT1920-TENSU       PIC  X(09).
      *    ���ް�����ô��
                   07  SKT1920-YKZFTN      PIC  X(07).
      *    ������ô��
                   07  SKT1920-ITBFTN      PIC  X(09).
      *    �������ܡ����
               05  SKT1920-S-KENSU         PIC  X(05).
      *    �������ܡ�����
               05  SKT1920-S-NISSU         PIC  X(05).
      *    �������ܡ����
               05  SKT1920-S-KINGK         PIC  X(09).
      *    �������ܡ�ɸ����ô��
               05  SKT1920-S-FTNGK         PIC  X(09).
      *    ��������ơ��֥�
           03  SKT1920-TKOUHI-TBL  OCCURS  3.
      *    �����ơ��֥�
               05  SKT1920-TNYUGAI-TBL  OCCURS  2.
      *    ���
                   07  SKT1920-TKENSU      PIC  X(05).
      *    ���ż�����
                   07  SKT1920-TNISSU      PIC  X(05).
      *    ����
                   07  SKT1920-TTENSU      PIC  X(09).
      *    ������ô��
                   07  SKT1920-TITBFTN     PIC  X(09).
      *    ����ơ��֥�
           03  SKT1920-KOUHI-TBL  OCCURS  3.
      *    ˡ���ֹ�
               05  SKT1920-HBTNUM          PIC  X(02).
      *    ����̾
               05  SKT1920-KOHNAME         PIC  X(20).
      *    �����ơ��֥�
               05  SKT1920-KNYUGAI-TBL  OCCURS  2.
      *    ���
                   07  SKT1920-KKENSU      PIC  X(05).
      *    ���ż�����
                   07  SKT1920-KNISSU      PIC  X(05).
      *    ����
                   07  SKT1920-KTENSU      PIC  X(09).
      *    ������ô��
                   07  SKT1920-KITBFTN     PIC  X(09).
      *    �������ܡ����
               05  SKT1920-S-KKENSU        PIC  X(05).
      *    �������ܡ�����
               05  SKT1920-S-KNISSU        PIC  X(05).
      *    �������ܡ����
               05  SKT1920-S-KKINGK        PIC  X(09).
      *    �������ܡ�ɸ����ô��
               05  SKT1920-S-KFTNGK        PIC  X(09).
