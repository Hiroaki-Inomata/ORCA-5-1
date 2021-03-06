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
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    ����      2010/10/13   ��������������б�
      *  01.00.02    ����      2018/10/11   �������򥳥��Ⱥ��
      ********************************************************************
       01  SKT2110.
      *    NO
           03  SKT2110-NO                  PIC  X(04).
      *    ����ǯ��
           03  SKT2110-SRYYM               PIC  X(16).
      *    ����ǯ����
           03  SKT2110-SEIYMD              PIC  X(22).
      *    ����
           03  SKT2110-ADRS                PIC  X(100).
      *    ���ŵ���̾
           03  SKT2110-HOSPNAME            PIC  X(100).
      *    ���߼�̾
           03  SKT2110-KAISETUNAME         PIC  X(60).
      *    �����ֹ�
           03  SKT2110-TEL                 PIC  X(15).
      *    �ݸ����ֹ�
           03  SKT2110-HKNJANUM            PIC  X(12).
      *    ���ŵ��إ�����
           03  SKT2110-HOSPCD              PIC  X(14).
      *    ����
           03  SKT2110-ZAI1                PIC  X(02).
           03  SKT2110-ZAI2                PIC  X(02).
      *    ����ɽ��ʬ
           03  SKT2110-TENHYOKBN           PIC  X(04).
      *    ���ճ��
           03  SKT2110-KYURATE-TBL  OCCURS  4.
               05  SKT2110-KYURATE         PIC  X(02).
      *    ���Ų�
           03  SKT2110-SRYKA               PIC  X(04).
      *    �ݸ�����
           03  SKT2110-HKNSYUBETU-TBL  OCCURS  2.
               05  SKT2110-HKNSYUBETU      PIC  X(02).
      *    ����������
           03  SKT2110-TOKUBETU            PIC  X(10).
      *    ����ơ��֥�
           03  SKT2110-SEIKYU-TBL  OCCURS  5.
      *    �����ơ��֥�
               05  SKT2110-NYUGAI-TBL  OCCURS  2.
      *    ���
                   07  SKT2110-KENSU       PIC  X(05).
      *    ���ż�����
                   07  SKT2110-NISSU       PIC  X(05).
      *    ����
                   07  SKT2110-TENSU       PIC  X(09).
      *    ������ô��
                   07  SKT2110-ITBFTN      PIC  X(09).
      *    �������ܡ����
               05  SKT2110-S-KENSU         PIC  X(05).
      *    �������ܡ�����
               05  SKT2110-S-NISSU         PIC  X(05).
      *    �������ܡ����
               05  SKT2110-S-KINGAKU       PIC  X(09).
      *    �������ܡ�ɸ����ô��
               05  SKT2110-S-FTNGAKU       PIC  X(09).
      *    ����ơ��֥�
           03  SKT2110-KOH-TBL  OCCURS  3.
      *    ����̾
               05  SKT2110-KOHNAME         PIC  X(08).
      *    �����ơ��֥�
               05  SKT2110-K-NYUGAI-TBL  OCCURS  2.
      *    ���
                   07  SKT2110-KKENSU      PIC  X(05).
      *    ���ż�����
                   07  SKT2110-KNISSU      PIC  X(05).
      *    ����
                   07  SKT2110-KTENSU      PIC  X(09).
      *    ������ô��
                   07  SKT2110-KITBFTN     PIC  X(09).
      *    �������ܡ����
               05  SKT2110-S-KKENSU        PIC  X(05).
      *    �������ܡ�����
               05  SKT2110-S-KNISSU        PIC  X(05).
      *    �������ܡ����
               05  SKT2110-S-KKINGAKU      PIC  X(09).
      *    �������ܡ�ɸ����ô��
               05  SKT2110-S-KFTNGAKU      PIC  X(09).
      *    Ĺ�������
           03  SKT2110-TYOKI-TBL  OCCURS  2.
               05  SKT2110-TKENSU          PIC  X(02).
      *    Ĺ�衧���
           03  SKT2110-TYOKIT-TBL  OCCURS  2.
               05  SKT2110-TTKENSU         PIC  X(02).
