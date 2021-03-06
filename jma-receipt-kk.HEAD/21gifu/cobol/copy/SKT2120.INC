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
       01  SKT2120.
      *    NO
           03  SKT2120-NO                  PIC  X(04).
      *    ����ǯ��
           03  SKT2120-SRYYM               PIC  X(16).
      *    ����ǯ����
           03  SKT2120-SEIYMD              PIC  X(22).
      *    ����
           03  SKT2120-ADRS                PIC  X(100).
      *    ���ŵ���̾
           03  SKT2120-HOSPNAME            PIC  X(100).
      *    ���߼�̾
           03  SKT2120-KAISETUNAME         PIC  X(60).
      *    �����ֹ�
           03  SKT2120-TEL                 PIC  X(15).
      *    �ݸ����ֹ�
           03  SKT2120-HKNJANUM            PIC  X(12).
      *    ���ŵ��إ�����
           03  SKT2120-HOSPCD              PIC  X(14).
      *    ����
           03  SKT2120-ZAI1                PIC  X(02).
           03  SKT2120-ZAI2                PIC  X(02).
      *    ����ɽ��ʬ
           03  SKT2120-TENHYOKBN           PIC  X(04).
      *    ���ճ��
           03  SKT2120-KYURATE-TBL  OCCURS  4.
               05  SKT2120-KYURATE         PIC  X(02).
      *    ���Ų�
           03  SKT2120-SRYKA               PIC  X(04).
      *    ����������
           03  SKT2120-TOKUBETU            PIC  X(10).
      *    ����ơ��֥�
           03  SKT2120-SEIKYU-TBL  OCCURS  2.
      *    �����ơ��֥�
               05  SKT2120-NYUGAI-TBL  OCCURS  2.
      *    ���
                   07  SKT2120-KENSU       PIC  X(05).
      *    ���ż�����
                   07  SKT2120-NISSU       PIC  X(05).
      *    ����
                   07  SKT2120-TENSU       PIC  X(09).
      *    ������ô��
                   07  SKT2120-ITBFTN      PIC  X(09).
      *    �������ܡ����
               05  SKT2120-S-KENSU         PIC  X(05).
      *    �������ܡ�����
               05  SKT2120-S-NISSU         PIC  X(05).
      *    �������ܡ����
               05  SKT2120-S-KINGAKU       PIC  X(09).
      *    �������ܡ�ɸ����ô��
               05  SKT2120-S-FTNGAKU       PIC  X(09).
      *    ����ơ��֥�
           03  SKT2120-KOH-TBL  OCCURS  3.
      *    ����̾
               05  SKT2120-KOHNAME         PIC  X(08).
      *    �����ơ��֥�
               05  SKT2120-K-NYUGAI-TBL  OCCURS  2.
      *    ���
                   07  SKT2120-KKENSU      PIC  X(05).
      *    ���ż�����
                   07  SKT2120-KNISSU      PIC  X(05).
      *    ����
                   07  SKT2120-KTENSU      PIC  X(09).
      *    ������ô��
                   07  SKT2120-KITBFTN     PIC  X(09).
      *    �������ܡ����
               05  SKT2120-S-KKENSU        PIC  X(05).
      *    �������ܡ�����
               05  SKT2120-S-KNISSU        PIC  X(05).
      *    �������ܡ����
               05  SKT2120-S-KKINGAKU      PIC  X(09).
      *    �������ܡ�ɸ����ô��
               05  SKT2120-S-KFTNGAKU      PIC  X(09).
      *    Ĺ�������
           03  SKT2120-TYOKI-TBL  OCCURS  2.
               05  SKT2120-TKENSU          PIC  X(02).
      *    Ĺ�衧���
           03  SKT2120-TYOKIT-TBL  OCCURS  2.
               05  SKT2120-TTKENSU         PIC  X(02).
