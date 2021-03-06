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
      *  01.00.01    ����      2010/12/09   ��������������б�
      *  01.00.02    ���      2019/01/10   �������򥳥��Ⱥ��
      ********************************************************************
       01  SKT3220.
      *    ����ǯ��
           03  SKT3220-SRYYM               PIC  X(16).
      *    ����ǯ����
           03  SKT3220-SEIYMD              PIC  X(22).
      *    ����
           03  SKT3220-ADRS                PIC  X(100).
      *    ���ŵ���̾
           03  SKT3220-HOSPNAME            PIC  X(100).
      *    ���߼�̾
           03  SKT3220-KAISETUNAME         PIC  X(40).
      *    �����ֹ�
           03  SKT3220-TEL                 PIC  X(15).
      *    �ݸ����ֹ�
           03  SKT3220-HKNJANUM            PIC  X(12).
      *    �ݸ���̾
           03  SKT3220-HKNJANAME           PIC  X(40).
      *    ���ŵ��إ�����
           03  SKT3220-HOSPCD              PIC  X(14).
      *    ��������
           03  SKT3220-ZAIMARU             PIC  X(02).
      *    ����ʸ��
           03  SKT3220-ZAIMOJI             PIC  X(02).
      *    ����������
           03  SKT3220-TOKUBETU            PIC  X(10).
      *    NO
           03  SKT3220-NO                  PIC  X(04).
      *    ����ơ��֥�
           03  SKT3220-SEIKYU-TBL  OCCURS  2.
      *    �����ơ��֥�
               05  SKT3220-NYUGAI-TBL  OCCURS  2.
      *    ���
                   07  SKT3220-KENSU       PIC  X(05).
      *    ���ż�����
                   07  SKT3220-NISSU       PIC  X(05).
      *    ����
                   07  SKT3220-TENSU       PIC  X(09).
      *    ������ô��
                   07  SKT3220-ITBFTN      PIC  X(09).
      *    Ĺ�����ܷ��
                   07  SKT3220-TYOUKEN     PIC  X(03).
      *    �������ܷ��
                   07  SKT3220-GENKEN      PIC  X(03).
      *    �������ܡ����
               05  SKT3220-S-KENSU         PIC  X(05).
      *    �������ܡ�����
               05  SKT3220-S-NISSU         PIC  X(05).
      *    �������ܡ����
               05  SKT3220-S-KINGAKU       PIC  X(09).
      *    �������ܡ�ɸ����ô��
               05  SKT3220-S-FTNGAKU       PIC  X(09).
      *    ����ơ��֥�
           03  SKT3220-KOH-TBL  OCCURS  17.
      *    ���񥳡���
               05  SKT3220-KOHCD           PIC  X(02).
      *    ����̾
               05  SKT3220-KOHNAME         PIC  X(12).
      *    ���
               05  SKT3220-KKENSU          PIC  X(05).
      *    ����
               05  SKT3220-KTENSU          PIC  X(09).
      *    ������ô��
               05  SKT3220-KKNJFTN         PIC  X(07).
      *    �������ܡ����
               05  SKT3220-S-KKENSU        PIC  X(05).
      *    �������ܡ����
               05  SKT3220-S-KKINGAKU      PIC  X(07).
      *    �������ܡ�ɸ����ô��
               05  SKT3220-S-KFTNGAKU      PIC  X(07).
