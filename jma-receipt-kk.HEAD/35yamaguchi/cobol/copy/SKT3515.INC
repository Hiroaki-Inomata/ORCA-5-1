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
      *  �������������ʻ�������     COPY (SKT3515.INC)
      *
      *  ��������    ��ȼ�        ����
      *  08/04/18    ��ƣ          ��������(���������б�)
      ********************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ********************************************************************
       01  SKT3515.
      *    ����ǯ��
           03  SKT3515-SRYYM                    PIC X(16).
      *    ����ǯ����
           03  SKT3515-SEIYMD                   PIC X(22).
      *    ���ŵ��إ�����
           03  SKT3515-HOSPCD                   PIC X(20).
      *    �ݸ����ֹ�
           03  SKT3515-HKNJANUM                 PIC X(04).
      *    �ݸ���̾
           03  SKT3515-HKNJANAME                PIC X(24).
      *    ���ŵ��ؽ���
           03  SKT3515-ADRS                     PIC X(100).
      *    ���ŵ���̾
           03  SKT3515-HOSPNAME                 PIC X(100).
      *    �����ֹ�
           03  SKT3515-TEL                      PIC X(20).
      *    ���߼�̾
           03  SKT3515-KAISETUNAME              PIC X(40).
      *    ���Ųʥ�����
           03  SKT3515-SRYKACD                  PIC X(02).
      *    ���Ų�̾
           03  SKT3515-SRYKANAME                PIC X(20).
      *    ������
           03  SKT3515-ZAI1                     PIC X(02).
           03  SKT3515-ZAI2                     PIC X(02).
      *    �ڡ����ֹ�
           03  SKT3515-NO                       PIC X(04).
      *    ����ơ��֥�
           03  SKT3515-SEIKYU          OCCURS  2.
      *    �����ơ��֥�
               05  SKT3515-NYUGAI-TBL      OCCURS  2.
      *    ���
                   07  SKT3515-KENSU            PIC X(05).
      *    ����
                   07  SKT3515-NISSU            PIC X(05).
      *    ����
                   07  SKT3515-TENSU            PIC X(09).
      *    ������ô��
                   07  SKT3515-ITBFTN           PIC X(09).
      *    �������ܡ����
               05  SKT3515-SKENSU               PIC X(05).
      *    �������ܡ�����
               05  SKT3515-SNISSU               PIC X(05).
      *    �������ܡ����
               05  SKT3515-SKINGK               PIC X(09).
      *    �������ܡ�ɸ����ô��
               05  SKT3515-SFTNGK               PIC X(09).
      *    ��������ơ��֥�
           03  SKT3515-K-SEIKYU        OCCURS  5.
      *    ����̾
               05  SKT3515-K-KOHNAME            PIC X(10).
      *    ���������ơ��֥�
               05  SKT3515-K-NYUGAI-TBL    OCCURS  2.
      *    ���񡧷��
                   07  SKT3515-K-KENSU          PIC X(05).
      *    ��������
                   07  SKT3515-K-NISSU          PIC X(05).
      *    ��������
                   07  SKT3515-K-TENSU          PIC X(09).
      *    ���񡧰�����ô��
                   07  SKT3515-K-ITBFTN         PIC X(09).
      *    ���񡧿������ܡ����
               05  SKT3515-K-SKENSU             PIC X(05).
      *    ���񡧿������ܡ�����
               05  SKT3515-K-SNISSU             PIC X(05).
      *    ���񡧿������ܡ����
               05  SKT3515-K-SKINGK             PIC X(09).
      *    ���񡧿������ܡ�ɸ����ô��
               05  SKT3515-K-SFTNGK             PIC X(09).
