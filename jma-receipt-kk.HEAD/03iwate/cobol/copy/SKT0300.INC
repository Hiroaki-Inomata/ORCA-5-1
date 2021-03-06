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
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01     ����     2006/08/29   ������̾(����)�ɲ�63��64
      *  01.00.02     ����     2010/03/11   ������̾(����)�ɲ�8��9
      *  01.00.03     ����     2011/06/15   ��������������б�
      *  01.00.04     ����     2012/04/19   Ģɼ�ͼ����ѹ�
      *  01.00.05     ���     2018/10/10   �������򥳥��Ⱥ��
      ************************************************************
       01  SKT0300.
      *    ����ǯ��
           03  SKT0300-SRYYM                PIC  X(16).
      *    ���ŵ��ؽ���
           03  SKT0300-ADRS                 PIC  X(100).
      *    ���ŵ���̾
           03  SKT0300-HOSPNAME             PIC  X(100).
      *    ���߼�̾
           03  SKT0300-KAISETUNAME          PIC  X(40).
      *    �����ֹ�
           03  SKT0300-TEL                  PIC  X(15).
      *    ���ŵ��إ�����
           03  SKT0300-HOSPCDN              PIC  X(40).
      *    ���ʰ����ݸ��ԡ�
           03  SKT0300-KYURATE              PIC  X(02).
      *    ����������
           03  SKT0300-TOKUBETU             PIC  X(10).
      *    ����ơ��֥�
           03  SKT0300-SEIKYU-TBL     OCCURS  8.
      *    �����ơ��֥�
               05  SKT0300-NYUGAI-TBL     OCCURS  2.
      *    ���⸩���ơ��֥�
                   07  SKT0300-PREF-TBL     OCCURS  2.
      *    ���
                       09  SKT0300-KENSU    PIC  X(05).
      *    ������
                       09  SKT0300-NISSU    PIC  X(05).
      *    ����
                       09  SKT0300-TENSU    PIC  X(09).
      *    �����ǥơ��֥�
           03  SKT0300-ZAI-TBL     OCCURS  4.
      *    ���⸩���ơ��֥�
               05  SKT0300-ZPREF-TBL    OCCURS  2.
      *    ���
                   07  SKT0300-ZKENSU       PIC  X(05).
      *    ������
                   07  SKT0300-ZNISSU       PIC  X(05).
      *    ����
                   07  SKT0300-ZTENSU       PIC  X(09).
      *    ��ץơ��֥�
           03  SKT0300-GOKEI-TBL     OCCURS  3.
      *    ���⸩���ơ��֥�
               05  SKT0300-GPREF-TBL    OCCURS  2.
      *    ���
                   07  SKT0300-GKENSU       PIC  X(05).
      *    ������
                   07  SKT0300-GNISSU       PIC  X(05).
      *    ����
                   07  SKT0300-GTENSU       PIC  X(09).
      *    �����ݸ��Կ�
           03  SKT0300-NHKNJA-TBL  OCCURS  40.
               05  SKT0300-NHKNJA           PIC  X(02).
      *    ��̾
           03  SKT0300-KENMEI-TBL  OCCURS  5.
               05  SKT0300-KENMEI           PIC  X(06).
      *    �����ݸ��Կ�
           03  SKT0300-GHKNJA-TBL  OCCURS  9.
               05  SKT0300-GHKNJA           PIC  X(02).
           03  SKT0300-GHKNJA-TBL2  OCCURS  5.
               05  SKT0300-GHKNJA2           PIC  X(02).
      *    �ݸ��Թ�׿�
           03  SKT0300-GOKEIHKNJA-TBL  OCCURS  2.
               05  SKT0300-GOKEIHKNJA       PIC  X(03).
