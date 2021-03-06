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
      *  01.00.01    ����         10/08/19  ������ֹ�η������
      *  01.00.02    ����         14/03/05  ���ŵ��إ����ɡ�������ô����ɲ�
      *  01.00.03    ����         14/04/11  ����ǯ��η�����á������Ⱥ��
      ********************************************************************
        01  SC07.
      *    ��Į¼������
           03  SC07-CITYCDUP             PIC X(03).
      *    ��Į¼̾
           03  SC07-CITYNAME             PIC X(26).
      *    ����ǯ����
           03  SC07-SEIYMD               PIC X(22).
      *    ���ŵ��إ�����
           03  SC07-HOSPCD               PIC X(20).
      *    ͹���ֹ�
           03  SC07-POST                 PIC X(10).
      *    �����
           03  SC07-ADRS                 PIC X(100).
      *    ���ŵ���̾��
           03  SC07-HOSPNAME             PIC X(100).
      *    ���߼Ի�̾
           03  SC07-KAISETU              PIC X(40).
      *    �����ֹ�
           03  SC07-TEL                  PIC X(15).
      *    �ڡ���
           03  SC07-PAGE                 PIC X(03).
      *    ����ơ��֥�
           03  SC07-SEIKYU-TBL  OCCURS  15.
      *    ��Į¼������
               05  SC07-CITYCD           PIC X(03).
      *    ��������
               05  SC07-KOHKBN           PIC X(01).
      *    ����Ի�ʾ��ֹ�
               05  SC07-JKYSNUM          PIC X(20).
      *    ��̾
               05  SC07-NAME             PIC X(26).
      *    ����
               05  SC07-SEX              PIC X(01).
      *    ��ǯ����
               05  SC07-BIRTHDAY         PIC X(07).
      *    ����ǯ��
               05  SC07-SRYYM            PIC X(05).
      *    ��ô���
               05  SC07-RATE             PIC X(02).
      *    ���Ųʥ�����
               05  SC07-SRYKA            PIC X(02).
      *    ����
               05  SC07-NYUGAIKBN        PIC X(01).
      *    ��������
               05  SC07-NISSU            PIC X(04).
      *    �ݸ���������
               05  SC07-TENSU            PIC X(07).
      *    ������ô��
               05  SC07-ITBFTN           PIC X(09).
      *    ����Ĺ��ʬ
               05  SC07-TYOKI            PIC X(02).
      *    ������������
               05  SC07-ETENSU           PIC X(07).
      *    ����Ĺ������ô��
               05  SC07-EFTNMONEY        PIC X(06).
      *    �������ݸ�����ʬ
               05  SC07-SHOKUJIRYOYOHI   PIC X(09).
      *    ������ɸ����ôʬ.
               05  SC07-SHOKUJIFTN       PIC X(09).
      *    �����ݸ�������ʬ
               05  SC07-ESHOKUJIRYOYOHI  PIC X(09).
      *    �����ݸ���ɸ����ôʬ
               05  SC07-ESHOKUJIFTN      PIC X(09).
      *    ��������ȯ�Զ�ʬ
               05  SC07-SYOHO            PIC X(01).
      *    ����
               05  SC07-BIKO             PIC X(20).
