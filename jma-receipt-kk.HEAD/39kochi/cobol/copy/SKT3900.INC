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
      *  01.00.02    ���    2019/01/07  �������򥳥��Ⱥ��
      ********************************************************************
       01  SKT3900.
      *����ǯ��
           03  SKT3900-SRYYM                   PIC  X(16).
      *���Ŷ�ʬ
           03  SKT3900-SRYKBN1                 PIC  X(02).
           03  SKT3900-SRYKBN2                 PIC  X(02).
      *���ڡ�����
           03  SKT3900-GPAGE                   PIC  X(02).
      *�ڡ�����
           03  SKT3900-PAGE                    PIC  X(02).
      *����ǯ����
           03  SKT3900-SEIYMD                  PIC  X(22).
      *���ŵ��إ�����
           03  SKT3900-HOSPCD                  PIC  X(20).
      *���ŵ��ؽ����
           03  SKT3900-ADRS                    PIC  X(100).
      *���ŵ���̾��
           03  SKT3900-HOSPNAME                PIC  X(100).
      *���߼�̾
           03  SKT3900-KAISETUNAME             PIC  X(40).
      *�����ֹ�
           03  SKT3900-TEL                     PIC  X(15).
      *����������
           03  SKT3900-TOKUBETU                PIC X(10).
      *����ơ��֥�
           03  SKT3900-TBL   OCCURS  12.
               05  SKT3900-SEIDOKBN-TBL  OCCURS  4.
                   07  SKT3900-SEIDOKBN        PIC  X(02).
      *�ݸ����ֹ�
               05  SKT3900-HKNJANUM            PIC  X(12).
      *�ܿͲ�²��ʬ
      *         05  SKT3900-HONKZKKBN           PIC  X(02).
      *����������
               05  SKT3900-NYUGAIKBN-TBL  OCCURS  2.
      *���
                   07  SKT3900-KENSU           PIC  X(05).
      *����
                   07  SKT3900-TENSU           PIC  X(09).
      *Ϸ�Ͱ�����ô�����Ϸ򹯿Ǻ������
                   07  SKT3900-RJNFTN          PIC  X(09).
      *�����ơ��֥�
                   07  SKT3900-GENBK-TBL.
                       09  SKT3900-GENMARU     PIC  X(02).
                       09  SKT3900-GENBK1      PIC  X(12).
                       09  SKT3900-GENBK2      PIC  X(03).
      *���������񡧷��
               05  SKT3900-SKJ-KENSU           PIC  X(05).
      *���������񡧶��
               05  SKT3900-SKJ-KINGAKU         PIC  X(09).
      *����������ɸ����ô��
               05  SKT3900-SKJ-FTNGAKU         PIC  X(09).
      *������׷��
      *     03  SKT3900-N-GKENSU                PIC  X(05).
      *�����׷��
      *     03  SKT3900-G-GKENSU                PIC  X(05).
      *���
           03  SKT3900-GOKEI-TBL    OCCURS  2.
      *����������ơ��֥�
               05  SKT3900-G-NYUGAI-TBL  OCCURS  2.
      *���
                   07  SKT3900-GKENSU          PIC  X(05).
      *���
                   07  SKT3900-GTENSU          PIC  X(09).
